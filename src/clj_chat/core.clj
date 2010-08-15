(ns clj-chat.core
  (:use [clojure.contrib.server-socket :only [create-server]]
        [clojure.java.io :only [reader writer]]
        [clojure.string :only [lower-case capitalize]]
        [clojure.contrib.str-utils :only [re-split]]
        [clj-config.core]
        [clj-time.core :only [interval in-minutes now]]
        [clj-time.coerce :only [to-date]])
  (:require [clojure.contrib.str-utils2 :as str]))

(def users (ref {}))
(def rooms (ref {}))
(def help-docs (ref {}))
(declare *session*)

(defmacro not-and [& args]
  `(not (and ~@args)))

(defn command-args [input n]
  (->> (re-split #"\s+" input)
       (drop 1)
       (take n)))

(defn command-str [input]
  (->> (re-split #"\s+" input)
       (rest)
       (str/join " ")))

(defn strs->help [& strs]
  (str/join
   " " (for [s strs]
         (condp = s
           "&" s
           (str "<" s ">")))))

(defmulti execute #(-> (re-split #"\s+" %)
                       (first)
                       (str/drop 1)
                       (lower-case))
  :default :default)

(defmacro defcommand
  {:arglists '([cmd help-string? help-args? & fn-tail])}
  [cmd & options]
  (let [m (if (string? (first options))
            {:help (first options)}
            {})
        options (if (string? (first options))
                  (next options)
                  options)
        help (if (vector? (not-empty (first options)))
               (assoc m :args (apply strs->help (first options)))
               m)
        body (if (vector? (first options))
               (next options)
               options)]
    (dosync (alter help-docs assoc (lower-case cmd) help))
    `(defmethod execute ~cmd [~'input]
       ~@body)))

(defmethod execute :default [input]
  "What?")

(defcommand "help"
  "Prints a list of possible commands, or if a command is
specified, prints the help string and argument list for it."
  (if-let [cmd-entry (->> (command-args input 1)
                          (apply str)
                          (lower-case)
                          (@help-docs))]
    (let [{:keys [help args]} cmd-entry]
      (println "Docs:" (or help "There is no help documentation for this command."))
      (println "Args:" (or args "There is no argument string for this command.")))
    (str "Commands: " (str/join " " (keys @help-docs)))))

(defcommand "register"
  ["username" "password"]
  (let [[username password] (command-args input 2)]
    (cond (@users username)
          "A user with that name already exists."
          (not-and username password)
          "You must specify a username and password."
          (not-every? #(re-find #"^[a-zA-Z0-9_]{3,12}$" %) [username password])
          "Invalid username/password."
          :else (dosync
                 (commute users assoc username {:password password})
                 "Registration successful."))))

(defcommand "login"
  ["username" "password"]
  (let [[username password] (command-args input 2)]
    (cond (:in-as @*session*)
          "You are already logged in. Use command /logout to log in as this user."
          (not-and username password)
          "You must specify a username and password."
          (not (@users username))
          "That user does not exist."
          (:logged-in? (@users username))
          "This user is already logged in."
          (= password (:password (@users username)))
          (do (println "Log in successful.")
              (dosync
               (commute users update-in [username] merge
                        {:logged-in? true
                         :sign-on (now)}))
              {:in-as username}))))

(defcommand "say"
  "Prints your message to all users in the specified room."
  ["room" "&" "message"]
  (let [[room words] ((juxt second nnext) (re-split #"\s+" input))
        streams (vals (@rooms room))
        message (str/join " " words)]
    (cond (not (:in-as @*session*))
          "You must be logged in to talk."
          (not streams)
          "A channel with that name does not exist, or contains no users."
          :else (doseq [stream streams]
                  (binding [*out* stream]
                    (println message))))))

(defcommand "join"
  "Creates or joins a room."
  ["room"]
  (let [in-as (:in-as @*session*)
        [room] (command-args input 1)]
    (cond (not in-as)
          "You must be logged in to join rooms."
          :else (dosync
                 (commute rooms update-in [room] assoc in-as *out*)
                 "Successfully joined the room."))))

(defcommand "logout"
  (if-let [in-as (:in-as @*session*)]
    (dosync (alter users update-in [in-as]
                   dissoc :logged-in? :sign-on :last-input)
            (send *session* dissoc :in-as)
            "You've successfully logged out.")
    "You're not logged in."))

(defcommand "whois"
  (let [username (->> (re-split #"\s+" input)
                      (second))]
    (if-let [user (@users username)]
      (let [{:keys [sign-on last-input]} user
            println-pre #(apply println (str username ":") %&)]
        (println-pre "WHOIS")
        (when sign-on
          (println-pre "Sign on" (subs (str (to-date sign-on)) 0 19)))
        (when last-input
          (println-pre "Idle" (in-minutes (interval last-input (now))) "minutes")))
      "A user with that username was not found.")))

(defcommand "session"
  (str @*session*))

(defn last-input []
  (when-let [in-as (:in-as @*session*)]
    (dosync (commute users assoc-in [in-as :last-input]
                     (now)))))

(defn load-commands []
  (doseq [command (-> "commands.config" read-config :commands)]
    (let [prefix (str "clj-chat.commands." command)]
      (require (symbol prefix))
      (resolve (symbol (str prefix "/execute"))))))

(defn execute-layer [input]
  (try (execute input)
       (catch java.lang.NullPointerException _
         (execute "/logout"))))

(defn loop-handler [in out]
  (binding [*in* (reader in)
            *out* (writer out)
            *session* (agent {})]
    (loop [input (read-line)]
      (last-input)
      (let [output (execute-layer input)]
        (cond (map? output)
              (dosync (send *session* merge output))
              (string? output)
              (println output)))
      (recur (read-line)))))

(defn -main []
  (load-commands)
  (defonce server (create-server 3333 loop-handler)))
