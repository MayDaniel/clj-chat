(ns clj-chat.core
  (:use [clojure.contrib.server-socket :only [create-server]]
        [clojure.java.io :only [reader writer]]
        [clojure.string :only [lower-case capitalize join]]
        [clojure.contrib.str-utils :only [re-split]]
        [clj-store.core :only [in]]
        [clj-time.core :only [interval in-minutes in-secs now]]
        [clj-time.coerce :only [to-date]])
  (:require [clojure.contrib.str-utils2 :as str]))

(def users (ref {}))
(def rooms (ref {}))
(def help-docs (ref {}))
(def *session*)

(defmacro do-when [& clauses]
  `(do ~@(loop [clauses clauses acc []]
           (cond (not (seq clauses)) acc
                 (not (next clauses))
                 (throw (IllegalArgumentException.
                         "do-when requires an even number of forms."))
                 :else (recur (nnext clauses)
                              (conj acc (list 'when (first clauses)
                                              (second clauses))))))))

(defn not-truthy? [& xs]
  (not-every? identity xs))

(defn command-args
  ([input] (re-split #"\s+" input))
  ([input n] (take n (command-args input))))

(defn str->help [s]
  (case s "&" "&" (str "<" s ">")))

(defmulti execute #(-> (re-split #"\s+" %)
                       (first) (str/drop 1) (lower-case))
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
               (assoc m :args (join " " (map str->help (first options))))
               m)
        body (if (vector? (first options))
               (next options)
               options)]
    (dosync (alter help-docs assoc (lower-case cmd) help))
    `(defmethod execute ~cmd
       [~'input]
       (let [~'input (->> (re-split #"\s+" ~'input)
                          (drop 1) (join " "))]
         ~@body))))

(defmethod execute :default [input]
  "What?")

(defcommand "help"
  "Prints a list of possible commands, or if a command is
specified, prints the help string and argument list for it."
  (if-let [cmd-entry (->> (command-args input 1) (apply lower-case) (@help-docs))]
    (let [{:keys [help args]} cmd-entry]
      (println "Docs:" (or help "There is no help documentation for this command."))
      (println "Args:" (or args "There is no argument string for this command.")))
    (str "Commands: " (join " " (keys @help-docs)))))

(defcommand "register"
  ["username" "password"]
  (let [[username password] (command-args input 2)]
    (cond (@users username)
          "A user with that name already exists."
          (not-truthy? username password)
          "You must specify a username and password."
          (not-every? #(re-find #"^[a-zA-Z0-9_]{3,12}$" %) [username password])
          "Invalid username/password."
          :else (do (dosync (alter users assoc username {:password password}))
                    "Registration successful."))))

(defcommand "login"
  ["username" "password"]
  (let [[username password] (command-args input 2)]
    (cond (:in-as @*session*)
          "You are already logged in. Use command /logout to log in as this user."
          (not-truthy? username password)
          "You must specify a username and password."
          (not (@users username))
          "That user does not exist."
          (:logged-in? (@users username))
          "This user is already logged in."
          (= password (:password (@users username)))
          (do (dosync (alter users update-in [username] assoc
                               :logged-in? true :sign-on (now))
                      (send-off *session* assoc :in-as username))
              "Log in successful."))))

(defn print-message [room user message]
  (println (str "(" (-> (now) (str) (subs 11 19)) ")"
                "[" room "]"
                (str/repeat " " (- 12 (count user)))
                user ": " message)))

(defcommand "say"
  "Prints your message to all users in the specified room."
  ["room" "&" "message"]
  (let [[room & words] (re-split #"\s+" input)
        streams (vals (@rooms room))
        message (join " " words)
        in-as (:in-as @*session*)]
    (cond (not in-as)
          "You must be logged in to talk."
          (not streams)
          "A channel with that name does not exist, or contains no users."
          :else (doseq [stream streams]
                  (binding [*out* stream]
                    (print-message room in-as message))))))

(defcommand "join"
  "Creates or joins a room."
  ["room"]
  (if-let [in-as (:in-as @*session*)]
    (do (dosync
         (alter rooms update-in (command-args input 1) assoc in-as *out*))
        "Successfully joined the room.")
    "You must be logged in to join rooms."))

(defcommand "logout"
  (if-let [in-as (:in-as @*session*)]
    (do (dosync (alter users update-in [in-as] dissoc
                         :logged-in? :sign-on :last-input)
                (doseq [[room users] @rooms :when (contains? users in-as)]
                  (alter rooms update-in [room] dissoc in-as))
                (send-off *session* dissoc :in-as))
        "You've successfully logged out.")
  "You're not logged in."))

(defcommand "whois"
  ["user"]
  (let [[username] (command-args input 1)]
    (if-let [user (@users username)]
      (let [{:keys [sign-on last-input]} user
            pre #(apply println (str username ":") %&)]
        (pre "WHOIS")
        (do-when sign-on
                 (pre "Sign on" (subs (str (to-date sign-on)) 0 19))
                 last-input
                 (->> ["minutes" "seconds"]
                      (interleave ((juxt in-minutes in-secs)
                                   (interval last-input (now))))
                      (cons "Idle")
                      (apply pre))))
      "A user with that username was not found.")))

(defcommand "session"
  (str @*session*))

(defn last-input []
  (when-let [in-as (:in-as @*session*)]
    (dosync (commute users assoc-in [in-as :last-input] (now)))))

(defn load-commands []
  (doseq [command (-> "commands.config" in :commands)]
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
      (let [output (execute-layer input)]
        (last-input)
        (cond (string? output)
              (println output)))
      (recur (read-line)))))

(defn -main []
  #_(load-commands)
  (defonce server (create-server 3333 loop-handler)))
