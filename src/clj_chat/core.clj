(ns clj-chat.core
  (:refer-clojure :exclude [assoc-in])
  (:require [clojure.contrib.str-utils2 :as str])
  (:use [clojure.contrib.server-socket :only [create-server]]
        [clojure.java.io :only [reader writer]]
        [clojure.string :only [lower-case capitalize join]]
        [clojure.contrib.str-utils :only [re-split]]
        [clj-store.core :only [in]]
        [clj-time.core :only [interval in-minutes in-secs now]]
        [clj-time.coerce :only [to-date]]))

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

(defn assoc-in [map [& ks] & key-vals]
  (update-in
   map ks merge
   (loop [[key val & kvs] key-vals tmap (transient {})]
     (cond val (recur kvs (assoc! tmap key val))
           key (throw (IllegalArgumentException. "Odd number of keys/vals."))
           :else (persistent! tmap)))))

(defn dissoc-in [map [& ks] key & keys]
  (apply update-in map ks dissoc key keys))

(defn print-message [room user message]
  (println (str "(" (-> (now) str (subs 11 19)) ")"
                "[" room "] " user ": " message)))

(defn date->str [date]
  (subs (str (to-date date)) 0 19))

(defn not-truthy? [& xs]
  (not-every? identity xs))

(defmulti execute (fn [input]
                    (-> (re-split #"\s+" input)
                        (first) (str/drop 1) (lower-case)))
  :default :default)

(defmacro defcommand
  {:arglists '([cmd help-string? help-args? & fn-tail])}
  [cmd & options]
  (let [cmd (-> cmd str lower-case)
        help-map (if (string? (first options))
                   {:help (first options)} {})
        options (if (string? (first options))
                  (next options) options)
        args (when (vector? (not-empty (first options)))
               (first options))
        last-arg (last args)
        help-map (if args
                   (assoc help-map :args (join " " args)) help-map)
        body (if (vector? (first options))
               (next options) options)]
    (dosync (alter help-docs assoc cmd help-map))
    `(defmethod execute ~cmd
       [~'input]
       (let [[~(gensym) ~@args] (re-split #"\s+" ~'input)
             ~(or last-arg (gensym)) (or (and (coll? ~last-arg) (join " " ~last-arg))
                                         ~last-arg)]
         ~@body))))

(defmethod execute :default [input]
  "What?")

(defcommand Help
  "Prints a list of possible commands, or if a command is
specified, prints the help string and argument list for it."
  [command]
  (if-let [cmd-entry (@help-docs command)]
    (let [{:keys [help args]} cmd-entry]
      (println "Docs:" (or help "There is no help documentation for this command."))
      (println "Args:" (or args "There is no argument string for this command.")))
    (str "Commands: " (join " " (keys @help-docs)))))

(defcommand Register
  "Registers a new user."
  [username password]
  (cond (@users username)
        "A user with that name already exists."
        (not-truthy? username password)
        "You must specify a username and password."
        (not-every? #(re-find #"^[a-zA-Z0-9_]{3,12}$" %) [username password])
        "Invalid username/password."
        :else (do (dosync (alter users assoc username {:password password}))
                  "Registration successful.")))

(defcommand Login
  "Logs a user in."
  [username password]
  (cond (:in-as @*session*)
        "You are already logged in. Use command /logout to log in as this user."
        (not-truthy? username password)
        "You must specify a username and password."
        (not (@users username))
        "That user does not exist."
        (:logged-in? (@users username))
        "This user is already logged in."
        (not= password (:password (@users username)))
        "Incorrect password."
        :else (do (dosync (alter users assoc-in [username] :logged-in? true :sign-on (now))
                          (send-off *session* assoc :in-as username))
                  "Log in successful.")))

(defcommand Say
  "Prints your message to all users in the specified room."
  [room & message]
  (let [streams (vals (@rooms room))
        in-as (:in-as @*session*)]
    (cond (not in-as)
          "You must be logged in to talk."
          (not streams)
          "A channel with that name does not exist, or contains no users."
          :else (let [p-msg #(print-message room in-as message)]
                  (doseq [stream streams]
                    (binding [*out* stream] (p-msg)))))))

(defcommand Join
  "Creates or joins a room."
  [room]
  (if-let [in-as (:in-as @*session*)]
    (do (dosync (alter rooms assoc-in [room] in-as *out*))
        "Successfully joined the room.")
    "You must be logged in to join rooms."))

(defcommand Logout
  "Logs a user out and removes them from all rooms they were in."
  (if-let [in-as (:in-as @*session*)]
    (do (dosync (alter users dissoc-in [in-as] :logged-in? :sign-on :last-input)
                (doseq [[room users] @rooms :when (contains? users in-as)]
                  (alter rooms dissoc-in [room] in-as))
                (send-off *session* dissoc :in-as))
        "You've successfully logged out.")
    "You're not logged in."))

(defcommand Whois
  "Print information about the user."
  [username]
  (if-let [user (@users username)]
    (let [{:keys [sign-on last-input]} user
          pre #(apply println (str username ":") %&)]
      (pre "WHOIS")
      (do-when
       sign-on (pre "Sign on" (date->str sign-on))
       last-input (->> ["minutes" "seconds"]
                       (interleave ((juxt in-minutes in-secs)
                                    (interval last-input (now))))
                       (cons "Idle")
                       (apply pre))))
    "A user with that username was not found."))

(defcommand Session
  "Retrieves information about your current session."
  (let [{:keys [in-as]} @*session*
        {:keys [sign-on]} (@users in-as)]
    (do-when in-as (println "Logged in as:" in-as)
             sign-on (println "Signed on:" (date->str sign-on)))))

(defn last-input []
  (when-let [in-as (:in-as @*session*)]
    (dosync (alter users assoc-in [in-as] :last-input (now)))))

(defn load-commands []
  (doseq [command (-> "commands.config" in :commands)]
    (let [prefix (str "clj-chat.plugins." command)]
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
  (load-commands)
  (defonce server (create-server 3333 loop-handler)))
