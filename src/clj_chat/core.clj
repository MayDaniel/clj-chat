(ns clj-chat.core
  (:refer-clojure :exclude [assoc-in load])
  (:import java.io.FileNotFoundException)
  (:require [clojure.contrib.str-utils2 :as str])
  (:use [clojure.contrib.server-socket :only [create-server]]
        [clojure.java.io :only [reader writer]]
        [clojure.string :only [lower-case capitalize join]]
        [clojure.contrib.str-utils :only [re-split]]
        [clj-store.core :only [in]]
        [clj-time.core :only [interval in-minutes in-secs now]]
        [clj-time.coerce :only [to-date]]))

(defonce users (ref {}))
(defonce rooms (ref {}))
(defonce help-docs (ref {}))
(declare *session*)

(defmacro do-when [& clauses]
  `(do ~@(loop [clauses clauses acc []]
           (cond (not (seq clauses)) acc
                 (not (next clauses))
                 (throw (IllegalArgumentException.
                         "do-when requires an even number of forms."))
                 :else (recur (nnext clauses)
                              (conj acc (list 'when (first clauses)
                                              (second clauses))))))))

(defn & [f & fs]
  ((apply comp f fs)))

(defn assoc-in [map [& ks] key val & key-vals]
  (apply update-in map ks assoc key val key-vals))

(defn dissoc-in [map [& ks] key & keys]
  (apply update-in map ks dissoc key keys))

(defn not-truthy? [& xs]
  (not-every? identity xs))

(defn id?<- [coll]
  (filter identity coll))

(defn date->str [date]
  (-> date to-date str (subs 0 19)))

(defn print-message [room user message]
  (println (str "(" (date->str (now)) ")"
                "[" room "] " user ": " message)))

(defn last-input []
  (when-let [in-as (:in-as @*session*)]
    (dosync (alter users assoc-in [in-as] :last-input (now)))))

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
    (dosync (commute help-docs assoc cmd help-map))
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
  (let [in-as (:in-as @*session*)
        users (@rooms room)]
    (cond (not in-as)
          "You must be logged in to talk."
          (not users)
          "A channel with that name does not exist, or contains no users."
          :else (doseq [[_ stream] users]
                  (binding [*out* stream]
                    (print-message room in-as message))))))

(defcommand Join
  "Creates or joins a room."
  [room]
  (if-let [in-as (:in-as @*session*)]
    (do (dosync (alter rooms assoc-in [room] in-as *out*))
        "Successfully joined the room.")
    "You must be logged in to join rooms."))

(defcommand Logout
  "Logs a user out and removes them from all rooms."
  (if-let [in-as (:in-as @*session*)]
    (do (dosync (alter users dissoc-in [in-as] :logged-in? :sign-on :last-input)
                (doseq [[room users] @rooms :when (contains? users in-as)]
                  (alter rooms dissoc-in [room] in-as))
                (send-off *session* dissoc :in-as))
        "You've successfully logged out.")
    "You're not logged in."))

(defcommand Whois
  "Shows information about the user."
  [username]
  (if-let [user (@users username)]
    (let [{:keys [sign-on last-input]} user]
      (dorun (map println (repeat (str username ":"))
                  (id?<- ["WHOIS"
                          (and sign-on (str "Sign on: " (date->str sign-on)))
                          (and last-input (->> ["minutes" "seconds"]
                                               (interleave ((juxt in-minutes in-secs)
                                                            (interval last-input (now))))
                                               (cons "Idle") (join " ")))]))))
    "A user with that username was not found."))

(defcommand Session
  "Retrieves information about your current session."
  (let [{:keys [in-as]} @*session*
        {:keys [sign-on]} (@users in-as)]
    (do-when in-as (println "Logged in as:" in-as)
             sign-on (println "Signed on:" (date->str sign-on)))))

(defprotocol PPlugins
  (loaded [_] "Shows a set of the loaded plug-ins.")
  (load [_] [_ command] "Loads all, or a single plug-in.")
  (unload [_] [_ command] "Unloads all, or a single plug-in.")
  (update [_] "Reloads the plug-in configuration file.")
  (reload [_] "Updates, unloads, loads."))

(defrecord Plugins [plugin-file loaded]
  PPlugins
  (loaded [_] @loaded)
  (load [plugins command]
        (try (require (symbol (str "clj-chat.plugins." command)) :reload)
             (swap! loaded conj command)
             (catch Exception e
               (println e)
               (println "Plugin:" (str "<" command ">") "could not be loaded.")
               (println "Reason:" (cond (instance? FileNotFoundException e)
                                        "File not found."
                                        :else "Unknown.")))))
  (load [plugins] (doseq [command @loaded] (load plugins command)))
  (unload [plugins] (doseq [command @loaded] (unload plugins command)))
  (unload [plugins command]
          (dosync (commute help-docs dissoc command))
          (swap! loaded disj command)
          (remove-method execute command))
  (update [_] (reset! loaded (-> plugin-file in :plugins)))
  (reload [plugins] (update plugins) (unload plugins) (load plugins)))

(defn init-plugins [plugin-file]
  (defonce plugins (Plugins. plugin-file (atom #{})))
  (update plugins) (load plugins))

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
  (defonce server (create-server 3333 loop-handler))
  (init-plugins "plugins.config"))
