(ns clj-chat.core
  (:refer-clojure :exclude [load])
  (:import java.io.FileNotFoundException)
  (:require [clojure.contrib.str-utils2 :as str]
            [clj-chat.db :as db])
  (:use [clojure.contrib.server-socket :only [create-server]]
        [clojure.java.io :only [reader writer]]
        [clojure.string :only [lower-case capitalize join]]
        [clojure.contrib.str-utils :only [re-split]]
        [clj-store.core :only [in]]
        [clj-time
         [core :only [interval in-minutes in-secs now]]
         [format :only [formatters parse unparse]]
         [coerce :only [to-date]]]))

(def user-streams (agent {}))
(declare *session*)

(defmacro do-when [& clauses]
  `(do ~@(loop [clauses (seq clauses) acc []]
           (cond (not clauses) acc
                 (not (next clauses))
                 (throw (IllegalArgumentException.
                         "do-when requires an even number of forms."))
                 :else (recur (nnext clauses)
                              (conj acc (list 'when (first clauses)
                                              (second clauses))))))))

(defn fn-and
  "((fn-and number? integer?) 5) -> true"
  [f & fns]
  (fn [x] (every? boolean ((apply juxt f fns) x))))

(defn fn-or
  "((fn-or char? string?) \"foo\") -> true"
  [f & fns]
  (fn [x] (boolean (some boolean ((apply juxt f fns) x)))))

(def format-time (formatters :basic-date-time))

(defn date->str
  ([] (date->str (now)))
  ([date] (-> date to-date str (subs 0 19))))

(defn print-message [room user message]
  (println (str "(" (date->str) ")"
                "[" room "] " user ": " message)))

(defn interval->now [date]
  (let [[minutes seconds] ((juxt in-minutes in-secs)
                           (interval date (now)))]
    (str minutes " minutes " (mod seconds 60) " seconds")))

(defn last-input []
  (when-let [in-as (:in-as @*session*)]
    (send-off user-streams assoc in-as *out*)
    (db/update-user! in-as merge {:last-input (unparse format-time (now))})))

(defn logout [user]
  (doseq [{:keys [room users]} (db/fetch-rooms) :when (some #{user} users)]
    (db/exit-room! room user))
  (db/update-user! user dissoc :logged-in? :sign-on :last-input))

(defmulti execute (fn [input]
                    (-> (re-split #"\s+" input)
                        (first) (str/drop 1) (lower-case)))
  :default :default)

(defmacro defcommand
  {:arglists '([cmd help-string? help-args? & fn-tail])}
  [cmd & options]
  (let [cmd (-> cmd str lower-case)
        help (if (string? (first options))
               {:doc (first options)} {})
        options (if (string? (first options))
                  (next options) options)
        args (when (vector? (not-empty (first options)))
               (first options))
        last-arg (last args)
        help (if args
               (assoc help :args (join " " args)) help)
        body (if (vector? (first options))
               (next options) options)]
    (db/add-help! cmd help)
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
  (if-let [entry (db/fetch-help command)]
    (let [{:keys [args doc]} entry]
      (println "Docs:" (or doc "There is no help documentation for this command."))
      (println "Args:" (or args "There is no argument string for this command.")))
    (str "Commands: " (db/fetch-help))))

(defcommand Register
  "Registers a new user."
  [username password]
  (cond (db/user-exists? username)
        "A user with that name already exists."
        (not (and username password))
        "You must specify a username and password."
        (not-every? #(re-find #"^[a-zA-Z0-9_]{3,12}$" %) [username password])
        "Invalid username/password."
        :else (do (db/add-user! username password)
                  "Registration successful.")))

(defcommand Login
  "Logs a user in."
  [username password]
  (cond (:in-as @*session*)
        "You are already logged in. Use command /logout to log in as this user."
        (not (and username password))
        "You must specify a username and password."
        (not (db/user-exists? username))
        "That user does not exist."
        (:logged-in? (db/fetch-user username))
        "This user is already logged in."
        (not= password (:password (db/fetch-user username)))
        "Incorrect password."
        :else (do (db/update-user! username merge {:logged-in? true :sign-on (date->str)})
                  (send *session* assoc :in-as username)
                  "Log in successful.")))

(defcommand Say
  "Prints your message to all users in the specified room."
  [room & message]
  (let [in-as (:in-as @*session*)
        users (db/fetch-room-users room)]
    (cond (not in-as)
          "You must be logged in to talk."
          (not users)
          "A channel with that name does not exist."
          :else (doseq [stream (map @user-streams users)]
                  (when stream
                    (binding [*out* stream]
                      (print-message room in-as message)))))))

(defcommand Say
  "Prints your message to all users in the specified room."
  [room & message]
  (let [in-as (:in-as @*session*)
        users (db/fetch-room-users room)]
    (cond (not in-as)
          "You must be logged in to talk."
          (not users)
          "A channel with that name does not exist."
          :else (doseq [user users]
                  (try (binding [*out* (@user-streams user)]
                         (print-message room in-as message))
                       (catch java.lang.NullPointerException _
                         (send user-streams dissoc user)
                         (logout user)))))))

(defcommand Join
  "Creates or joins a room."
  [room]
  (if-let [in-as (:in-as @*session*)]
    (do (db/join-room! room in-as)
        "Successfully joined the room.")
    "You must be logged in to join rooms."))

(defcommand Logout
  "Logs a user out and removes them from all rooms."
  (if-let [in-as (:in-as @*session*)]
    (do (logout in-as)
        (send *session* dissoc :in-as)
        "You've successfully logged out.")
    "You're not logged in."))

(defcommand Whois
  "Shows information about the user."
  [username]
  (if-let [user (db/fetch-user username)]
    (let [{:keys [sign-on last-input]} user]
      (println "WHOIS:" username)
      (do-when sign-on
               (println "Sign on:" sign-on)
               last-input
               (println "Idle:" (interval->now (parse format-time last-input)))))
    "A user with that username was not found."))

(defcommand Session
  "Retrieves information about your current session."
  (let [{:keys [in-as]} @*session*
        {:keys [sign-on]} (db/fetch-user in-as)]
    (do-when in-as (println "Logged in as:" in-as)
             sign-on (println "Signed on:" sign-on))))

(defprotocol PPlugins
  (loadable [_] "Shows a map of the loadable plug-ins, and their commands.")
  (loaded [_] "Shows a set of the loaded plug-ins.")
  (load [_] [_ command] "Loads all, or a single plug-in.")
  (unload [_] [_ command] "Unloads all, or a single plug-in.")
  (update [_] "Reloads the plug-in configuration file.")
  (reload [_] "Updates, unloads, loads."))

(defrecord Plugins [loadable loaded]
  PPlugins
  (loadable [_] @loadable)
  (loaded [_] @loaded)
  (load [_ ns-str]
        (try (require (symbol (str "clj-chat.plugins." ns-str)) :reload)
             (swap! loaded conj ns-str)
             (catch Exception e
               (println "Plugin:" (str "<" ns-str ">") "could not be loaded.")
               (println "Reason:" (cond (instance? FileNotFoundException e)
                                        "File not found" :else "Unknown.")))))
  (load [p] (doseq [command (keys @loadable)] (load p command)))
  (unload [p ns-str]
          (swap! loaded disj ns-str)
          (doseq [command (@loadable ns-str)]
            (db/remove-help! command)
            (remove-method execute command)))
  (unload [p] (doseq [ns-str @loaded] (unload p ns-str)))
  (update [_] (reset! loadable (in "plugins.clj")))
  (reload [p] (doto p update unload load)))

(defn execute-layer [input]
  (try (execute input)
       (catch java.lang.NullPointerException _
         (when-let [in-as (:in-as @*session*)]
           (logout in-as)))))

(defn loop-handler [in out]
  (binding [*in* (reader in)
            *out* (writer out)
            *session* (agent {})]
    (loop [input (read-line)]
      (last-input)
      (let [output (execute-layer input)]        
        (cond (string? output)
              (println output)))
      (recur (read-line)))))

(defn -main []
  (defonce server (create-server 3333 loop-handler))
  (defonce plugins (doto (Plugins. (atom #{}) (atom #{}))
                     update load))
  ;; (use 'clj-chat.core :reload)
  ;; (use 'clj-chat.admin #_:reload)
  ;; (use 'clj-chat.db #_:reload)
  )
