(ns clj-chat.plugins.message
  (:use [clj-chat.core :only [defcommand *session* user-streams date->str]]
        [somnium.congomongo :only [fetch-one update! insert!]])
  (:require [clj-chat.db :as db]))

(defn- print-private-msg [{:keys [date from message]}]
  (println (str "Private: [" date "]" "(" from ")") message))

;;;;;;

(defn- has-record? [username]
  (boolean (fetch-one :messages :where (db/user-map username))))

(defn- fetch-messages [username]
  (:messages (fetch-one :messages :where (db/user-map username))))

(defn- add-message! [username message]
  (if (has-record? username)
    (let [record (fetch-messages username)]
      (update! :messages record (update-in record [:messages] conj message)))
    (insert! :messages {:username username :messages [message]})))

(defn- remove-message! [username message]
  (let [record (fetch-one :messages :where (db/user-map username))]
    (update! :messages record (update-in record [:messages] #(remove #{message} %)))))

;;;;;;

(defcommand msg
  "Sends a private message to the target user. If they're logged out, the message is saved, and can be accessed using command \"/inbox\""
  [target & message]
  (let [in-as (:in-as @*session*)
        msg {:message message :from in-as :date (date->str)}]
    (cond (not in-as)
          "You must be logged in to send private messages."
          (not (db/user-exists? target))
          "The specified user does not exist."
          (:logged-in? (db/fetch-user target))
          (binding [*out* (@user-streams target)]
            (print-private-msg msg))
          :else (do (add-message! target msg)
                    "The message was added to their inbox."))))

(defcommand inbox
  "View your private messages"
  (if-let [in-as (:in-as @*session*)]
    (let [msgs (fetch-messages in-as)
          msg-count (count msgs)]
      (if (zero? msg-count)
        "You have 0 messages in your inbox."
        (doseq [msg msgs]
          (print-private-msg msg)
          (remove-message! in-as msg))))
    "You must be logged in to view your private messages."))
