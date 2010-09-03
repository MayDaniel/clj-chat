(ns clj-chat.db
  (:use somnium.congomongo
        [clojure.string :only [join]]))

(mongo! :db "clj-chat")

(defn user-map
  ([username] {:username username})
  ([username password] {:username username :password password}))

(defn fetch-user [username]
  (fetch-one :users :where (user-map username)))

(defn user-exists? [username]
  (boolean (fetch-user username)))

(defn add-user! [username password]
  (boolean (and (not (user-exists? username))
                (insert! :users (user-map username password)))))

(defn update-user! [username f & args]
  (let [user (fetch-user username)]
    (update! :users user (apply f user args))))

(defn remove-user! [username]
  (destroy! :users (user-map username)))

;;;;;;

(defn help-map
  ([command] {:command command})
  ([command doc] {:command command :doc doc}))

(defn fetch-help
  ([] (->> (fetch :help) (map :command) (join " ")))
  ([command] (:doc (fetch-one :help :where (help-map command)))))

(defn remove-help! [command]
  (destroy! :help (help-map command)))

(defn add-help! [command doc]
  (remove-help! command)
  (insert! :help (help-map command doc)))

;;;;;;

(defn fetch-rooms
  ([] (fetch :rooms))
  ([room] (fetch-one :rooms :where {:room room})))

(defn room-exists? [room]
  (boolean (fetch-rooms room)))

(defn join-room! [room username]
  (if (room-exists? room)
    (let [room (fetch-rooms room)]
      (update! :rooms room (update-in room [:users] conj username)))
    (insert! :rooms {:room room :users #{username}})))

(defn exit-room! [room username]
  (let [room (fetch-rooms room)]
    (update! :rooms room (update-in room [:users] #(remove #{username} %)))))

(defn fetch-room-users [room]
  (:users (fetch-rooms room)))

;;;;;;

(defn has-record? [username]
  (boolean (fetch-one :messages :where (user-map username))))

(defn fetch-messages [username]
  (:messages (fetch-one :messages :where (user-map username))))

(defn add-message! [username message]
  (if (has-record? username)
    (let [record (fetch-messages username)]
      (update! :messages record (update-in record [:messages] conj message)))
    (insert! :messages {:username username :messages [message]})))

(defn remove-message! [username message]
  (let [record (fetch-one :messages :where (user-map username))]
    (update! :messages record (update-in record [:messages] #(remove #{message} %)))))
