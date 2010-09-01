(ns clj-chat.db
  (:use [somnium.congomongo :only [mongo! insert! update! destroy! fetch-one]]))

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
