(ns clj-chat.admin
  (:use [somnium.congomongo :only [fetch]]
        [clj-chat.core :only [logout]]))

(defn logout-all! []
  (doseq [user (fetch :users)]
    (logout (:username user))))
