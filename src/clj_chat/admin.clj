(ns clj-chat.admin
  (:use [somnium.congomongo :only [fetch]]
        [clj-chat.core :only [logout user-streams]]))

(defn logout-all! []
  (doseq [user (fetch :users)]
    (logout (:username user))))

(defn global-say [s]
  (doseq [stream (vals @user-streams)]
    (binding [*out* stream]
      (println "GLOBAL:" s))))
