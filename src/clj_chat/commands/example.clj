(ns clj-chat.commands.example
  (:use [clj-chat.core :only [defcommand]]))

(defn example []
  (println "EXAMPLE!!"))

(defcommand "Example"
  (example))
