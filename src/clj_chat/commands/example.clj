(ns clj-chat.commands.example
  (:use [clj-chat.core :only [defcommand]]))

(defcommand "Example" "Prints a useless string" []
  (str "Ich bin ein example."))
