(ns clj-chat.plugins.echo
  (:use [clj-chat.core :only [defcommand]]))

(defcommand echo
  [& input]
  (println input))
