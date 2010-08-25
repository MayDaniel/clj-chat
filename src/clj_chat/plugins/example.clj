(ns clj-chat.plugins.example
  (:use [clj-chat.core :only [defcommand]]))

(defcommand Example
  (println input))
