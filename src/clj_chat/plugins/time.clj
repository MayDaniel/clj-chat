(ns clj-chat.plugins.time
  (:use [clj-chat.core :only [defcommand date->str &]]
        [clj-time.core :only [now]]))

(defcommand time
  "Shows the current time."
  (& date->str now))
