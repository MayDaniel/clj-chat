(ns clj-chat.plugins.time
  (:use [clj-chat.core :only [defcommand date->str]]
        [clj-time.core :only [now]]))

(defcommand Time
  "Shows the current time."
  (date->str (now)))
