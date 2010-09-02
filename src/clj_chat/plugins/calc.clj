(ns clj-chat.plugins.calc
  (:use [clj-chat.core :only [defcommand fn-or]]
        [clojure.contrib.seq :only [separate]]
        [clojure.string :only [split]]))

(def operators #{'+ '- '* '/ '=})

(defcommand calc
  "A simple infix-notation calculator, supporting operators \"+ - * / =\""
  [& input]
  (try (let [[init & operations :as input] (map read-string (split input #"\s+"))
             result (eval (list* '-> init (map (fn [operation] (remove nil? operation))
                                               (partition-all 2 operations))))]
         (if (or (not-every? (fn-or number? operators) input)
                 (not (number? result)))
           "Input error." (str result)))
       (catch Exception _ "Input error.")))
