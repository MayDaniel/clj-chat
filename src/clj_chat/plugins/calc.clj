(ns clj-chat.plugins.calc
  (:use [clj-chat.core :only [defcommand fn-or]]
        [clojure.contrib.seq :only [separate]]
        [clojure.string :only [split]]))

(def operators #{'+ '- '* '/ '=})

(defcommand calc
  "A simple infix-notation calculator."
  [& input]
  (try (let [input (map read-string (split input #"\s+"))
             init (first input)
             operations (->> (rest input)
                             (separate symbol?)
                             (apply interleave))]
         (let [result (reduce (fn [result [op n]] ((resolve op) result n)) init
                              (partition 2 operations))]
           (if (or (even? (count input))
                   (not-every? (fn-or number? operators) operations)
                   (not (number? result)))
             "Input error." (str result))))
       (catch Exception _ "Input error.")))
