(ns clj-chat.plugins.calculator
  (:use [clj-chat.core :only [defcommand fn-or]]
        [clojure.string :only [split]]))

(def operators #{'+ '- '* '/})

(defn- boolean? [x]
  ((fn-or true? false?) x))

(defcommand calc
  "A simple infix-notation calculator, supporting operators \"+ - * \""
  [& input]
  (let [result (try (let [[init & operations :as input] (map read-string (split input #"\s+"))
                          result (reduce (fn [init [op n]] ((resolve op) init n)) init
                                         (partition-all 2 operations))]
                      (when (and (every? (fn-or number? operators) input)
                                 ((fn-or number? boolean?) result)) result))
                    (catch Exception _))]
    (println (or result "Input error."))))
