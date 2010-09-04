(ns clj-chat.plugins.leet
  (:use [clj-chat.core :only [defcommand]]
        [clojure.string :only [lower-case]]))

(defn- map-str [f coll]
  (apply str (map f coll)))

(defn- char->leet [c]
  (case (lower-case c)
        "e" 3
        "a" 4
        "b" 8
        "g" 9
        "i" \!
        "l" 1
        "o" 0
        "q" 9
        "s" 5
        "t" 7
        "z" 2
        c))

(defcommand leet
  [& input]
  (map-str char->leet input))
