(ns clj-chat.commands.repl
  (:use [clj-chat.core :only [defcommand command-str]]
        [clojure.contrib.str-utils2 :only [trim]]))

(defcommand "repl"
  "Evaluates some Clojure. (not sandboxed)"
  ["&" "input"]
  (let [input (-> input (subs 5) trim)]
    (if-not (empty? input)
      (println (try (-> input read-string eval)
                    (catch Exception e e)))
      "Requires input.")))
