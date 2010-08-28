(ns clj-chat.plugins.weather
  (:use [clj-chat.core :only [defcommand]]
        [clj-weather.api.yahoo :only [yahoo-weather]]
        [clojure.string :only [join]]))

(defcommand weather
  [& search]
  (let [[{:keys [city country]} condition]
        ((juxt :location :condition) (yahoo-weather search))]
    (println "Yahoo! Weather:" (str city ",") country)
    (println (->> condition vals (join ", ")))))
