(ns clojure-noob.operator
  (:require [clojure.core.async
              :as a
              :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]]))

(defn operator
  [name]
  (let [c (chan)]
    (go
      (dotimes [n 5]
        (>! c (str name ": " n))
        (Thread/sleep (rand-int 1000))))
    c))

(def joe (operator "Joe"))
(def ann (operator "Ann"))

(dotimes [n 5]
  (println (<!! joe))
  (println (<!! ann)))
