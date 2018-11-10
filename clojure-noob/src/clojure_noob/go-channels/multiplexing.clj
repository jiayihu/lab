(ns clojure-noob.multiplexing
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

(defn multiplexing
  [c1 c2]
  (let [c (chan)]
    (go (while true (>! c (<! c1))))
    (go (while true (>! c (<! c2))))
    c))

(def joe (operator "Joe"))
(def ann (operator "Ann"))

(let [c (multiplexing joe ann)]
  (dotimes [n 10]
    (println (<!! c))))
