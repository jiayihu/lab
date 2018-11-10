(ns clojure-noob.ping-pong
  (:require [clojure.core.async
    :as a
    :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]]))

(def c (chan))

(go
  (dotimes [n 5]
    (>! c (str n))
    (Thread/sleep (rand-int 1000))))

(dotimes [n 5]
  (println (<!! c)))
