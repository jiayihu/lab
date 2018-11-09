(ns clojure-noob.process-pipeline
  (:require [clojure.core.async
    :as a
    :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]]))

(let [c1 (chan) c2 (chan) c3 (chan)]
  (go (>! c2 (clojure.string/upper-case (<! c1))))
  (go (>! c3 (clojure.string/reverse (<! c2))))
  (go (println (<! c3)))
  (>!! c1 "redrum")
  (Thread/sleep 200))
