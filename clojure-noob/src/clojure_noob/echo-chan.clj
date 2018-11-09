(ns clojure-noob.echo-chan
  (:require [clojure.core.async
              :as a
              :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]]))

(def echo-chan (chan))
; (go (println (<! echo-chan)))
(thread (println (<!! echo-chan))) ; Creates new thread out of `go`'s pool
(>!! echo-chan "ketchup")
