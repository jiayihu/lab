(ns clojure-noob.agents)

(def counter (agent 0))

(send counter (fn [c]
  (Thread/sleep 1000)
  (+ 10 c)))

(println @counter)

