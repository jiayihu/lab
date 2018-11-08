(ns clojure-noob.atoms)

(def fred (atom { :cuddle-hunger-level 0
                  :percent-deteriorated 0 }))

(swap! fred
  (fn [current-state] (merge-with + current-state {:cuddle-hunger-level 1
                                                    :percent-deteriorated 1})))

@fred
