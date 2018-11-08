(ns clojure-noob.pquotes)

(defn quote-word-count
  [number-of-quotes]
  (let [word-count (atom 0)]
    (doseq [x (range number-of-quotes)]
      (let [req (future (slurp "https://www.braveclojure.com/random-quote"))]
        (swap! word-count + (count @req))))
    @word-count))

(quote-word-count 5)
