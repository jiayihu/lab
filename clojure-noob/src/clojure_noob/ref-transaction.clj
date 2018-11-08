(ns clojure-noob.ref-transaction)

(def counter (ref 0))
(def counter-transaction 
  (future
    (dosync
      (alter counter inc)
      (println @counter)
      (Thread/sleep 500)
      (alter counter inc)
      (println @counter))))
(Thread/sleep 250)
(println @counter)
@counter-transaction
(println @counter)
