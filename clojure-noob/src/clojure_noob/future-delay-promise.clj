(ns clojure-noob.future-delay-promise)

; (def task (future
;   (Thread/sleep 1000)
;   (println "task")))

; (println "A")

; (let [result (delay (println "once") (+ 1 1))]
;   (println "deref: " (deref result))
;   (println "@: " @result))


(def yak-butter-international
  { :store "Yak Butter Internation"
    :price 90
    :smoothness 90 })
(def butter-than-nothing
  { :store "Butter than nothing"
    :price 150
    :smoothness 83 })
(def baby-got-yak
  { :store "Baby got tak"
    :price 94
    :smoothness 99 })

(defn mock-api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  [butter]
  (and
    (<= (:price butter) 100)
    (>= (:smoothness butter) 97)
    butter))

(defn runButter
  []
  (time
    (let [butter-promise (promise)]
      (doseq [butter [yak-butter-international butter-than-nothing baby-got-yak]]
        (future (if-let [satisfactory-butter (satisfactory? (mock-api-call butter))]
                  (deliver butter-promise satisfactory-butter))))
      (println "And the winner is:" @butter-promise))))  

(defn search
  [query]
  (let
    [result (promise) 
      todos (future (slurp (str "http://jsonplaceholder.typicode.com/comments/" query)))
      users (future (slurp (str "http://jsonplaceholder.typicode.com/users/" query)))
      ]
    (doseq [req [todos users]] (deliver result @req))
    (println @result)))

(get { :name "M" } (keyword "name"))
