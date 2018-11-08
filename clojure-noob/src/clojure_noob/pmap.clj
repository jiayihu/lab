(def alphabet-length 26)

(def letters (mapv (comp str char (partial + 65)) (range alphabet-length)))

(defn random-string
  [length]
  (apply str (take length (repeatedly #(rand-nth letters)))))

(defn random-string-list
  [list-length string-length]
  (doall (take list-length (repeatedly (partial random-string string-length)))))

(def orc-names (random-string-list 3000 7000))
(def orc-names-abbrevs (random-string-list 2000 300))

(time (dorun (map clojure.string/lower-case orc-names)))
(time (dorun (pmap clojure.string/lower-case orc-names)))
(time
  (dorun
    (apply concat
      (pmap (fn [name] (doall (map clojure.string/lower-case name)))
            (partition-all 1000 orc-names)))))
