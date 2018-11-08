(ns clojure-noob.vampire)

(def filename "suspects.csv")
(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",") (clojure.string/split string #"\n")))

(defn mapify
  [rows]
  (map (fn [unmapped-row]
          (reduce (fn [row-map [vamp-key value]]
                    (assoc row-map vamp-key (convert vamp-key value)))
                    {}
                    (map vector vamp-keys unmapped-row)))
        rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(glitter-filter 3 (mapify (parse (slurp filename))))
