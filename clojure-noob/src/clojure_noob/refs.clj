(ns clojure-noob.refs)

(def sock-varieties
  #{"darned" "argyle" "wool" "horsehair" "mulleted" "passive-aggressive" "striped"
    "polka-dotted" "athletic" "business" "power" "invisible" "gollumed"})

(defn sock-count
  [sock-variety count]
  {:variety sock-variety
    :count count})

(defn generate-sock-gnome
  [name]
  {:name name
    :socks #{}})

(def sock-gnome (ref (generate-sock-gnome "Barumpharumph")))
(def dryer (ref {:name "LG 1337"
                  :socks (set (map #(sock-count % 2) sock-varieties))}))

(defn steal-sock
  [gnome dryer]
  (dosync
    (when-let [pair (some #(if (= (:count %) 2) %) (:socks @dryer))]
      (let [updated-count (sock-count (:variety pair) 1)]
        (alter gnome update-in [:socks] conj updated-count)
        (alter dryer update-in [:socks] disj pair)
        (alter dryer update-in [:socks] conj updated-count)))))

(defn similar-socks
  [target-sock sock-set]
  (filter #(= (:variety %) (:variety target-sock)) sock-set))

(steal-sock sock-gnome dryer)
(println (:socks @sock-gnome))
(println (similar-socks (first (:socks @sock-gnome)) (:socks @dryer)))
