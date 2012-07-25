(ns flock.vector)

(defn add [& vects] (apply mapv + vects))
(defn sub [& vects] (apply mapv - vects))
(defn mult [vect scalar] (mapv #(* % scalar) vect))
(defn div [vect scalar] (mapv #(/ % scalar) vect))
(defn safe-div [vect scalar & {:keys [default] :or {default vect}}]
  (if (zero? scalar)
    default
    (mapv #(/ % scalar) vect)))

(defn mag [vect]
  (Math/sqrt (reduce + (map * vect vect) )))

(defn normalise [vect]
  (let [m (mag vect)]
    (if (or (zero? m) (== m 1)) vect (div vect m))))

(defn limit-mag [vect limit]
  (if (> (mag vect) limit)
    (mult (normalise vect) limit)
    vect))

(defn dist [v1 v2] (mag (sub v1 v2)))

(defn heading [[x y]] (* -1 (Math/atan2 (* -1 y) x)))

(defn avg [vects]
 (-> (reduce add [0.0 0.0] vects)
     (div (count vects))))

(defn rand-vect []
  (normalise [(- 1.0 (rand 2.0)) (- 1.0 (rand 2.0))]))

(defn div-by-mag [vect]
  (safe-div (normalise vect) (mag vect)))
