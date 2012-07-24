(ns flock.dynamic
  (:use quil.core)
  (:require [flock.vector :as vt]
            [clojure.math.combinatorics :as comb]))

(defn alignment [boid boids]
  (vt/avg (map :vect boids)))

(defn separation [boid boids]
  (let [loc (:pos boid)
        other-locs (map :pos boids)
        diffs (map #(let [dist (vt/dist loc %)]
                      (-> (vt/sub loc %)
                         vt/normalise
                         (vt/div dist))) other-locs)]
    (vt/avg diffs)))

(defn cohesion [boid boids]
  (let [target (vt/avg (map :pos boids))]
    (-> (vt/sub target (:pos boid))
        (vt/div 100.0))))

(defn avoid [target boid]
  (-> (vt/sub (:pos boid) target)
      (vt/div 100.0)))

(def neighbour-dist 80.0)
(def momentum 0.5)
(def max-mag 4.0)
(def boid-rules [{:rule alignment :weight 0.2}
                 {:rule separation :weight 20.0}
                 {:rule cohesion :weight 0.6}
                 ])

(defn apply-force [{:keys [pos vect] :as boid} force]
  (let [new-vect (vt/add
                  vect
                  (vt/mult vect momentum)
                  (vt/mult force (- 1.0 momentum)))]
    (assoc boid :vect (vt/limit-mag new-vect max-mag))))

(defn calc-force [boid boids {:keys [rule weight]}]
  (-> (if (zero? (count boids))
         [0.0 0.0]
         (rule boid boids))
      (vt/mult weight)))

(defn run-rules [boid boids rules]
  (let [weighted-forces (map #(calc-force boid boids %) rules)
        force (reduce vt/add [0.0 0.0] weighted-forces)]
    (apply-force boid force)))

(defn add-boid [boids* boid]
  (swap! boids* conj boid))

(defn wrap-borders [pos]
  (let [w (width) h (height)
        [x y] pos
        x (if (> x w) (- x w) x)
        x (if (< x 0) (+ x w) x)
        y (if (> y h) (- y h) y)
        y (if (< y 0) (+ y h) y)]
    [x y]))

(defn trans-boid [{:keys [pos vect] :as boid}]
  (assoc boid :pos (wrap-borders (vt/add pos vect))))

(defn calc-dist-map [boids]
  (let [pairs (map (partial into #{}) (comb/combinations boids 2))]
    (into {} (for [p pairs] [p (apply vt/dist (map :pos p))]))))

(defn calc-neighbours [boid boids thres dist-map]
  (filter #(< (dist-map #{boid %}) thres) (disj boids boid)))

(defn update-fn [boids]
  (let [dist-map (calc-dist-map boids)
        boid-update (fn [boid]
                      (let [neighbours (calc-neighbours boid boids neighbour-dist dist-map)]
                        (trans-boid (run-rules boid neighbours boid-rules))))]
    (into #{} (map boid-update boids))))

(defn update-boids [boids*]
  (swap! boids* update-fn))

(defn mouse-released []
  (add-boid (state :boids)
            {:pos  [(mouse-x) (mouse-y)] :vect (vt/rand-vect)}))

(defn mouse-moved []
  (let [boids* (state :boids)]
    (swap! boids*
           (fn [boids]
             (into #{}
                   (map #(apply-force % (vt/mult (avoid [(mouse-x) (mouse-y)] %) 100.0)) boids))))))

(defn draw-boid [{:keys [pos vect]}]
  (with-translation pos
    (with-rotation [(+ (vt/heading vect) (Math/toRadians 90))]
      (triangle 0 -10 -5 10 5 10))))

(defn setup []
  (set-state! :boids (atom #{}))
  (smooth)
  (frame-rate 30))

(defn draw []
  (background 200)
  (stroke 0 0 0)
  (fill 0 0 255)

  (update-boids (state :boids))

  (doseq [boid @(state :boids)]
    (draw-boid boid)))
