(ns flock.dynamic
  (:use quil.core)
  (:require [flock.vector :as vt]
            [clojure.math.combinatorics :as comb]))

(defn alignment [boid {:keys [neighbours]}]
  (vt/avg (map :vect neighbours)))

(defn separation [boid {:keys [neighbours]}]
  (let [loc (:pos boid)
        other-locs (map :pos neighbours)
        diffs (map #(vt/scale-by-mag (vt/sub loc %)) other-locs)]
    (vt/avg diffs)))

(defn cohesion [boid {:keys [neighbours]}]
  (let [target (vt/avg (map :pos neighbours))]
    (-> (vt/sub target (:pos boid))
        (vt/div 100.0))))

(defn avoidance [{boid-pos :pos} {{target-pos :pos target-dist :dist} :target}]
  (if (< (vt/dist boid-pos target-pos) target-dist)
    (vt/scale-by-mag (vt/sub boid-pos target-pos))
    [0 0]))

(def momentum 0.5)
(def max-mag 4.0)
(def boid-rules {:alignment {:rule-fn alignment :weight 0.2 :dist 80.0}
                 :seperation {:rule-fn separation :weight 20.0 :dist 40.0}
                 :cohesion {:rule-fn cohesion :weight 0.6 :dist 80.0}
                 })

(defn calc-force [boid neighbours-fn {:keys [rule-fn weight dist] :as args}]
  (let [force (if dist
                (let [neighbours (neighbours-fn dist)]
                  (if (zero? (count neighbours))
                    [0.0 0.0]
                    (rule-fn boid (merge {:neighbours neighbours} args))))
                (rule-fn boid args))]
    (vt/mult force weight)))

(defn apply-force [{:keys [pos vect] :as boid} force]
  (let [new-vect (vt/add
                  vect
                  (vt/mult vect momentum)
                  (vt/mult force (- 1.0 momentum)))]
    (assoc boid :vect (vt/limit-mag new-vect max-mag))))

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

(defn run-rules [boid neighbours-fn rules]
  (let [weighted-forces (map #(calc-force boid neighbours-fn %) rules)
        force (reduce vt/add [0.0 0.0] weighted-forces)]
    (apply-force boid force)))

(defn calc-dist-map [boids]
  (let [pairs (map (partial into #{}) (comb/combinations boids 2))]
    (into {} (for [p pairs] [p (apply vt/dist (map :pos p))]))))

(defn calc-neighbours [boid boids dist-map dist-thres]
  (filter #(< (dist-map #{boid %}) dist-thres) (disj boids boid)))

(defn update-fn [boids rules]
  (let [dist-map (calc-dist-map boids)
        boid-update (fn [boid]
                      (let [neighbours-fn (partial calc-neighbours
                                                   boid boids dist-map)]
                        (trans-boid (run-rules boid neighbours-fn rules))))]
    (into #{} (map boid-update boids))))

(defn update-boids [boids* rules*]
  (swap! boids* update-fn (vals @rules*)))

(defn add-boid [boids* boid]
  (swap! boids* conj boid))

(defn mouse-released []
  (add-boid (state :boids) {:pos  [(mouse-x) (mouse-y)] :vect (vt/rand-vect)}))

(defn mouse-moved []
  (let [rule {:rule-fn avoidance :weight 50.0
              :target {:pos [(mouse-x) (mouse-y)] :dist 40.0}}
        rules* (state :rules)]
    (swap! rules* assoc :avoidance rule)))

(defn draw-boid [{:keys [pos vect]}]
  (with-translation pos
    (with-rotation [(+ (vt/heading vect) (Math/toRadians 90))]
      (triangle 0 -10 -5 10 5 10))))

(defn setup []
  (set-state! :boids (atom #{})
              :rules (atom boid-rules))
  (smooth)
  (frame-rate 30))

(defn draw []
  (background 200)
  (stroke 0 0 0)
  (fill 0 0 255)

  (update-boids (state :boids) (state :rules))

  (doseq [boid @(state :boids)]
    (draw-boid boid)))
