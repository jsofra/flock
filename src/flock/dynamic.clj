(ns flock.dynamic
  "This is an implementation of a flocking model first designed by Craig Reynolds in 1986.
   You can see Craig's discussion of it here: http://www.red3d.com/cwr/boids/

   This also draws on some ideas from 'The Computational Beauty of Nature' - Gary William Flake

   It is a very straight foward implementation that is not efficient but allows for rules
   to be added, removed and modified while the application is running.

   TODO:
   * Add some form of data structure for efficently calculating neighbours,
     possibly a quad-tree or kd-tree.
   * Write or find faster vector operations (could use PVector from proccessing libs).
   * Refine the boid neigbourhood by restricting the angular range.
   * Add 'sight' rule from Flake to produce V shape."

  {:author "James Sofra"}

  (:use quil.core)
  (:require [flock.vector :as vt]
            [clojure.math.combinatorics :as comb]))

(defn alignment
  "Steer towards the average heading of the neighbours.
   Returns the average of all the neighbours heading vectors."
  [{:keys [neighbours]}]
  (vt/avg (map :vect neighbours)))

(defn separation
  "Steer to avoid crowding neighbours.
   Returns the average of all the vectors pointing away from the neighbours.
   These vectors are scaled by their magnitude so that the boids will repond
   with greater force the closer they get to each other."
  [{{boid-pos :pos} :boid neighbours :neighbours}]
  (let [diffs (for [n-pos (map :pos neighbours)]
                (vt/div-by-mag (vt/sub boid-pos n-pos)))]
    (vt/avg diffs)))

(defn cohesion
  "Steer to move toward the average position of the neighbours.
   Returns a scaled vector from the boid to the average position of its neigbours."
  [{:keys [neighbours boid]}]
  (let [target (vt/avg (map :pos neighbours))]
    (-> (vt/sub target (:pos boid))
        (vt/div 100.0))))

(defn avoidance
  "Avoid a target position.
   The target distance is used to create a buffer around the target position.
   Return a vector away from the target position, or [0 0] if not within dist.
   The returned vector is scaled by its magnitude so the boid will avoid with
   greater force the closer it is to the target."
  [{{boid-pos :pos} :boid {target-pos :pos target-dist :dist} :target}]
  (if (< (vt/dist boid-pos target-pos) target-dist)
    (vt/div-by-mag (vt/sub boid-pos target-pos))
    [0 0]))

(def momentum 0.5)
(def max-mag 4.0)
(def boid-rules
  ^{:doc "Defines a map of initial rules to be used to calculate new forces to
          apply to the boids.
          All the entries in the rule maps will be passed as arguments to the
          the rules-fn each iteration as the simulation runs."}
  {:alignment {:rule-fn alignment :weight 0.2 :dist 80.0}
   :seperation {:rule-fn separation :weight 20.0 :dist 40.0}
   :cohesion {:rule-fn cohesion :weight 0.6 :dist 80.0}
   :avoidance {:rule-fn avoidance :weight 50.0 :target {:pos [0 0] :dist 40.0}}})

(defn calc-force
  "Calculate the weighted force applied to a boid given a fn to calculate its neighbours
   and a map of rule arguments.
   Returns a force vector."
  [boid neighbours-fn {:keys [rule-fn weight dist] :as rule-args}]
  (let [force (if dist
                ;; if there is a dist argument calculate neigbours,
                ;; otherwise don't bother
                (let [neighbours (neighbours-fn dist)]
                  (if (zero? (count neighbours))
                    [0.0 0.0]
                    (rule-fn (merge {:boid boid :neighbours neighbours} rule-args))))
                (rule-fn (merge {:boid boid} rule-args)))]
    (vt/mult force weight)))

(defn apply-force
  "Apply a force to a boid, returning a new boid with the force applied.
   The force is modulated by a momentum value and limited to a maximum limit."
  [{:keys [pos vect] :as boid} force]
  (let [new-vect (vt/add
                  vect
                  (vt/mult vect momentum)
                  (vt/mult force (- 1.0 momentum)))]
    (assoc boid :vect (vt/limit-mag new-vect max-mag))))

(defn wrap-borders
  "Takes a [x y] position vector and returns a position wrapped around
   the borders of the window."
  [pos]
  (let [w (width) h (height)
        [x y] pos
        x (if (> x w) (- x w) x)
        x (if (< x 0) (+ x w) x)
        y (if (> y h) (- y h) y)
        y (if (< y 0) (+ y h) y)]
    [x y]))

(defn trans-boid
  "Translates a boid in space by moving it by the direction and magnitude
   of its heading vector. Returns the translated boid."
  [{:keys [pos vect] :as boid}]
  (assoc boid :pos (wrap-borders (vt/add pos vect))))

(defn run-rules
  "Given a boid, a fn to calculate its neighbours, and a list of rules
   apply the rules to the boid, returning a boid translated by the forces
   calculated by the rules."
  [boid neighbours-fn rules]
  (let [weighted-forces (map #(calc-force boid neighbours-fn %) rules)
        force (reduce vt/add [0.0 0.0] weighted-forces)]
    (apply-force boid force)))

(defn calc-dist-map
  "Returns a map of boid pair sets to distances.
   All combinations of boid pairings are found and distances between them calculated."
  [boids]
  (let [pairs (map (partial into #{}) (comb/combinations boids 2))]
    (into {} (for [p pairs] [p (apply vt/dist (map :pos p))]))))

(defn calc-neighbours
  "Calculates a boid's neighbours within a distance threshold."
  [boid boids dist-map dist-thres]
  (filter #(< (dist-map #{boid %}) dist-thres) (disj boids boid)))

(defn update-fn
  "Given a set of boids and a list of rules returns the set of boids after having the
   rules applied to them."
  [boids rules]
  (let [dist-map (calc-dist-map boids)
        boid-update (fn [boid]
                      (let [neighbours-fn (partial calc-neighbours
                                                   boid boids dist-map)]
                        (trans-boid (run-rules boid neighbours-fn rules))))]
    (into #{} (map boid-update boids))))

(defn update-boids
  "Swap out the old boids with new voids using the update-fn."
  [boids* rules*]
  (swap! boids* update-fn (vals @rules*)))

(defn add-boid [boids* boid]
  (swap! boids* conj boid))

(defn mouse-released
  "Add a boid at a random position when the mouse is released."
  []
  (add-boid (state :boids) {:pos  [(mouse-x) (mouse-y)]
                            :vect (vt/rand-vect)
                            :colour [(rand 255) (rand 255) (rand 255)]}))

(defn mouse-moved
  "Update the target position of the avoidance rule when the mouse is moved.
   This causes the boids to avoid the mouse cusor."
  []
  (swap! (state :rules) assoc-in [:avoidance :target :pos] [(mouse-x) (mouse-y)]))

(defmacro with-style [style & body]
  `(do (push-style)
       (try
         (doseq [[style-fn# args#] ~style]
           (apply style-fn# args#))
         ~@body
         (finally (pop-style)))))

(defn draw-boid [{:keys [pos vect colour]}]
  (with-translation pos
    (with-rotation [(+ (vt/heading vect) (Math/toRadians 90))]
      (with-style
          {fill colour}
          (triangle 0 -10 -5 10 5 10)))))

(defn setup []
  ;; use some atoms to store both boids and rules in the quil application state
  (set-state! :boids (atom #{})
              :rules (atom boid-rules))
  (smooth)
  (frame-rate 30))

(defn draw []
  (with-style
      {background [200]
       stroke [0 0 0]}

    (update-boids (state :boids) (state :rules))

    (doseq [boid @(state :boids)]
      (draw-boid boid))))
