(ns flock.dynamic
  (:use quil.core)
  (:import processing.core.PVector))

(defn draw-boid [{:keys [pos vect]}]
  (with-translation [(.-x pos) (.-y pos)]
    (with-rotation [(+ (.heading2D vect) (Math/toRadians 90))]
      (triangle 0 -10 -5 10 5 10))))

(defn normalize [vect]
  (let [v (.get vect)]
    (.normalize v)
    v))

(defn rand-PVector []
  (normalize (PVector. (- 1.0 (rand 2.0)) (- 1.0 (rand 2.0)))))

(defn dist-filter [thres boid boids]
  (filter #(< (PVector/dist (:pos boid) (:pos %)) thres) boids))

(defn vect-avg [vects]
 (-> (reduce #(PVector/add %1 %2) (PVector.) vects)
     (PVector/div (float (count vects)))))

(defn alignment [boid boids]
  (vect-avg (map :vect boids)))

(defn separation [boid boids]
  (let [loc (:pos boid)
        locs (map :pos boids)
        dists (map #(PVector/dist loc %) locs)
        diffs (map #(-> loc
                        (PVector/sub %1)
                        normalize
                        (PVector/div %2)) locs dists)]
    (vect-avg diffs)))

(defn cohesion [boid boids]
  (let [target (vect-avg (map :pos boids))]
    (-> (PVector/sub target (:pos boid))
        (PVector/div 100.0))))

(defn avoid [target boid]
  (-> (PVector/sub (:pos boid) target)
      (PVector/div 100.0)))

(defn limit-force [force limit]
  (if (> (.mag force) limit)
    (PVector/mult (normalize force) (float limit))
    force))

(defn apply-force [{:keys [pos vect] :as boid} force]
  (assoc boid :vect (limit-force (PVector/add vect force) 4.0)))

(defn run-rules [boid boids rules weights]
  (let [forces (map (fn [rule] (if (zero? (count boids))
                                (PVector.)
                                (rule boid boids))) rules)
        weighted-forces (map #(PVector/mult %1 (float %2)) forces weights)
        force (reduce #(PVector/add %1 %2) (PVector.) weighted-forces)]
    (apply-force boid force)))

(defn add-boid [boids* boid]
  (swap! boids* conj boid))

(defn wrap-borders [pos]
  (let [w (width) h (height)
        x (.-x pos) y (.-y pos)
        x (if (> x w) (- x w) x)
        x (if (< x 0) (+ x w) x)
        y (if (> y h) (- y h) y)
        y (if (< y 0) (+ y h) y)]
    (PVector. x y)))

(defn trans-boid [{:keys [pos vect] :as boid}]
  (assoc boid :pos (wrap-borders (PVector/add pos vect))))

(defn update-fn [boids]
  (into #{}
        (map #(let [neighbours (into #{} (dist-filter 500.0 % boids))]
                (-> %
                    (run-rules (disj neighbours %)
                               [alignment separation cohesion]
                               [1.0 2.0 1.0])
                    trans-boid)) boids)))

(defn update-boids [boids*]
  (swap! boids* update-fn))

(defn mouse-released []
  (add-boid (state :boids)
            {:pos (PVector. (mouse-x) (mouse-y))
             :vect (rand-PVector)}))

(defn mouse-moved []
  (let [boids* (state :boids)]
    (swap! boids*
           (fn [boids]
             (into #{}
                   (map #(apply-force % (avoid (PVector. (mouse-x) (mouse-y)) %))
                        boids))))))

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
