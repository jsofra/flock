(ns flock.core
  (:use quil.core)
  (:require [flock.dynamic :as dyn])
  (:gen-class))

(defn -main [& args]
  (println "Start Flocking!")
  (defsketch flock-sketch
    :title "Flock!"
    :setup dyn/setup
    :draw dyn/draw
    :mouse-released dyn/mouse-released
    :mouse-moved dyn/mouse-moved
    :size [800 800]))
