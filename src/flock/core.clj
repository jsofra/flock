(ns flock.core
  (:use quil.core)
  (:require [flock.dynamic :as dyn]))

(defsketch flock-sketch
  :title "Flock!"
  :setup dyn/setup
  :draw dyn/draw
  :mouse-released dyn/mouse-released
  :mouse-moved dyn/mouse-moved
  :size [400 400])
