(ns cljminecraft.overtone
  "Music and Minecraft."
  (:use [mud.core] [mud.timing] [overtone.core])
  (:require [cljminecraft.bukkit :as bk]
            [cljminecraft.blocks :as b]
            [cljminecraft.world :as w]))

(connect-external-server)

(do
  (def dirty-kick (freesound 30669))
  (def ring-hat (freesound 12912))
  (def snare (freesound 26903))
  (def click (freesound 406))
  (def wop (freesound 85291))
  (def subby (freesound 25649)))

(def ctx (b/setup-context (first (.getOnlinePlayers (bk/server)))))
(defn draw [m actions] (bk/ui-sync @cljminecraft.core/clj-plugin #(apply b/run-actions ctx (b/material m) actions)))

(ctl-global-clock 8.0)

(def growth (atom 0))
(def material-bag (cycle [:stone :grass :wood]))

(def sub-trigger
  (on-beat-trigger
   16
   (fn []
     (subby)
     (swap! growth inc)
     (draw (nth material-bag @growth) [(b/pen-up) (b/forward 1) (b/left @growth) (b/pen-down) (b/forward @growth)]))))

(remove-beat-trigger sub-trigger)

(def ring-trigger (on-beat-trigger
        32
        (fn []
          (ring-hat)
          (draw :sand [(b/up 10) (b/left 1) (b/forward 10)]))))

(remove-beat-trigger ring-trigger)

(def sub2-trigger
  (on-beat-trigger
   44
   (fn []
     (subby)
     (draw :grass [(b/up 10) (b/left 1) (b/left 1)]))))


(remove-beat-trigger sub2-trigger)
(remove-all-beat-triggers)
(stop)
