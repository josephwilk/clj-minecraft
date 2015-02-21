(ns cljminecraft.overtone
  (:use [mud.core] [mud.timing] [overtone.core])
  (:require [cljminecraft.bukkit :as bk]
            [cljminecraft.blocks :as b]))

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

(def t2 (on-beat-trigger 16
                         (fn []
                           (subby)
                           (swap! growth inc)
                           (draw :water [(b/pen-up) (b/up 5) (b/forward @growth) (b/pen-down) (b/left 1) (b/forward 1)]))))


(def t (on-beat-trigger 32 (fn []
                             (ring-hat)
                             (draw :sand [(b/up 10) (b/left 1) (b/forward 10)]))))

(remove-beat-trigger t2)
(remove-all-beat-triggers)
(stop)
