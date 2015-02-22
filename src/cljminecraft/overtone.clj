(ns cljminecraft.overtone
  "Music and Minecraft."
  (:use [mud.core] [mud.timing] [overtone.core])
  (:require [cljminecraft.bukkit :as bk]
            [cljminecraft.blocks :as b]
            [cljminecraft.world :as w]
            [cljminecraft.entity :as e]
            [cljminecraft.items :as i]))

;;(connect-external-server)

(bk/broadcast "Overtone, Clojure and Minecraft")

(def player (first (.getOnlinePlayers (bk/server))))

(defn block "relative to player" [x y z material]
  (let [l (.getLocation player)
        m (i/get-material material)]
    (doto l
      (.setX (+ x (.getX l)))
      (.setY (+ y (.getY l)))
      (.setZ (+ z (.getZ l))))
    (bk/ui-sync
     @cljminecraft.core/clj-plugin
     (fn []
       (doto (.getBlock l)
         (.setData 0)
         (.setType (.getItemType m))
         (.setData (.getData m)))))))

(comment
  (loop [x -1
         y -1]
    (block x y 0 :brick)
    (when (> x -10)
      (recur (dec x) (inc y)))))

(block 9 10 0 :air)

(def ctx (b/setup-context (first (.getOnlinePlayers (bk/server)))))
(defn draw [m actions] (bk/ui-sync @cljminecraft.core/clj-plugin #(apply b/run-actions ctx (b/material m) actions)))
(defn monster [type] (bk/ui-sync @cljminecraft.core/clj-plugin #(e/spawn-entity (:origin ctx) type)))

(defn bump-y [y-offset] (.setY (:origin ctx) (+ y-offset (.getY (:origin ctx)))))
(defn bump-x [x-offset] (.setX (:origin ctx) (+ x-offset (.getX (:origin ctx)))))
(defn bump-z [z-offset] (.setZ (:origin ctx) (+ z-offset (.getZ (:origin ctx)))))

;;(.setY (:origin ctx) 70)

(ctl-global-clock 8.0)

(def cell-size (atom 1))
(def growth (atom 0))
(def material-bag (cycle [:sand :stone :grass :wood :dirt :wool :pumpkin :skull :stationary_water  :water :cobblestone :lava ]))
(def instructions [(b/pen-up)
                   (b/up 2)
                   (b/forward 1)
                   (b/left 1)
                   (b/pen-down)
                   (b/back 2)

;;                   (b/left (rand-int 10))
;;                   (b/right (rand-int 10))
;;                   (b/up (rand-int 10))
;;                   (b/right (rand-int 10))
;;                   (b/down (rand-int 10))
                   ])

(reset! cell-size 20)

(def sub-trigger
  (on-beat-trigger
   8
   (fn []
     (swap! growth inc)
     (if (= (nth material-bag @growth) :water)
       (do       (sample-player subby :rate 0.5 :amp 1.0)
                 (sample-player wop :rate -0.8 :amp 1.0))
       (sample-player subby :rate 0.4)
       )
     (draw (nth material-bag @growth) instructions))))

(remove-beat-trigger sub-trigger)
(remove-all-beat-triggers)

(def instr2 [(b/pen-up)
             (b/up 5)
             (b/left 3)
             (b/forward 1)
             (b/pen-down)
             (b/forward (mod @growth 5))])
(def mat2 [:sand])

(def ring-trigger (on-beat-trigger
        32
        (fn []
          (ring-hat :amp 0.2)
          (draw (choose mat2) instr2))))

(remove-beat-trigger ring-trigger)

(def highhat (freesound 53532))

(def sub2-trigger
  (on-beat-trigger
   (* 4)
   (fn []
     (highhat :rate 1.0)
     (monster :pig)
     (draw :air [(b/up 10) (b/left 1) (b/forward 10) (b/left 1)]))))

(remove-beat-trigger sub2-trigger)
(remove-all-beat-triggers)
(stop)

(do
  (def dirty-kick (freesound 30669))
  (def ring-hat (freesound 12912))
  (def snare (freesound 26903))
  (def click (freesound 406))
  (def wop (freesound 85291))
  (def subby (freesound 25649)))
