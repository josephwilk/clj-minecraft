(ns cljminecraft.overtone
  "Music and Minecraft."
  (:import [org.bukkit Location])
  (:use [overtone.core] [mud.core] [mud.timing])
  (:require [cljminecraft.bukkit :as bk]
            [cljminecraft.blocks :as b]
            [cljminecraft.world :as w]
            [cljminecraft.entity :as e]
            [cljminecraft.items :as i]))

;;(connect-external-server)

(bk/broadcast "Overtone, Clojure and Minecraft")

(def player (first (.getOnlinePlayers (bk/server))))
(def active-world (last (bk/worlds)))
(def start-player-loc  (.getLocation player))
(def ctx (b/setup-context player))

(defn draw [m actions] (bk/ui-sync @cljminecraft.core/clj-plugin #(apply b/run-actions ctx (b/material m) actions)))
(defn monster [x y z type]
  (let [start-player-loc (.getLocation player)
        l (Location. active-world
                     (.getX start-player-loc)
                     (.getY start-player-loc)
                     (.getZ start-player-loc))

        _ (.setX l (+ x (.getX l)))
        _ (.setY l (+ y (.getX l)))
        _ (.setZ l (+ z (.getX l)))]
    (bk/ui-sync @cljminecraft.core/clj-plugin #(e/spawn-entity l type))))

(defn bump-y [y-offset] (.setY (:origin ctx) (+ y-offset (.getY (:origin ctx)))))
(defn bump-x [x-offset] (.setX (:origin ctx) (+ x-offset (.getX (:origin ctx)))))
(defn bump-z [z-offset] (.setZ (:origin ctx) (+ z-offset (.getZ (:origin ctx)))))

(defn teleport [x y z]
  (let [l (.getLocation player) ]
    (doto l
      (.setX (+ x (.getX l)))
      (.setY (+ y (.getY l)))
      (.setZ (+ z (.getZ l))))
    (.teleport player l)))

(teleport 0 100 0)

(defn blocks "relative to player"
  ([actions material] (blocks (map #(if (= 3 (count %1)) (concat %1 [material])) actions)) )
  ([actions]
     (bk/ui-sync
      @cljminecraft.core/clj-plugin
      (fn []
        (doseq [[x y z m] actions]
          (let [m (i/get-material m)
                l (.getLocation player)]
            (doto l
              (.setX (+ x (.getX l)))
              (.setY (+ y (.getY l)))
              (.setZ (+ z (.getZ l))))
            (doto (.getBlock l)
              (.setData 0)
              (.setType (.getItemType m))
              (.setData (.getData m)))))))))

(defn block "relative to player" [x y z material & [fixed]]
  (let [l (if fixed
            (Location. active-world
                       (.getX start-player-loc)
                       (.getY start-player-loc)
                       (.getZ start-player-loc))
            (.getLocation player))
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

(def stairs (atom {:x -1 :y -1 :z 2}))

(defn add-step [thing]
  (do
    (block (:x @stairs) (:y @stairs) (:z   @stairs) thing true)
    (swap! stairs assoc :x (dec (:x @stairs)))
    ;;(swap! stairs assoc :y (min 5 (inc (:y @stairs))))
    ))

(defn letters []
  (blocks [[-5 2 0] [-5 2 1] [-5 2 2]
           [-5 1 2] [-5 0 2] [-5 -1 2]
           [-5 -1 1] [-5 -1 0]
           [-5 -1 0] [-5 0 0] [-5 1 0]

           [-5 2 -2] [-5 1 -2] [-5 0 -2]
           [-5 -1 -3] [-5 2 -4]
           [-5 1 -4]
           [-5 0 -4]] :dirt))

(defn pattern->cords [pattern material]
  (map
   (fn [[x line]]
     (map
      (fn [[y cell]]
        (case cell
          1 [x y material]
          0 [x y :air]))
      (map vector (range) line)))
   (map vector (range) pattern)))

(defn circle
  ([size thing] (circle size -1 thing))
  ([size y thing]
     (let [top    (map (fn [s] [1  y  s]) (range (- 1 (int (/ size 2))) (dec size)))
           bottom (map (fn [s] [-1 y  s]) (range (- 1 (int (/ size 2))) (dec size)))
           left   (map (fn [s] [s  y  1]) (range (- 1 (int (/ size 2))) (dec size)))
           right  (map (fn [s] [s  y -1]) (range (- 1 (int (/ size 2))) (dec size)))
           cords (distinct (apply concat top bottom left right))]
       (blocks cords thing))))

(defn diamond
  [material]
  (pattern->cords [[0 1 0]
                   [1 0 1]
                   [0 1 0]] material))

(defn corners [material]
  (pattern->cords [[1 0 1]
                   [0 0 0]
                   [1 0 1]] material))

(block -1 -1 1 :dirt)
(circle 5 -1 :stone)
(circle 1 -1 :air)

(circle 3 :dirt)
(diamond :dirt)
(corners :dirt)

(block 0 -1 0 :stone)

(doseq [i (range 100)] (add-step :water))

(comment
  (loop [x -1
         y -1]
    (block x y 0 :air)
    (when (> x -10)
      (recur (dec x) (inc y))))
  )

(defn set-time [t]
  (if-not (integer? t)
    (case t
      :sunset (.setTime active-world 12400)
      :night (.setTime  active-world 21900)
      :day  (.setTime  active-world 0))
    (.setTime (first (bk/worlds)) t)))

(set-time :day)

(blocks [[-5 -1 0]
         [-5 -1 1]
         [-6 -1 1]
         [-6 -1 0]] :air)



;;(.setY (:origin ctx) 70)

(ctl-global-clock 8.0)

(def cell-size (atom 1))
(def growth (atom 0))
(def material-bag (cycle [:sand :stone :grass :wood :dirt :wool :pumpkin :skull :stationary_water :water :cobblestone :lava ]))
(def instructions [(b/pen-up)
                   (b/up 2)
                   (b/forward 1)
                   (b/left 1)
                   (b/pen-down)
                   (b/back 2)

                   (b/left (rand-int 10))
                   (b/right (rand-int 10))
                   (b/up (rand-int 10))
                   (b/right (rand-int 10))
                   (b/down (rand-int 10))
                   ])

(reset! cell-size 20)

(defonce boom-s      (freesound-sample 33637))

(def trigger-g77218
  (on-beat-trigger 8 #(do
                        (add-step (nth (cycle [:glass :brick :grass]) (* -1 (:x @stairs))) )
                        (boom-s)

                       )))

(remove-beat-trigger trigger-g77218)
(remove-all-beat-triggers)

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
   (* 8)
   (fn []
     (highhat :rate 1.0)
     (monster -2 1 0 :pig)
     (block -5 7 0 :sand)
     (blocks [[-5 5 0]
              [-5 4 0]
              [-5 2 0]] :air)
     (draw :air [(b/up 10) (b/left 1) (b/forward 10) (b/left 1)]))))
     (block -5 7 0 :air)

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
