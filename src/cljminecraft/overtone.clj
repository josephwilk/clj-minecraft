(ns cljminecraft.overtone
  "Music and Minecraft.
       d[-_-]b
         /█\\
         .Π.
" (:import [org.bukkit Location Material Block]) (:use [overtone.core] [mud.core] [mud.timing]) (:require [cljminecraft.bukkit :as bk]) [cljminecraft.blocks :as b] [cljminecraft.world :as w] [cljminecraft.entity :as e] [cljminecraft.items :as i])

(set! *warn-on-reflection* true)

(do
  (defonce big-reverb-kick-s (freesound 219515))
  (defonce reverb-kick-s     (freesound 90243))

  (defonce light-s (freesound 169678))

  (defonce line-s  (freesound 144919))
  (defonce bonus-s (freesound 55570))

  (defonce highhat    (freesound 53532))
  (defonce dirty-kick (freesound 30669))
  (defonce ring-hat   (freesound 12912))
  (defonce snare      (freesound 193023))
  (defonce click      (freesound 406))
  (defonce wop        (freesound 85291))
  (defonce subby      (freesound 25649))
  (defonce boom-s     (freesound-sample 33637))
)

(do
  (def block-material
    (->>
     (keys i/materials)
     (filter #(re-find #"^[\w]*_block$" (str (name %1))))
     (remove #(some #{%1} [:cake_block :iron_door_block :sugar_cane_block :bed_block]))))
  (def ore-material (filter #(re-find #"_ore" (str (name %1))) (keys i/materials)))

  (def player (first (.getOnlinePlayers (bk/server))))
  (def active-world (last (bk/worlds)))
  (def start-player-loc  (.getLocation player))
  (def ctx (b/setup-context player))

  (defn storm [on] (.setStorm active-world on))

  (defn draw [m actions] (bk/ui-sync @cljminecraft.core/clj-plugin #(apply b/run-actions ctx (b/material m) actions)))

  (defn life [x y z thing]
    (bk/ui-sync @cljminecraft.core/clj-plugin
                #(let [l ^Location (.getLocation player)]
                   (doto l
                     (.setX (+ x (.getX l)))
                     (.setY (+ y (.getY l)))
                     (.setZ (+ z (.getZ l))))
                   (e/spawn-entity l thing))))

(defn bump-y [y-offset] (.setY (:origin ctx) (+ y-offset (.getY (:origin ctx)))))
(defn bump-x [x-offset] (.setX (:origin ctx) (+ x-offset (.getX (:origin ctx)))))
(defn bump-z [z-offset] (.setZ (:origin ctx) (+ z-offset (.getZ (:origin ctx)))))

(defn teleport [x y z]
  (let [l ^Location (.getLocation player) ]
    (doto l
      (.setX (+ x (.getX l)))
      (.setY (+ y (.getY l)))
      (.setZ (+ z (.getZ l))))
    (.teleport player l)))

(defn explode [x y z]
  (let [l ^Location (.getLocation player)]
    (doto l
      (.setX (+ x (.getX l)))
      (.setY (+ y (.getY l)))
      (.setZ (+ z (.getZ l))))
    (bk/ui-sync
     @cljminecraft.core/clj-plugin
     #(w/explode l 10 true))))

(defn blocks-around-player [size mat]
  (let [l ^Location (.getLocation player)
        grid (range (- 0 size) size)]
    (map
     #(let [b-l ^Location (.getLocation %1)]
        [(.getX b-l) (.getY b-l) (.getZ b-l) mat])
     (filter
      #(not= Material/AIR (.getType %1))
      (mapcat
       (fn [x]
         (mapcat
          (fn [z]
            (map (fn [y]
                   (.getBlock
                    (Location. active-world
                               (+ x (.getX l))
                               (+ y (.getY l))
                               (+ z (.getZ l)))))
                 grid))
          (range -10 10)))
       (range -10 10))))))

(defn blocks "relative to player"
  ([actions material] (blocks (map #(if (= 3 (count %1)) (concat %1 [material])) actions)) )
  ([actions]
     (bk/ui-sync
      @cljminecraft.core/clj-plugin
      (fn []
        (doseq [[x y z m] actions]
          (let [m ^Material (i/get-material m)
                l ^Location (.getLocation player)]
            (doto l
              (.setX (+ x (.getX l)))
              (.setY (+ y (.getY l)))
              (.setZ (+ z (.getZ l))))
            (doto ^Block (.getBlock l)
              (.setData 0)
              (.setType (.getItemType m))
              (.setData (.getData m)))))))))

(defn fixed-blocks "fixed at init player location"
  ([actions material] (blocks (map #(if (= 3 (count %1)) (concat %1 [material])) actions)))
  ([actions]
     (bk/ui-sync
      @cljminecraft.core/clj-plugin
      (fn []
        (doseq [[x y z m] actions]
          (let [m ^Material (i/get-material m)
                l ^Location (Location. active-world (.getX start-player-loc) (.getY start-player-loc) (.getZ start-player-loc) )]
            (doto l
              (.setX (+ x (.getX l)))
              (.setY (+ y (.getY l)))
              (.setZ (+ z (.getZ l))))
            (doto ^Block (.getBlock l)
              (.setData 0)
              (.setType (.getItemType m))
              (.setData (.getData m)))))))))

(defn block "relative to player" [x y z material & [fixed]]
  (let [l ^Location (if fixed
                      (Location. active-world
                                 (.getX start-player-loc)
                                 (.getY start-player-loc)
                                 (.getZ start-player-loc))
                      (.getLocation player))
        m ^Material (i/get-material material)]
    (doto l
      (.setX (+ x (.getX l)))
      (.setY (+ y (.getY l)))
      (.setZ (+ z (.getZ l))))
    (bk/ui-sync
     @cljminecraft.core/clj-plugin
     (fn []
       (doto ^Block (.getBlock l)
         (.setData 0)
         (.setType (.getItemType m))
         (.setData (.getData m)))))))

(defn block-fill
  ([x y z mat]        (block-fill x y z 0 0 mat))
  ([x y z offset mat] (block-fill x y z offset 0 mat))
  ([x y z x-offset z-offset mat]
      (blocks (mapcat (fn [i] (map (fn [x] [(+ x-offset i) y (+ z-offset x) mat]) (range (- 0 x) x))) (range (- 0 z) z)))))

(defn blocks-absolute "absolute"
  ([actions material] (blocks (map #(if (= 3 (count %1)) (concat %1 [material])) actions)))
  ([actions]
     (bk/ui-sync
      @cljminecraft.core/clj-plugin
      (fn []
        (doseq [[x y z m] actions]
          (let [m ^Material (i/get-material m)
                l ^Location (Location. active-world x y z)]
            (doto ^Block (.getBlock l)
              (.setData 0)
              (.setType (.getItemType m))
              (.setData (.getData m)))))))))

(defn destroyer-of-worlds []
  (blocks-absolute (blocks-around-player 100 :gravel))
  ;;  (Thread/sleep 5000)
  ;;  (teleport 0 200 0)
  )

(defn paint-line [steps mat] (blocks (map (fn [xy] [xy -1 0 mat]) (range steps))))

(def stairs (atom {:x -1 :y -1 :z 2}))

(defn add-step [thing]
  (block (:x @stairs) (:y @stairs) (:z   @stairs) thing true)
  (swap! stairs assoc :x (dec (:x @stairs)))
  (swap! stairs assoc :y (min 5 (inc (:y @stairs)))))

(defn letter-pattern->cords
  ([pattern x material] (letter-pattern->cords x 0 0 material))
  ([pattern x y-offset z-offset material]
     (mapcat
      (fn [[y line]]
        (map
         (fn [[z cell]]
           (case cell
             1 [x (+ y-offset (- 0 y)) (- (+ z-offset z) 20) material]
             0 [x (+ y-offset (- 0 y)) (- (+ z-offset z) 20) :air]))
         (map vector (range (- 0 (int (/ (count line) 2))) (- (count line) (int (/ (count line) 2)))) line)))
      (map vector (range (- 0 (int (/ (count (first pattern)) 2))) (count (first pattern))) pattern))))

(defn pattern->cords
  ([pattern y material] (pattern->cords y 0 material))
  ([pattern y offset material]
      (mapcat
       (fn [[x line]]
         (map
          (fn [[z cell]]
            (case cell
              1 [x y z material]
              0 [x y z :air]))
          (map vector (range (+ offset (- 0 (int (/ (count line) 2)))) (+ offset (- (count line) (int (/ (count line) 2))))) line)))
       (map vector (range (- 0 (int (/ (count (first pattern)) 2))) (count (first pattern))) (reverse pattern)))))

(def letter-patterns
  {:C [[1 1 1 1 1]
       [1 0 0 0 0]
       [1 0 0 0 0]
       [1 0 0 0 0]
       [1 1 1 1 1]]

   :L [[1 0 0 0 0]
       [1 0 0 0 0]
       [1 0 0 0 0]
       [1 0 0 0 0]
       [1 1 1 1 1]]

   :J [[1 1 1 1 1]
       [0 0 1 0 0]
       [0 0 1 0 0]
       [0 0 1 0 0]
       [1 1 1 0 0]]

   :U [[1 0 0 0 1]
       [1 0 0 0 1]
       [1 0 0 0 1]
       [1 0 0 0 1]
       [1 1 1 1 1]]

   :O [[1 1 1 1 1]
       [1 0 0 0 1]
       [1 0 0 0 1]
       [1 0 0 0 1]
       [1 1 1 1 1]]

   :E [[1 1 1 1 1]
       [1 0 0 0 0]
       [1 1 1 1 0]
       [1 0 0 0 0]
       [1 1 1 1 1]]

   :V [[1 0 0 0 1]
       [1 0 0 0 1]
       [1 0 0 0 1]
       [0 1 0 1 0]
       [0 0 1 0 0]]

   :R [[1 1 1 1 1]
       [1 0 0 0 1]
       [1 1 1 1 1]
       [1 0 0 1 0]
       [1 0 0 0 1]]

   :T [[1 1 1 1 1]
       [0 0 1 0 0]
       [0 0 1 0 0]
       [0 0 1 0 0]
       [0 0 1 0 0]]

   :N [[1 0 0 0 1]
       [1 1 0 0 1]
       [1 0 1 0 1]
       [1 0 0 1 1]
       [1 0 0 0 1]]

   :I [[1 1 1 1 1]
       [0 0 1 0 0]
       [0 0 1 0 0]
       [0 0 1 0 0]
       [1 1 1 1 1]]

   :M [[1 0 0 0 1]
       [1 1 0 1 1]
       [1 0 1 0 1]
       [1 0 0 0 1]
       [1 0 0 0 1]]})

(defn letter [char x y-offset z-offset material]
  (blocks (letter-pattern->cords (get letter-patterns char) x y-offset z-offset material)))

(defn word [word x z material]
  (doseq [[idx c] (map vector (range) (map keyword (drop 1 (clojure.string/split word #""))))]
    (letter c x z (* 6 idx) material)))

(defn circle
  ([size thing]               (circle size -1 -1 thing :flat))
  ([size y thing]             (circle size y y thing :flat))
  ([size y thing orientation] (circle size y y thing orientation) )
  ([size y z thing orientation]
     (let [mid (int (/ size 2))
           neg-mid (- 0 mid)
           cords (case orientation
                   :upright (let [top    (map (fn [s] [y (+ z mid) s])     (range neg-mid (- size mid)))
                                  bottom (map (fn [s] [y (+ z neg-mid) s]) (range neg-mid (- size mid)))
                                  left   (map (fn [s] [y (+ z s) mid])    (range neg-mid (- size mid)))
                                  right  (map (fn [s] [y (+ z s) neg-mid]) (range neg-mid (- size mid)))]
                              (distinct (apply concat top bottom left right [])))
                   :flat (let [top    (map (fn [s] [mid y s])     (range neg-mid (- size mid)))
                               bottom (map (fn [s] [neg-mid y s]) (range neg-mid (- size mid)))
                               left   (map (fn [s] [s y mid])    (range neg-mid (- size mid)))
                               right  (map (fn [s] [s y neg-mid]) (range neg-mid (- size mid)))]
                           (distinct (apply concat top bottom left right []))))]
       (blocks cords thing)
       cords)))

;;(circle 5 5 :gold_block :upright)

(defn diamond
  [material]
  (blocks (pattern->cords [[0 1 0]
                           [1 0 1]
                           [0 1 0]] 3 material)))

(defn corners [material]
  (blocks (pattern->cords [[1 0 1]
                           [0 0 0]
                           [1 0 1]] 3 material)))


(defn star
  "3d star"
  ([mat] (star 7 5 mat))
  ([x y mat]
     (blocks [[(- x 1) y 0] [(- x 2) (- y 1) 0]  [(- x 1) (- y 1) 0] [x (- y 1) 0] [(- x 1) (- y 1) -1] [(- x 1) (- y 1) 1] [(- x 1) (- y 2) 0]] mat)))

(def swirl-state {:x (atom 0) :y (atom 3) :z (atom 0) :size (atom 4) :dir (atom :forward) :material (atom :gold_block)})

(defn reset-swirl! []
  (reset! (swirl-state :x) 0)
  (reset! (swirl-state :y) 3)
  (reset! (swirl-state :z) 0)
  ;;(reset! (swirl-state :size) 4)
  (reset! (swirl-state :dir) :forward))

(defn swirl-cords
  ([material its]
     (loop [cords []]
       (if (>= (count cords) its)
         cords

         (let [{ x :x y :y z :z dir :dir size :size mat :material} swirl-state
               m @(:size swirl-state)
               offset (int (/ @(:size swirl-state) 2))]
           (when (and (= @x 0) (= 0 @z) (= @dir :forward)) (reset! dir :forward))
           (when (and (= @x m) (= 0 @z)) (reset! dir :right))
           (when (and (= @x m) (= m @z)) (reset! dir :back))
           (when (and (= @x 0) (= m @z)) (reset! dir :left))

           (when (and (= @x 0) (= 0 @z) (not= @dir :forward))
             (reset! dir :forward))

           (swap! y + 2)

           (when (>= @y 130)
             (reset-swirl!)
             (reset! (swirl-state :material) (choose block-material))
             (swap! size + 2))
           (when (>= @size 25)
             (reset! size 4))

           (case @dir
             :forward (swap! x inc)
             :back    (swap! x dec)
             :left    (swap! z dec)
             :right   (swap! z inc))
           (recur (concat cords [[(- @x offset) @y (- @z offset) @mat]])))))))

(reset-swirl!)
(reset! (:size swirl-state) 3)

(defn paint-swirl
  ([mat] (paint-swirl mat 1))
  ([mat its]
     (let [cords (swirl-cords mat its)]
       (blocks cords)
       cords)))

(def spiral-state {:x (atom 0)
                   :y (atom 3)
                   :z (atom 0)
                   :size (atom 10)
                   :dir (atom :forward)
                   :material (atom :diamond_block)})

(defn reset-spiral! []
  (def start-player-loc (.getLocation player))
  (reset! (:x spiral-state) 0)
  (reset! (:z spiral-state) 0)
  (reset! (:y spiral-state) 3)
  (reset! (:size spiral-state) 10)
  (reset! (:dir spiral-state) :forward))

(defn spiral-cords
  ([material growth-fn iterations]
     (loop [cords []]
       (if (>= (count cords) iterations)
         cords
         (let [{ x :x y :y z :z dir :dir size :size mat :material } spiral-state
               m @size
               offset (int (/ @size 2))]
           (when (and (= @x 0) (= 0 @z) (= @dir :forward)) (reset! dir :forward))
           (when (and (= @x m) (= 0 @z)) (reset! dir :right))
           (when (and (= @x m) (= m @z)) (reset! dir :back))
           (when (and (= @x 0) (= m @z)) (reset! dir :left))

           (when
               (and (= @x 0) (= 0 @z) (not= @dir :forward))
             (swap! y + 4)
             (growth-fn)
             (reset! dir :forward))

           (when (<= @size 1)
             (reset-spiral!)
             (if (= :sand @mat)
               (reset! mat material)
               (reset! mat :sand)))

           (case @dir
             :forward (swap! x inc)
             :back    (swap! x dec)
             :left    (swap! z dec)
             :right   (swap! z inc))
           (recur (conj cords [(- offset @x) @y (- offset @z) @mat])))))))

(defn paint-spiral
  ([material]           (paint-spiral material #(swap! (:size spiral-state) + 4) 1))
  ([material growth-fn] (paint-spiral growth-fn 1))
  ([material growth-fn iterations]
     (fixed-blocks (spiral-cords material growth-fn iterations))))

(comment
  (reset-spiral!)
  (dotimes [i 100]
    (paint-spiral :dirt)
    )
  )

(def triangle-state
  {:x (atom 0)
   :y (atom 3)
   :z (atom 0)
   :size (atom 10)
   :dir (atom :forward)
   :material (atom :grass)})

(defn reset-triangle! []
  (def start-player-loc  (.getLocation player))
  (reset! (:x triangle-state) 0)
  (reset! (:z triangle-state) 0)
  (reset! (:y triangle-state) 3)
  (reset! (:size triangle-state) 10)
  (reset! (:dir triangle-state) :forward))

(defn triangle-cords [material growth-fn iterations]
  (loop [cords []]
    (if (>= (count cords) iterations)
      cords

      (let [{ x :x y :y z :z dir :dir size :size mat :material} triangle-state
            m @size
            offset (int (/ @size 2))]

        (when (and (= @x 0) (= 0 @z) (= @dir :forward)) (reset! dir :forward))
        (when (and (= @x m) (= m @z)) (reset! dir :back))
        (when (and (= @x 0) (= m @z)) (reset! dir :left))

        (when
            (and (= @x 0) (= 0 @z) (not= @dir :forward))
          (swap! y + 4)
          (growth-fn)
          (reset! dir :forward))

        (when (<= @size 1)
          (reset-triangle!)
          (if (= :sand @mat)
            (reset! mat material)
            (reset! mat :sand)))

        (case @dir
          :forward (do (swap! x inc) (swap! z inc))
          :back    (swap! x dec)
          :left    (swap! z dec))
        (recur (conj cords [(- (- offset @x) 2) @y (- offset @z) @mat]))))))

(defn paint-triangle
  ([material]           (paint-triangle material #(swap! (:size triangle-state) inc) 2))
  ([material growth-fn] (paint-triangle material growth-fn 1))
  ([material growth-fn iterations]
     (reset! (:material triangle-state) material)
     (let [cords (triangle-cords material growth-fn iterations)]
       (fixed-blocks cords)
       cords)))

;;(reset-triangle!)
;;(dotimes [_ 100] (paint-triangle :air))
;;(dotimes [i 1] (bump-player))

(def allow-bump (atom true))
(defn bump-player []
  (let [y (int (.getY (.getLocation player)))
        mat (nth (cycle [:slime_block :snow_block :ice :coal_block]) y)]
    (if (and @allow-bump (< y 190))
      (do
        (teleport 0 3 0 )
        (blocks [[0 -1 0 mat]]))
      (reset! allow-bump false))))

;;(bump-player)
;;(dotimes [_ 100] (bump-player))
;;(set-time :day)
;;(bump-player)

(defn set-time [t]
  (if-not (integer? t)
    (case t
      :sunset (.setTime active-world 12400)
      :night  (.setTime  active-world 21900)
      :day    (.setTime  active-world 0))
    (.setTime (first (bk/worlds)) t)))

(defn setup-world []
  (block-fill 100 -65 100 0 0 :bedrock)
  (block-fill 100 -65 100 -150 0 :bedrock)
  (block-fill 100 -65 100 150 0 :bedrock)
  (block-fill 100 -65 100 150 150 :bedrock)
  (block-fill 100 -65 100 150 -150 :bedrock)

  (block-fill 100 -65 100 0 -150 :bedrock)
  (block-fill 100 -65 100 -150 -150 :bedrock)
  (block-fill 100 -65 100 0 150 :bedrock)

  (block-fill 200 -64 200 :water)

  (block-fill 50 -2 50 :water)
  (block-fill 20 -2 20 0 -100 :water)
  (block-fill 20 -2 20 -100 0 :water)
  )

(defonce growth (atom 0))
)
;;(setup-world)
;; \:O
;;  █\
;;./ \.
;;->START<-
(do
  (def overpad-note (cycle (degrees [1 1 1 1 1 1 1 1
                                     1 1 1 1 3 3 3 3
                                     1 1 1 1 3 3 3 3]
                                    :major :C1)))
  (def overpad-note-inc (atom 0))
  (def trigger-g62427
    (on-beat-trigger
     8
     (fn [b]
       (synth/overpad (nth overpad-note @overpad-note-inc)
                      1.0 0.001 4)
       (swap! overpad-note-inc inc))))

  ;;(remove-beat-trigger trigger-g62427)
  ;;(remove-all-beat-triggers)
  ;;(kill synth/pad)
 )

(set-time :night)

(dotimes [_ 10] (bk/broadcast "!:&*&@£$$@£$(*:&*£££££££@:£"))
(bk/broadcast "RUN")

(def trigger-g62426 (on-beat-trigger 64 #(do (set-time (rand-int 1000000)))))
(remove-beat-trigger trigger-g62426)
(remove-all-beat-triggers)
(remove-all-sample-triggers)

(one-time-beat-trigger 16 32 (fn [& _] (boom-s) (set-time :day)))

(def trigger-g62421
  (do
    (def walk-count (atom 4))
    (on-beat-trigger
     4 (fn [b]
         (swap! walk-count inc)
         (let [simple false
               beat (int (mod b 32))
               start (if (= 0.0 beat) 0.0 0.09)
               ]
           (case beat
             0 (do (paint-line 10 :mob_spawner)
                   (line-s :rate 1.0 :start 0.4 :end 0.42)
                   (reverb-kick-s :amp 1.1 :start start)
                   (bonus-s :rate 0.2)
                   )

             16 (let [size (nth (cycle (filter odd? (range 7 15))) @walk-count)]
                  (paint-line 10 :ice)
                  (reverb-kick-s :amp 1.0 :start start)
                  (reverb-kick-s :amp 1.0 :rate -1)
                  (when-not simple
                    (circle beat size  -1 :coal_block :upright)
                    (circle beat (dec size) -1 :fire :upright))
                  )

             (do (paint-line 10 :ice)
                 (line-s :rate 1.0 :start 0.4 :end 0.42)
                 (reverb-kick-s :start 0.09)
                 ))

           (when-not simple
             (blocks [
                      [12 -1 -3 :snow_block] [12 -1 3 :snow_block]
                      [12 0 -3 :torch] [12 0 3 :torch]

                      [9 -1 -3 :stone] [9 -1 3 :stone]

                      [6 -1 -3 :coal_block] [6 -1 3 :coal_block]
                      [6 0 -3 :torch] [6 0 3 :torch]

                      [3 -1 -3 :snow_block] [3 -1 3 :snow_block]
                      [3 0 -3 :torch] [3 0 3 :torch]
                      ])
             (life 9 0 -3 :skeleton)
             (life 9 0 3 :skeleton)))))))

(set-time :night)
(remove-beat-trigger trigger-g62421)
(remove-all-beat-triggers)
(remove-all-sample-triggers)

(def trigger-g62422
  (do
    (on-beat-trigger 8 (fn [b]
                         (reverb-kick-s)
                         (if (= 0.0 (mod b 16))
                           (word "CLOJURE"  20 (+ (rand-int 10) 25) (choose (concat block-material [ :sand])))
                           (word "OVERTONE" 20 (+ (rand-int 10) 10)  (choose (concat block-material [ :sand]))))
                         ))))
(remove-all-sample-triggers)
(remove-beat-trigger trigger-g62422)
(remove-all-beat-triggers)
(set-time :day)

(boom-s)
(explode 20 20 5)

(def trigger-g77218
  (on-beat-trigger
   16 #(do
         (add-step (nth (cycle [:grass :dirt]) (* -1 (:x @stairs))) )
         (boom-s))))

(remove-beat-trigger trigger-g77218)
(remove-all-beat-triggers)

(remove-all-sample-triggers)
(remove-all-beat-triggers)
(ctl-global-clock 8.0)

(def sub-trigger
  (do (defonce growth (atom 0))
      (defonce material-bag (cycle [:sand :stone :grass :brick :wood :dirt :wool :pumpkin :diamond_brick :gold_brick :air :coal_block :fire]))

      (sample-trigger
       [1 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0
        1 0 1 0 0 0 0 0
        0 0 0 0 0 0 0 0]
       (fn [b]
         (let [r (if (= 0.0 (mod b 32)) 0.5 0.4)]
           (if (= (nth material-bag @growth) :fire)
             (do (sample-player subby :rate 0.5 :amp 1.0)
                 (sample-player wop :rate -0.8 :amp 1.0))
             (sample-player subby :rate r)
             ))
         (swap! growth inc)
         (star (nth material-bag @growth))
         ))))

(remove-beat-trigger sub-trigger)
(remove-all-beat-triggers)
(remove-all-sample-triggers)

(def spir-trigger
  (do
    (reset! (:material spiral-state) :grass)
    (reset-triangle!)
    (sample-trigger
     [0 0 0 0 0 0 0 0
      1 0 0 0 0 0 0 0]
     (fn [b]
       (let [beat   (= 0.0 (- (mod b 64) 8))
             r     (if beat 1.01 0.99)
             start (if beat 0.0 (ranged-rand 0.02 0.05))
             a     (if beat 1.0 0.99)]
         (snare :rate r :amp a :start start))
       ;;(paint-swirl :stone 100)

       (paint-triangle :grass #(swap! (:size triangle-state) + 2) (* 2 @(:size spiral-state)))
       (when (> 75 (rand-int 100))
         (paint-spiral  :stone #(swap! (:size spiral-state) inc)  (* 2 @(:size spiral-state)))
         )
       ))))

(remove-beat-trigger spir-trigger)
(remove-all-beat-triggers)
(remove-all-sample-triggers)

(def ring-trigger
  (on-beat-trigger
   32
   (fn [b]
     (let [r (if (= 0.0 (mod b 128)) 0.9 1.0)]
       (ring-hat :amp 0.2 :rate r))
     (circle (inc (mod @growth 20)) (choose [:sand]))
)))

(remove-beat-trigger ring-trigger)
(remove-all-sample-triggers)

(def sub2-trigger
  (on-beat-trigger
   (* 8 2)
   (fn [b]
     (let [s (if (= 0.0 (mod b 16)) 0.0 0.1)]
       (highhat :rate 1.0 :start s))

     ;;(life 2 5 2 :pig)
     (bump-player)
     ;;(blocks [[5 5 0 :diamond_block][5 4 0 :gold_block][5 2 0 :coal_block]])
)))

(remove-beat-trigger sub2-trigger)
(remove-all-beat-triggers)
(remove-all-sample-triggers)

;;(destroyer-of-worlds)
(block-fill 10 0 10 :grass)

(def trigger-g62425  (on-beat-trigger 32 #(do
                                            (storm true)
                                            (light-s)
                                            (w/lightning (.getLocation player)))))
(storm false)

;;mv create clojure normal -g EmptyWorldGenerator
