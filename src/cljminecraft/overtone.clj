(ns cljminecraft.overtone
  "Music and Minecraft."
  (:import [org.bukkit Location Material])
  (:use [overtone.core] [mud.core] [mud.timing])
  (:require [cljminecraft.bukkit :as bk]
            [cljminecraft.blocks :as b]
            [cljminecraft.world :as w]
            [cljminecraft.entity :as e]
            [cljminecraft.items :as i]))

;;(connect-external-server)

(do
  (def block-material
    (->>
     (keys i/materials)
     (filter #(re-find #"^[\w]*_block$" (str (name %1))))
     (remove #(some #{%1} [:cake_block :iron_door_block :sugar_cane_block :bed_block])))
    )
  (def ore-material (filter #(re-find #"_ore" (str (name %1))) (keys i/materials)))

  (do
    (def big-reverb-kick-s (freesound 219515))
    (def reverb-kick-s (freesound 90243))

    (def line-s (freesound 144919))
    (def bonus-s (freesound 55570))

    (def highhat (freesound 53532))
    (def dirty-kick (freesound 30669))
    (def ring-hat (freesound 12912))
    (def snare (freesound 193023);;(freesound 26903)
      )
    (def click (freesound 406))
    (def wop (freesound 85291))
    (def subby (freesound 25649))
    (defonce boom-s      (freesound-sample 33637)))

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

(defn explode [x y z]
  (let [l (.getLocation player)]
    (doto l
      (.setX (+ x (.getX l)))
      (.setY (+ y (.getY l)))
      (.setZ (+ z (.getZ l))))
    (bk/ui-sync
     @cljminecraft.core/clj-plugin
     #(w/explode l 10 true))))

;;(explode 0 -0 0)

(defn blocks-around-player [size mat]
  (let [l (.getLocation player)
        grid (range (- 0 size) size)]
    (map
     #(let [b-l (.getLocation %1)]
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
                               (+ z (.getZ l))))
                   )
                 grid)
)
          (range -10 10)))
       (range -10 10))))))

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

(defn block-fill
  ([x y z mat] (block-fill x y z 0 0 mat))
  ([x y z offset mat] (block-fill x y z offset 0 mat))
  ([x y z x-offset z-offset mat]
      (blocks  (mapcat (fn [i] (map (fn [x] [(+ x-offset i) y (+ z-offset x) mat]) (range (- 0 x) x))) (range (- 0 z) z)))))

(defn blocks-absolute "absolute"
  ([actions material] (blocks (map #(if (= 3 (count %1)) (concat %1 [material])) actions)))
  ([actions]
     (bk/ui-sync
      @cljminecraft.core/clj-plugin
      (fn []
        (doseq [[x y z m] actions]
          (let [m (i/get-material m)
                l (Location. active-world x y z)]
            (doto (.getBlock l)
              (.setData 0)
              (.setType (.getItemType m))
              (.setData (.getData m)))))))))

(defn destroyer-of-worlds []
  (blocks-absolute (blocks-around-player 100 :gravel))
  ;;  (Thread/sleep 5000)
  ;;  (teleport 0 200 0)
  )

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
  ([size thing] (circle size -1 thing))
  ([size y thing]
     (let [mid (int (/ size 2))
           neg-mid (- 0 mid)
           top    (map (fn [s] [mid  y  s])    (range neg-mid (- size mid)))
           bottom (map (fn [s] [neg-mid y  s]) (range neg-mid (- size mid)))
           left   (map (fn [s] [s  y  mid])    (range neg-mid (- size mid)))
           right  (map (fn [s] [s  y neg-mid]) (range neg-mid (- size mid)))
           cords (distinct (apply concat top bottom left right []))]
       (blocks cords thing)
       cords)))

(defn diamond
  [material]
  (blocks (pattern->cords [[0 1 0]
                           [1 0 1]
                           [0 1 0]] 3 material)))

(defn corners [material]
  (blocks (pattern->cords [[1 0 1]
                           [0 0 0]
                           [1 0 1]] 3 material)))


(def spiral-state {:x (atom 0)
                   :y (atom 3)
                   :z (atom 0)
                   :size (atom 10)
                   :dir (atom :forward)
                   :material (atom :sand)})

(reset! (:material spiral-state) :diamond_block)

(defn reset-spiral! []
  (reset! (:x spiral-state) 0)
  (reset! (:z spiral-state) 0)
  (reset! (:y spiral-state) 3)
  (reset! (:size spiral-state) 10)
  (reset! (:dir spiral-state) :forward))

(defn spiral-cords
  ([material growth-fn iterations]
     (loop [cords []]
       (if (>= (count cords) iterations)
         (do
           (println cords)
           cords)
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
  ([material]           (paint-spiral material #(swap! (:size spiral-state) inc) 1))
  ([material growth-fn] (paint-spiral growth-fn 1))
  ([material growth-fn iterations]
     (blocks (spiral-cords material growth-fn iterations))))

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
        (recur (conj cords [(- offset @x) @y (- offset @z) @mat]))))))

(defn paint-triangle
  ([material]           (paint-triangle material #(swap! (:size triangle-state) inc) 1))
  ([material growth-fn] (paint-triangle material growth-fn 1))
  ([material growth-fn iterations]
     (let [cords (triangle-cords material growth-fn iterations)]
       (blocks cords)
       cords)))

(reset-triangle!)
(dotimes [_ 100] (paint-triangle :dirt))
;;(dotimes [i 1] (bump-player))

(defn bump-player []
  (when
      (< (.getY (.getLocation player))
         200)

    (teleport 0 3 0 )
    (blocks [[0 -1 0]] :grass)))

(bump-player)
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
)

(set-time :day)

(bk/broadcast "[:overtone :clojure :minecraft]")
(bk/broadcast "(do)")

(bump-player)

(one-time-beat-trigger 32 64 (fn [& _]
                                 (boom-s)
                                 (set-time :day)
                                 ))

(pattern->cords [[0 1 0]
                 [1 0 1]
                 [0 1 0]] 3 :dirt)

(block -1 -1 1 :dirt)
(circle 3 3 :sand)
(circle 5 3 :sand)
(circle 8 3 :sand)

(circle 8 -1 :sand)

(circle 3 :sand)
(diamond :air)
(diamond :sand)
(corners :sand)

(block 0 -1 0 :stone)

(doseq [i (range 10)] (add-step :dirt))

(comment
  (loop [x -1
         y -1]
    (block x y 0 :air)
    (when (> x -10)
      (recur (dec x) (inc y))))
  )

(set-time :day)

(blocks [[-5 -1 0]
         [-5 -1 1]
         [-6 -1 1]
         [-6 -1 0]] :dirt)

;;(.setY (:origin ctx) 70)

(ctl-global-clock 8.0)

(def cell-size (atom 1))
(def growth (atom 0))
(def material-bag (cycle [:sand :stone :grass :brick :wood :dirt :wool :pumpkin :skull :air :stationary_water :water :lava]))
(def instructions [(b/pen-up)
                   (b/up 3)
                   (b/forward 1)
                   (b/right 4)
                   (b/pen-down)
                   (b/back 1)
;;                   (b/up 2)
;;                   (b/forward 4)
;;                   (b/down  3)


;;                   (b/left (rand-int 10))
  ;;                 (b/right (rand-int 10))
    ;;               (b/up (rand-int 10))
      ;;             (b/right (rand-int 10))
        ;;           (b/down (rand-int 10))
                   ])

(reset! cell-size 20)

(def trigger-g77218
  (on-beat-trigger 16 #(do
                        (add-step (nth (cycle [:grass :dirt]) (* -1 (:x @stairs))) )
                        (boom-s)

                       )))

(remove-beat-trigger trigger-g77218)
(remove-all-beat-triggers)

(def sub-trigger
  (sample-trigger
   [1 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    1 0 1 0 0 0 0 0
    0 0 0 0 0 0 0 0]
   (fn []
     (def ctx (b/setup-context player))
     (swap! growth inc)
     (if (= (nth material-bag @growth) :water)
       (do       (sample-player subby :rate 0.5 :amp 1.0)
                 (sample-player wop :rate -0.8 :amp 1.0))
       (sample-player subby :rate 0.4)
       )
     (draw (nth material-bag @growth) instructions))))

(set-time :day)

(remove-beat-trigger sub-trigger)
(remove-all-beat-triggers)

(def instr2 [(b/pen-up)
             (b/up 5)
             (b/left 3)
             (b/forward 1)
             (b/pen-down)
             (b/forward (mod @growth 10))])
(def mat2 [:sand])

(def high-trigger
  (on-beat-trigger 32  #(do
                          (bump-player)
                          (click :rate 1.0)
                          (click :rate -0.5)
                          )))

(remove-beat-trigger high-trigger)
(remove-all-beat-triggers)

(def spir-trigger
  (do
    (reset! (:material spiral-state) :grass)
    (reset! (:material triangle-state) :ice)
    (reset-spiral!)
    (sample-trigger
     [0 0 0 0 0 0 0 0
      1 0 0 0 0 0 0 0]
     (fn []
       (snare :rate 1.0)
       (if (> (rand-int 100) 50)
         (paint-triangle :stone #(swap! (:size triangle-state) inc) (* 2 @(:size spiral-state)))
         (paint-spiral  :stone #(swap! (:size spiral-state) inc)  (* 2 @(:size spiral-state))))))))

(volume 1)

(reset! (:material spiral-state) :diamond_block)
(remove-beat-trigger spir-trigger)
(remove-all-beat-triggers)
(remove-all-sample-triggers)



(bump-player)


(def ring-trigger (on-beat-trigger
        32
        (fn []
          (ring-hat :amp 0.2)
          (circle (inc (mod @growth 20)) :sand)
;;          (draw (choose mat2) instr2)
          )))

(remove-beat-trigger ring-trigger)

(set-time :day)

(remove-all-sample-triggers)

(def sub2-trigger
  (on-beat-trigger
   (* 8)
   (fn []
     (highhat :rate 1.0)

     (monster 0 0 0 :pig)
     (bump-player)
     (block 5 7 0 :dirt)
     (blocks [[5 5 0]
              [5 4 0]
              [5 2 0]] :dirt)
     (draw :dirt [(b/pen-up) (b/forward 10) (b/pen-down) (b/up 10) (b/left 1) (b/forward 10) (b/left 1) (b/forward 3) (b/left 2)]))))

(block 5 7 0 :air)

(bump-player)
(reset-spiral)
(reset! spiral-material :stone)
(set-time :day)
(remove-beat-trigger sub2-trigger)
(remove-all-beat-triggers)
(remove-all-sample-triggers)


(defn paint-line [steps mat]
  (blocks (map (fn [xy] [xy -1 0 mat]) (range steps)))
  )

(def trigger-g62421
  (on-beat-trigger
   4 (fn [b]
       (let [beat (int (mod b 16))]
         (case beat
           0 (do (paint-line 10 :mob_spawner)
                 (line-s :rate 1.0 :start 0.4 :end 0.42)
                 (reverb-kick-s :amp 1.1)
                 (bonus-s :rate 0.2))

           (do (paint-line 10 :ice)
               (line-s :rate 1.0 :start 0.4 :end 0.42)
               (reverb-kick-s :start 0.09)
               ))))))


(remove-beat-trigger trigger-g62421)
(remove-all-beat-triggers)
(remove-all-sample-triggers)

;;diamond_block ;nice glow
;;ice snow_block

(def trigger-g62422
  (do
    (on-beat-trigger 8 (fn [b]
                         (if (= 0.0 (mod b 16))
                           (word "CLOJURE"  20 (+ (rand-int 10) 25) (choose (concat block-material [:tnt :sand])))
                           (word "OVERTONE" 20 (+ (rand-int 10) 10)  (choose (concat block-material [:tnt :sand]))))
                         ;;(word "OVERTONE" 20 :air)
                         (reverb-kick-s)
                         ))))

(remove-beat-trigger trigger-g62422)
(remove-all-beat-triggers)
(set-time :day)

(explode 20 20 5)

;;:mob_spawner onfire pigs
(def block-material [:ice :snow_block :quartz_block :dimond_block])

(defn setup-world []
  (block-fill 100 -65 100 0 0 :bedrock)
  (block-fill 100 -65 100 -150 0 :bedrock)
  (block-fill 100 -65 100 150 0 :bedrock)

  (block-fill 100 -65 100 100 100 :bedrock)
  (block-fill 100 -65 100 150 -150 :bedrock)

  (block-fill 100 -65 100 0 -150 :bedrock)
  (block-fill 100 -65 100 -150 -150 :bedrock)
  (block-fill 100 -65 100 -150 150 :bedrock)

  (block-fill 100 -65 100 :water)

  (block-fill 50 -2 50 :water)
  (block-fill 20 -2 20 0 -100 :water)
  )

(setup-world)


(block 1 -1 1 :stone)
(monster 1 1 1 :pig)
(stop)
(teleport 260 260 300)

(ctl (foundation-output-group) :master-volume 1)
(stop-all)

(do (big-reverb-kick-s)
    (set-time :day))

(bump-player)

(paint-spiral :dirt)

(blocks [[3 1 0]] :brick)
(set-time :day)
;;(destroyer-of-worlds)
