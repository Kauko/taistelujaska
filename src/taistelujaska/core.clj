(ns taistelujaska.core
  (:require [taistelujaska.api :as api]
            [taistelujaska.pathing :as path]
            [clojure.string :as str]
            [clojure.core.async :refer [put! >! <! timeout chan go go-loop]]
            [clojure.math.numeric-tower :as math]
            [astar.core :as astar]))

(def player \@)
(def wall \#)
(def ground \.)
(def enemy \e)
(def gold \$)
(def other-player \P)
(def spawn \:)

(def traversable-cell-types #{ground gold other-player spawn player})

(defn traversable? [[coord char]]
  (traversable-cell-types char))

(defn board [board-str]
  (->> board-str
       str/split-lines
       (map-indexed
         (fn [row-index row]
           (map-indexed
             (fn [cell-index cell]
               [[cell-index row-index] cell])
             row)))
       (mapcat identity)
       (into {})))

(defn corner? [[x y] [tx ty]]
  (and (or (= tx (- x 1))
           (= tx (+ x 1)))
       (or (= ty (- y 1))
           (= ty (+ y 1)))))

(defn cells-next-to
  ([cell] (cells-next-to cell nil))
  ([[x y :as cell] board]
   (let [width (when board (apply max (map first (keys board))))
         height (when board (apply max (map second (keys board))))]
     (for [tx (range (- x 1) (+ x 2))
           ty (range (- y 1) (+ y 2))
           :when (and (>= tx 0)
                      (>= ty 0)
                      (or (nil? width) (<= tx width))
                      (or (nil? height) (<= ty height))
                      (not= [x y] [tx ty])
                      (or (nil? board) (traversable-cell-types (board [tx ty])))
                      (not (corner? [x y] [tx ty])))]
       [tx ty]))))

(defn graph [board]
  (let [traversable-cells (filter traversable? board)]
    (into {}
          (map
            (fn [[[x y :as cell] _]]
              [cell (cells-next-to cell board)])
            traversable-cells))))

(defn indices-of [char board]
  (keys (filter (comp (partial = char) val) board)))

(defn index-of [char board]
  (some (fn [[coord cell]] (when (= char cell) coord)) board))

(def player-pos (partial index-of player))

(defn nearest-goal [start goals]
  (let [distances (into {} (map (fn [goal] [(path/manhattan-distance start goal) goal]) goals))
        shortest-distance (when-not (empty? distances) (apply min (keys distances)))]
    (get distances shortest-distance)))

(defn opposite-direction [[posx posy] [goalx goaly]]
  [(+ posx (- posx goalx))
   (+ posy (- posy goaly))])

(defn cardinal-directions [start path]
  (loop [cardinals []
         [[cx cy :as current] [nx ny :as next] & others :as path] (into [start] path)]
    (if-not next
      cardinals
      (recur (conj cardinals (cond
                               (> nx cx) :east
                               (< nx cx) :west
                               (> ny cy) :south
                               (< ny cy) :north
                               :else nil))
             (rest path)))))

(defn weights [enemy-positions]
  (fn [from to]
    (->> enemy-positions
        (map (partial path/manhattan-distance to))
        (map (partial / 50))
        (reduce +))))

(def gold-spawns (atom #{}))
(def gold-camps (atom []))

(defn save-gold-camps! [spawns]
  (println "Calculating gold camps!")
  (let [camps (remove spawns (mapcat cells-next-to spawns))
        ranked (into
                 {}
                 (map
                   (fn [camp]
                     [camp (-> (reduce + (map (partial path/manhattan-distance camp) spawns))
                               (math/expt 2))])
                   camps))]
    (reset! gold-camps ranked)))

(add-watch gold-spawns ::gold-camps
           (fn [_ _ old new]
             (when-not (= old new) (save-gold-camps! new))))

(defn save-gold-spawns! [locations]
  (swap! gold-spawns #(apply conj % locations))
  locations)

(defn best-gold-camp []
  (let [best-rank (apply min (vals @gold-camps))
        best-camps (filter (comp (partial = best-rank) val) @gold-camps)]
    (keys best-camps)))

(defn actions []
  (let [board (board (:body (api/board!)))]
    (when-let [player-position (player-pos board)]
      (let [nearest-gold (some->> (indices-of gold board)
                                  save-gold-spawns!
                                  (nearest-goal player-position))
            nearest-gold-spawn (when-not nearest-gold
                                 (some->> (best-gold-camp)
                                          (nearest-goal player-position)))
            goal (or nearest-gold nearest-gold-spawn)
            graph (graph board)
            path (->> (astar/route graph
                                   (weights (indices-of enemy board))
                                   (partial path/manhattan-distance goal)
                                   player-position
                                   goal)
                      (cardinal-directions player-position))
            actions (->> path
                         (interleave (repeat :move))
                         (partition 2))]
        (when-not nearest-gold
          (println "No gold found! Travelling to best camp."))
        #_(println "Travelling from " (pr-str player-position) " to " (pr-str goal))
        #_(println "It takes " (count actions) " steps to get there")
        actions))))

(defn do-actions! [ms]
  (when-let [a (first (actions))]
    #_(println "Executing " (pr-str a))
    (apply api/act! a)
    (Thread/sleep ms))
  (recur ms))

(defn aloita []
  (let [ms (Integer. (:body (api/turn-duration!)))]
    (do-actions! ms)))

(defn new-round!
  ([] (new-round! 0))
  ([lvl]
   (reset! gold-camps {})
   (reset! gold-spawns #{})
   (api/reset-game! lvl)
   (api/add-player!)
   (aloita)))

(defn -main []
  (new-round!))