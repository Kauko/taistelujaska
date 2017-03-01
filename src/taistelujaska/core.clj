(ns taistelujaska.core
  (:require [taistelujaska.api :as api]
            [taistelujaska.pathing :as path]
            [clojure.string :as str]
            [clojure.core.async :refer [put! >! <! timeout chan go go-loop]]
            [astar.core :as astar]))

(def player \@)
(def wall \#)
(def ground \.)
(def enemy \e)
(def gold \$)
(def other-player \P)
(def spawn \:)

(def traversable-cell-types #{enemy ground gold other-player spawn player})

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

(defn graph [board]
  (let [width (apply max (map first (keys board)))
        height (apply max (map second (keys board)))
        traversable-cells (filter traversable? board)
        corner? (fn [[x y] [tx ty]]
                  (and (or (= tx (- x 1))
                           (= tx (+ x 1)))
                       (or (= ty (- y 1))
                           (= ty (+ y 1)))))]
    (into {}
          (map
            (fn [[[x y :as cell] _]]
              [cell (for [tx (range (- x 1) (+ x 2))
                          ty (range (- y 1) (+ y 2))
                          :when (and (>= tx 0)
                                     (>= ty 0)
                                     (<= tx width)
                                     (<= ty height)
                                     (not= [x y] [tx ty])
                                     (traversable-cell-types (board [tx ty]))
                                     (not (corner? [x y] [tx ty])))]
                      [tx ty])])
            traversable-cells))))

(defn distances [graph goal]
  (into {} (map (fn [cell] [cell (path/manhattan-distance goal cell)]) (keys graph))))

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

(def actions-ch (chan))

(defn do-actions! [ms actions-ch]
  (go-loop []
    (let [a (<! actions-ch)]
      (println "Executing " (pr-str a))
      (apply api/act! a))
    (<! (timeout ms))
    (recur)))

(defn new-round! []
  (api/reset-game!)
  (api/add-player!))

(defn aloita []
  (let [board (board (:body (api/board!)))
        turn-duration (Integer. (:body (api/turn-duration!)))]
    (when-let [player-position (player-pos board)]
      (let [nearest-gold (->> (indices-of gold board)
                              (nearest-goal player-position))
            nearest-enemy (when-not nearest-gold
                            (->> (indices-of enemy board)
                                 (nearest-goal player-position)
                                 (opposite-direction player-position)))
            graph (graph board)
            end-goal (or nearest-gold nearest-enemy)
            path (->> (astar/route graph
                                   (constantly 1)
                                   (distances graph nearest-gold)
                                   player-position
                                   end-goal)
                      (cardinal-directions player-position))
            actions (->> path
                         (interleave (repeat :move))
                         (partition 2))]
        (println "Travelling from " (pr-str player-position) " to " (pr-str end-goal))
        (println "It takes " (count actions) " steps to get there")
        (do-actions! (/ turn-duration 2) actions-ch)
        (for [action actions]
          (put! actions-ch action))))))

(defn -main []
  (new-round!)
  (aloita))