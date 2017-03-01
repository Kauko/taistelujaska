(ns taistelujaska.core
  (:require [taistelujaska.api :as api]
            [taistelujaska.pathing :as path]
            [clojure.string :as str]
            [astar.core :as astar]))

(def maze [[:a :b :c]
           [:d :e :f]
           [:g :h :i]])

(def graph {:a [:b :d]
            :b [:c :a :e]
            :c [:b :f]
            :d [:a :e :g]
            :e [:d :b :f :h]
            :f [:e :c :i]
            :g [:d :h]
            :h [:g :e :i]
            :i [:f :h]})

(defn board-as-numbers [board-str]
  (->> (str/split-lines board-str)
       (mapv (fn [str]
              (mapv
                (fn [ch]
                  (case ch
                    \. path/ground
                    \# path/wall
                    \e path/enemy
                    \$ path/money
                    \: path/spawn
                    \@ path/player))
                str)))))

(defn index-of [char board]
  (assert (= 1 (count (into #{} (map count board)))))
  (let [cells (apply concat board)
        index (.indexOf cells char)
        row-length (count (first board))]
    [(mod index (count (first board)))
     (/ index (count (first board)))]))

(defn indexes-of [char board]
  (assert (= 1 (count (into #{} (map count board)))))
  (let [board-with-idx (remove
                         (fn [[_ cells]]
                           (empty? cells))
                         (map-indexed
                          (fn [idx row]
                            [idx (remove nil? (map-indexed
                                    (fn [idx cell]
                                      (when (= char cell) idx))
                                    row))])
                          board))]
    (mapcat
      (fn [[row cells]]
        (partition 2 (interleave (repeat row) cells)))
      board-with-idx)))

(def player-pos (partial index-of path/player))

(defn nearest-gold [start board]
  (let [golds (into {} (map (fn [coord] [(path/manhattan-distance start coord) (vec coord)]) (indexes-of path/money board)))]
    (get golds (apply min (keys golds)))))

(defn shorten-distance [start [x y :as end]]
  (let [dist (path/manhattan-distance start end)
        max-dist 5
        lessener (int (/ dist max-dist))]
    (if (> lessener 1)
      [(int (/ x lessener)) (int (/ y lessener))]
      end)))

(defn aloita []
  (api/reset)
  (let [board (board-as-numbers (:body (api/add-player)))
        foo (some #(contains? % path/player) board)
        player (player-pos board)
        path (path/search board player (nearest-gold player board))
        direction (fn [[xc yc] [xn yn]]
                    (cond
                      (> xc xn) :south
                      (< xc xn) :noth
                      (> yc yn) :east
                      (< yc yn) :west))]
    (map direction path)))