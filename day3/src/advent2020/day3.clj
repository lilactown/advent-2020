(ns advent2020.day3
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))


(def test-input
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")


(def TREE \#)


(defn input->grid
  ([input]
   (vec
    (for [line (string/split-lines input)]
      (vec line)))))


(input->grid test-input)
;; => [[\. \. \# \# \. \. \. \. \. \. \.] [\# \. \. \. \# \. \. \. \# \. \.] [\. \# \. \. \. \. \# \. \. \# \.] [\. \. \# \. \# \. \. \. \# \. \#] [\. \# \. \. \. \# \# \. \. \# \.] [\. \. \# \. \# \# \. \. \. \. \.] [\. \# \. \# \. \# \. \. \. \. \#] [\. \# \. \. \. \. \. \. \. \. \#] [\# \. \# \# \. \. \. \# \. \. \.] [\# \. \. \. \# \# \. \. \. \. \#] [\. \# \. \. \# \. \. \. \# \. \#]]

(defn move
  [grid x y]
  (let [grid' (-> (split-at y grid) ;; move down
                  (second)
                  (vec))]
    (mapv (fn [row]
            (let [[behind-us row'] (split-at x row)]
              (into (vec row') behind-us)))
          grid')))


(-> (input->grid test-input)
    (move 3 1)
    (move 3 1)
    (move 3 1)
    (move 3 1)
    (move 3 1)
    (move 3 1)
    (move 3 1)
    (move 3 1)
    (move 3 1)
    (move 3 1))
;; => [[\# \. \# \. \# \. \. \# \. \. \.]]

(defn part-1
  [grid x y]
  (loop [grid grid
         trees-hit 0]
    (if-some [row (first grid)]
      (recur (move grid x y)
             (if (= TREE (first row))
               (inc trees-hit)
               trees-hit))
      trees-hit)))


(part-1 (input->grid test-input) 3 1)
;; => 7

(def my-grid (input->grid (slurp (io/resource "advent2020/day3-input"))))

(part-1 my-grid 3 1)
;; => 178


(defn part-2
  [grid]
  (* (part-1 grid 1 1)
     (part-1 grid 3 1)
     (part-1 grid 5 1)
     (part-1 grid 7 1)
     (part-1 grid 1 2)))


(part-2 (input->grid test-input))
;; => 336

(part-2 my-grid)
;; => 3492520200