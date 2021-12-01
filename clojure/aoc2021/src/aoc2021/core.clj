(ns aoc2021.core)
(require '[clojure.java.io :as io])

(defn count-increases
  "Count the number of depth samples which are increases from the previous sample"
  [depths]
  (if (< (count depths) 2) 0
    (+ (if (> (second depths) (first depths)) 1 0)
       (count-increases (rest depths)))))

(defn window3-sums [depths]
  (if (< (count depths) 3)
    ()
    (cons (reduce + (list (first depths) (second depths) (nth depths 2)))
          (window3-sums (rest depths)))))

(defn day1-part1 []
  (with-open [rdr (io/reader "input/day1.txt")]
    (println "Number of depth increases = "
             (count-increases (map #(Integer/parseInt %) (line-seq rdr)))
             )))

(defn day1-part2 []
  (with-open [rdr (io/reader "input/day1.txt")]
    (println "Number of 3-window sum increases = "
             (count-increases (window3-sums (map #(Integer/parseInt %) (line-seq rdr))))
             )))
