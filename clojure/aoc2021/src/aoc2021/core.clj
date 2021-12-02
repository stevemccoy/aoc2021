(ns aoc2021.core)
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

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

(defn sum-by-verb [instructions verb]
  (->> instructions
       (filter #(= (first %) verb))
       (map #(second %))
       (reduce +)))

(defn day2-part1 []
  (with-open [rdr (io/reader "input/day2.txt")]
    (let [raw (map #(str/split % #" ") (line-seq rdr))]
      (let [parsed (map #(list (first %) (Integer/parseInt (second %))) raw)]
        (let [horizontal (sum-by-verb parsed "forward")
              vertical (- (sum-by-verb parsed "down")
                          (sum-by-verb parsed "up"))
              product (* horizontal vertical)]
          (doall ((println "Day2, Part 1. Ending Position:" )
                  (println "Horizontal = " horizontal)
                  (println "Vertical = " vertical)
                  (println "Product = " product)
                  product))
          )
        )
      )
    )
  )

(defn calculate-aims [instructions start]
  (if (empty? instructions)
    ()
    (let [verb (ffirst instructions)
          amount (second (first instructions))]
      (let [new-aim (cond (= verb "down") (+ start amount)
                          (= verb "up") (- start amount)
                          :else start)]
        (cons new-aim (calculate-aims (rest instructions) new-aim))
        )
      )
    )
  )

(defn day2-part2 []
  (with-open [rdr (io/reader "input/day2.txt")]
    (let [raw (map #(str/split % #" ") (line-seq rdr))]
      (let [parsed (map #(list (first %) (Integer/parseInt (second %))) raw)]
        (let [aims (calculate-aims parsed 0)]
          (let [deltas (map #(list (if (= (first %1) "forward") (second %1) 0) %2) parsed aims)]
            (let [horizontal (reduce + (map #(first %) deltas))
                  vertical (reduce + (map #(* (first %) (second %)) deltas))
                  product (* horizontal vertical)]
              (doall ((println "Day2, Part 2. Ending Position:" )
                      (println "Horizontal = " horizontal)
                      (println "Vertical = " vertical)
                      (println "Product = " product)))
              )
            )
          )
        )
      )
    )
  )



