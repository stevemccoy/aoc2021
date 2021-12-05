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

(def day3-test-data
  '( "00100"  "11110"    "10110"    "10111"    "10101"
     "01111"  "00111"    "11100"   "10000"    "11001"
     "00010"  "01010"
    )
  )

(defn bit-vector [bit-string]
  (vec (map #(if (= % \1) 1 0) (map char (.getBytes bit-string)))))

(defn vector-sum [v1 v2]
  (vec (map #(+ %1 %2) v1 v2)))

(defn bit-vector-complement [v1]
  (vec (map #(if (= % 1) 0 1) v1)))

(defn bit-vector-value-rec [sofar bits]
  (if (empty? bits)
    sofar
    (bit-vector-value-rec (+ (* sofar 2) (first bits)) (rest bits))
    ))

(defn bit-vector-integer-value [v1]
  (bit-vector-value-rec 0 v1))

(defn day3-part1 []
  (with-open [rdr (io/reader "input/day3.txt")]
    (let [lines (line-seq rdr)
          num-lines (/ (count lines) 2)]
      (let [totals (->> lines
                        (map bit-vector)
                        (reduce vector-sum))]
        (let [gamma-bits (map #(if (> % num-lines) 1 0) totals)
              epsilon-bits (bit-vector-complement gamma-bits)
              gamma (bit-vector-integer-value gamma-bits)
              epsilon (bit-vector-integer-value epsilon-bits)]
          (doall ((println "Day 3 Part 1.")
                 (println "Gamma = " gamma)
                 (println "Epsilon  = " epsilon)
                 (println "Power = " (* gamma epsilon))))
          )
        )
      )
    )
  )

(defn select-bits-based-on
  "Reduce bit vector set according to value of indexed bit, for appropriate rating (0 = O2, 1 = CO2)"
  [bit-vectors idx rating-reqd]
  (let [num-vecs (/ (count bit-vectors) 2)
        num-ones (reduce + (map #(get % idx) bit-vectors))
        select-by (if (= rating-reqd 0)
                    (if (>= num-ones num-vecs) 1 0)
                    (if (< num-ones num-vecs) 1 0))]
    (filter #(= (get % idx) select-by) bit-vectors)
    )
  )

(defn find-o2-rating [bit-vectors]
  (loop [idx 0
         vectors bit-vectors]
    (let [num-vectors (count vectors)]
      (if (> num-vectors 1)
        (recur (inc idx) (select-bits-based-on vectors idx 0))
        (bit-vector-integer-value (first vectors))
        )
        )
    )
  )

(defn find-co2-rating [bit-vectors]
  (loop [idx 0
         vectors bit-vectors]
    (let [num-vectors (count vectors)]
      (if (> num-vectors 1)
        (recur (inc idx) (select-bits-based-on vectors idx 1))
        (bit-vector-integer-value (first vectors))
        )
        )
    )
  )

(defn day3-part2 []
  (with-open [rdr (io/reader "input/day3.txt")]
    (let [bit-vectors (->> (line-seq rdr) (map bit-vector))]
      (let [o2-rating (find-o2-rating bit-vectors)
            co2-rating (find-co2-rating bit-vectors)
            life-support-rating (* o2-rating co2-rating)]
        (doall ((println "Day3 Part 2.")
                (println "Oxygen rating = " o2-rating)
                (println "CO2 rating = " co2-rating)
                (println "Life support rating = " life-support-rating)))
        )
      )
    )
  )
