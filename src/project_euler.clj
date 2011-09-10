(ns project-euler)
(set! *warn-on-reflection* true)
; Non-idiomatic clojure at its worst

(defn add-multiple [first-factor second-factor acc-first acc-second sum limit]
  (let [[next-first next-second next-add]
        (if (< acc-first acc-second)
          [(+ acc-first first-factor) acc-second acc-first]
          (if (== acc-first acc-second)
            [(+ acc-first first-factor) (+ acc-second second-factor) acc-first]
            [acc-first (+ acc-second second-factor) acc-second]))]
;    (prn (format "%d %d %d %d %d %d" first-factor second-factor acc-first acc-second sum limit))
    (if (<= limit next-add)
      sum
      (recur first-factor second-factor next-first next-second (+ next-add sum) limit))))

; Uninspired brute-force for now, until i get the hang of clojure
(defn sum-multiple-below-limit [first-factor second-factor limit]
  "The sum of all the multiples of first-factor or second-factor below limit"
  (add-multiple first-factor second-factor first-factor second-factor 0 limit))

; Again, no finesse. Need to learn more of the language before this gets cleaned up.
(defn add-even-fibonacci [first-num second-num sum limit]
  "Adds even numbers in a fibonacci sequence up to a limit"
;  (prn (format "%d %d" first-num sum))
  (let [next-add (+ first-num second-num)]
    (if (>= next-add limit)
      sum
      (recur second-num next-add
             (if (== (mod next-add 2) 0) (+ sum next-add) sum)
             limit))))

; More idiomatic clojure, but runs slower (~4x)
(def fib-seq
  (lazy-cat [0 1]
            (map + fib-seq (rest fib-seq))))

(defn fib [prior-2 prior limit]
  (when (< prior-2 limit)
    (lazy-seq (cons prior-2 (fib prior (+ prior-2 prior) limit)))))

(defn add-even-fib [limit]
  (reduce +
          (take-nth 3
                    (fib 0 1 limit))))

(defn -main [& options]
  ; Problem 1
  (time (prn (sum-multiple-below-limit 3 5 1000)))
  ; Problem 2 - imperative clojure
  (time (prn (add-even-fibonacci 0 1 0 4000000)))
  ; Problem 2 - slightly more idiomatic
  (time (prn (add-even-fib 4000000))))