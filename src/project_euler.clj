(ns project-euler)
; Non-idiomatic clojure at its worst

(defn add-multiple [first-factor second-factor acc-first acc-second sum limit]
  (let [[next-first next-second next-add]
        (if (< acc-first acc-second)
          [(+ acc-first first-factor) acc-second acc-first]
          (if (= acc-first acc-second)
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
(defn add-even-fibonnaci [first-num second-num sum limit]
  "Adds even numbers in a fibonacci sequence up to a limit"
;  (prn (format "%d %d" first-num sum))
  (let [next-add (+ first-num second-num)]
    (if (>= next-add limit)
      sum
      (recur second-num next-add
             (if (= (mod next-add 2) 0) (+ sum next-add) sum)
             limit))))

(defn -main [& options]
  ; Problem 1
  (prn (sum-multiple-below-limit 3 5 1000))
  ; Problem 2
  (prn (add-even-fibonnaci 0 1 0 4000000)))