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

(defn -main [& options]
  ; Problem 1
  (prn (sum-multiple-below-limit 3 5 1000)))