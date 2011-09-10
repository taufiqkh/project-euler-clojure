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
(defn fib [prior-2 prior limit]
  (when (< prior-2 limit)
    (lazy-seq (cons prior-2 (fib prior (+ prior-2 prior) limit)))))

(defn add-even-fib [limit]
  (reduce +
          (take-nth 3
                    (fib 0 1 limit))))

;======================= Ignore above, doesn't work =======================
; For Sieve of Atkin
(def first-odd-squarefree-filter #{1 13 17 29 37 41 49 53})
(def second-odd-squarefree-filter #{7 19 31 43})
(def third-odd-squarefree-filter #{11 23 47 59})

(defn determine-prime [primes remainder]
  (if (empty? remainder)
    primes
    (let [n (first remainder)
          next-primes (if (nil? (some #(= 0 (rem n %)) primes)) (conj primes n) primes)]
      (recur next-primes (rest remainder)))))

(defn find-primes [limit]
  (let [primes (sorted-set 2 3 5) numbers (range 1 limit)]
    (determine-prime primes numbers)))
;======================= Ignore above, doesn't work =======================

; Find factors of a number excluding that number, in order.
(defn find-factors [number]
  (let [numbers (range 1 (with-precision 1 :rounding FLOOR (Math/sqrt number)))]
    (filter #(= 0 (mod number %)) numbers)))

; Naive test of primality
(defn is-prime? [number]
  (empty? (rest (find-factors number))))

(defn largest-prime-factor [number]
  (let [factors (find-factors number)]
      (last (filter #(is-prime? %) (find-factors number)))))

(defn -main [& options]
  ; Problem 1
  (time (prn (sum-multiple-below-limit 3 5 1000)))
  ; Problem 2 - imperative clojure
  (time (prn (add-even-fibonacci 0 1 0 4000000)))
  ; Problem 2 - slightly more idiomatic
  (time (prn (add-even-fib 4000000)))
  ; Problem 3 - largest prime factor of 600851475143
  (time (prn (largest-prime-factor 600851475143)))
  )