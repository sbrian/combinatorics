(ns sbrian.combinatorics.core
  (:use [slingshot.slingshot :only [throw+]]))

(defn factorial
  ([n acc] (if (< n 2) acc (recur (dec n) (* acc n))))
  ([n] (factorial n 1)))

(defn falling-factorial [n p]
  (/ (factorial n) (factorial (- n p))))

(defn n-choose-k [n k]
  "Number of ways to choose k items from n items"
  (if (< n 0) (throw+ {:type ::n-must-be-postive :n n}))
  (if (< k 0) (throw+ {:type ::k-must-be-postive :k k}))
  (if (> k n) (throw+ {:type ::n-must-be-greater-than-or-equal-to-k :n n :k k}))
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

(defn chance-that-on-n-choose-k-avoid-t-items [n k t]
  (/ (falling-factorial (- n k) t) (falling-factorial n t)))

(defn n-choose-k-avoid-s-of-t [n k t s]
  "Number of ways to choose k items from n items
   where exactly s items are in a group of t items.
   Note that (= (n-choose-k t s) (n-choose-k t (- t s)))"
  (if (> (- t s) k) (throw+ {:type ::invalid-args :msg "k must be greater than t - s" :k k :t t :s s}))
  (* (n-choose-k t s)
     (n-choose-k (- n t) (- k (- t s)))))

(defn n-choose-k-avoid-at-least-s-of-t [n k t s]
  (reduce +
  (map #(n-choose-k-avoid-s-of-t n k t %) (range s (+ t 1)))))

(defn chance-that-on-n-choose-k-avoid-s-of-t [n k t s]
  "There are interesting aspects to this.
  For example, when choosing 200 items from 2000 items, the chance of avoiding
  exactly 100 out of a group of 100 is much better than the chance of avoiding
  exactly 1 out of that group of 100"
  (/ (n-choose-k-avoid-s-of-t n k t s)
     (n-choose-k n k)))

(defn chance-that-on-n-choose-k-avoid-at-least-s-of-t [n k t s]
  (/ (n-choose-k-avoid-at-least-s-of-t n k t s)
     (n-choose-k n k)))

(comment
  (float (chance-that-on-n-choose-k-avoid-at-least-s-of-t 20000 50 2000 1950))
  "This means when selecting 50 from 20000 I miss anywhere from exactly 50 of 2000
   to exactly 0 of 2000, which covers every possibility and gives 100%")


