(ns sbrian.combinatorics.core
  (:use (clojure.contrib
         [condition
          :only (handler-case print-stack-trace raise *condition*)])))

(defn factorial
  ([n acc] (if (< n 2) acc (recur (dec n) (* acc n))))
  ([n] (factorial n 1)))

(defn falling-factorial [n p]
  (/ (factorial n) (factorial (- n p))))

(defn n-choose-k [n k]
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

(defn chance-that-on-n-choose-k-avoid-t-items [n k t]
  (/ (falling-factorial (- n k) t) (falling-factorial n t)))


(defn chance-that-on-n-choose-k-avoid-exactly-s-of-t-items [n k t s]
  "This one is still wrong"
  (* (n-choose-k t s) (chance-that-on-n-choose-k-avoid-t-items [n k s])))





