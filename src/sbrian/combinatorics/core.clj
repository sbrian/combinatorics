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
   to exactly 0 of 2000, which covers every possibility and gives 100%"
  
  (reduce + (map #(float (chance-that-on-n-choose-k-avoid-s-of-t 20000 1000 2000 %))
                 (range 1880 1921)))
  "This calculates the chance that when picking 1000 from 20000, I will get 80 to 120
   from a group of 2000, putting me at 2% margin of error.  Result 0.97353363"
  
  (reduce + (map #(float (chance-that-on-n-choose-k-avoid-s-of-t 20000 500 2000 %))
                 (range 1940 1961)))
  "This calculates the chance that when picking 500 from 20000, I will get 40 to 60
   from a group of 2000, putting me at 2% margin of error.  Result 0.8877281"
 
  (reduce + (map #(float (chance-that-on-n-choose-k-avoid-s-of-t 20000 100 2000 %))
                 (range 1988 1993)))
  "This calculates the chance that when picking 100 from 20000, I will get 8 to 12
   from a group of 2000, putting me at 2% margin of error.  Result 0.59694666"
  
  (reduce + (map #(float (chance-that-on-n-choose-k-avoid-s-of-t 20000 200 2000 %))
                 (range 1972 1989)))
  "This calculates the chance that when picking 200 from 20000, I will get 12 to 28
   from a group of 2000, putting me at 4% margin of error.  Result 0.9571556"
  
  (reduce + (map #(float (chance-that-on-n-choose-k-avoid-s-of-t 20000 200 2000 %))
                 (range 1970 1991)))
  "This calculates the chance that when picking 200 from 20000, I will get 10 to 30
   from a group of 2000, putting me at 5% margin of error.  Result 0.987409"
  
  (reduce + (map #(float (chance-that-on-n-choose-k-avoid-s-of-t 20000 50 2000 %))
                 (range 1993 1998)))
  "This calculates the chance that when picking 50 from 20000, I will get 3 to 7
   from a group of 2000, putting me at 4% margin of error.  Result 0.76669806"
  
)


