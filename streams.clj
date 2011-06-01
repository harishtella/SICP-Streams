; two kinds of infinite sequences
; infinite seq from producer function - an iteration
; infinite seq from implicit definition - recursive data


; lazy seqs defined in terms of a producer function

; generating fibs
(defn i-enum [x] 
  (lazy-seq 
    (cons x (i-enum (+ x 1)))))

(defn fib-gen [a b]
  (lazy-seq
    (cons a (fib-gen b (+ a b)))))

(def fibs (fib-gen 0 1))

(take 10 fibs)




;generating primes
(defn integers [x]
  (lazy-seq
    (cons x (integers (inc x)))))

(defn divisible-by [y]
  (fn [x] (= (rem x y) 0)))

(defn not-divisible-by [y] 
  (comp not (divisible-by y)))

(defn sieve [x rest-of-ints] 
  (filter (not-divisible-by x) rest-of-ints)) 

(defn eros [x]
  (lazy-seq 
    (let [next-prime (first x)
          rest-of-ints (rest x)]
      (cons 
        next-prime 
        (eros (sieve next-prime rest-of-ints)))))) 

(def primes (eros (integers 2)))

(take 10 primes)


;lazy seqs defined implicitly 

(def ones
  (lazy-seq
    (cons 1 ones)))

(def integers 
  (lazy-seq 
    (cons 1 (map + ones integers))))

; powers of 2
; EX 3.53
(def p2
  (lazy-seq
    (cons 1 (map + p2 p2))))

(def factorials 
  (lazy-seq
    (cons 1 (map * (rest integers) factorials))))

(take 100 factorials)




; PARTIAL SUMS - trying 4 different ways
; EX 3.55

; works at top level 
(def partial-sums
  (lazy-seq
    (cons (first integers) 
          (map + (rest integers) partial-sums))))

(take 10 partial-sums)

; doesn't work inside let because of undefined recursive reference is to ps 
; what I need here is a letrec
(defn partial-sums [x]
  (let [ps (lazy-seq 
                (cons 
                  (first x) 
                  (map + (rest x) ps)))]
          ps))

; works using iterate, this is the default clojure way
(defn partial-sums [coll]
  (map first
       (iterate 
         (fn [[sum coll-rest]] [(+ sum (first coll-rest))
                                (rest coll-rest)])
         [(first coll) (rest coll)])))

; a fourth way, I like this the best.
(require 'clojure.contrib.seq)
(defn partial-sums [coll]
  (clojure.contrib.seq/rec-seq ps (lazy-cons (first coll)
                                      (map + (rest coll) ps))))

;test out partial-sums
(def pst (partial-sums integers))
(take 10 pst)





;--------
(defn estim-sqrt [x]
  (iterate 
    (fn [g]  (-> (/ x g) 
                  (+ g)
                  (/ 2)
                  (double)))
    1))

; EX 3.64
(defn stream-limit [stream tolerance] 
  (letfn [(abs [x] (if (< x 0) (* -1 x) x))
          (sl-h [stream tolerance pos]
                (if (< (abs 
                         (- (first stream) 
                            (second stream)))
                       tolerance)
                  [(second stream) pos]
                  (sl-h (rest stream) tolerance (inc pos))))]
    (sl-h stream tolerance 1)))

(stream-limit (estim-sqrt 227) 0.001)

; EX 3.76
(defn smooth [stream]
  (map (fn [x y] (/ (+ x y) 2)) stream (cons 0 stream)))
(defn sign-change-detector [a b]
  (cond (and (neg? a) (pos? b)) -1
        (and (pos? a) (neg? b)) 1
        :else 0))
(defn zero-crossings [sense-data]
  (let [smooth-sd (smooth sense-data)]
    (map sign-change-detector smooth-sd (cons 0 smooth-sd)))) 
(def x (list 0 10 -1 -10 1 7))
(def z (zero-crossings x))


; EX 3.60
(defn mul-series [s1 s2]
  (let [row (map (fn [x] (* (first s1) x)) s2)]
    (lazy-seq 
      (cons (first row) (map + (rest row)
                           (mul-series (rest s1) s2))))))
