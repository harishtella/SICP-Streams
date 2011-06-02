
(define (scar s) (car s))
(define (scdr s) (force (cdr s)))

; the nth elem of s
(define (nth s n) 
  (if (= n 0) 
      (scar s) 
      (nth (scdr s) (- n 1)))) 

; get n elems from S
(define (take n s)
  (if (= n 0)
	'()
	(cons (scar s) (take (- n 1) (scdr s)))))

; get all the elements from S
(define (take-all s)
  (if (stream-null? s)
	'()
	(cons (scar s) (take-all (scdr s)))))

(define (list->stream lst)
  (if (null? lst)
	'()
	(cons-stream (car lst) (list->stream (cdr lst)))))

(define (stream-for-each proc s) 
  (if (stream-null? s) 
      'done 
      (begin (proc (scar s)) 
             (stream-for-each proc (scdr s))))) 

(define (memo-proc proc) 
  (let ((already-run? false) (result false)) 
    (lambda () 
      (if (not already-run?) 
          (begin (set! result (proc)) 
                 (set! already-run? true) 
                 result)
           result)))) 

; TODO check if any of the input streams are null 
; not just the first one
(define (smap proc . sx) 
  (if (stream-null? (car sx))
      the-empty-stream 
	  (cons-stream (apply proc (map scar sx))
				   (apply smap (cons proc (map scdr sx))))))

(define (add-streams s1 s2) 
  (smap + s1 s2)) 

(define (mul-streams s1 s2)
		(smap * s1 s2))

(define ones (cons-stream 1 ones)) 
(define integers (cons-stream 1 (add-streams ones integers))) 

;EX 3.54
(define factorials (cons-stream 1 (mul-streams factorials (scdr integers))))

;EX 3.56
(define (scale-stream stream factor) 
  (smap (lambda (x) (* x factor)) stream)) 
(define (merge s1 s2) 
		(cond ((stream-null? s1) s2) 
		  ((stream-null? s2) s1) 
		  (else 
			(let ((s1car (scar s1))
				  (s2car (scar s2))) 
			  (cond ((< s1car s2car) 
					 (cons-stream s1car (merge (scdr s1) s2))) 
				((> s1car s2car) 
				 (cons-stream s2car (merge s1 (scdr s2)))) 
				(else 
				  (cons-stream s1car 
							   (merge (scdr s1) 
									  (scdr s2))))))))) 
(define S (cons-stream 1 (merge (scale-stream S 2)
								(merge (scale-stream S 3)
									   (scale-stream S 5)))))


;EX 3.59 a
(define (integrate-series s)
		(define (helper s ints)
				(cons-stream (/ (scar s) 
								(scar ints))
							 (helper (scdr s) (scdr ints))))
		(helper s integers))


;EX 3.59b
; wow really slick
(define cosine-series 
  (cons-stream 1 
			   (smap (lambda (x) (* x -1))
					 (integrate-series sine-series)))) 
(define sine-series 
  (cons-stream 0 
			   (integrate-series cosine-series))) 

; EX 3.60
; dont think about the recursion too much it will explode your mind
; TODO make this work for finite series
(define zeros
  (cons-stream 0 zeros))
(define (mul-series s1 s2)
  (if (stream-null? s1)
	zeros
	(let ((row (smap (lambda (x) (* x (scar s1)))
					 s2)))
	  (cons-stream (scar row) (add-streams (scdr row)
										   (mul-series (scdr s1) s2))))))
(define (sqr-series s)
		(mul-series s s))
(define should-be-one
		(add-streams
		  (sqr-series cosine-series)
		  (sqr-series sine-series)))

; EX 3.61
(define (invert-unit-series s)
  (letrec ((inv-series
			 (cons-stream 1 (smap (lambda (x) (* x -1))
								  (mul-series (scdr s) 
											  inv-series)))))
		   inv-series))

; EX 3.62
;could use some fixin, but good enough for now
(define (div-series num denom)
  (mul-series num 
			  (invert-unit-series denom)))
(define tan-series
  (div-series sine-series cosine-series))

;-----------------


(define (interleave s1 s2)
		(if (stream-null? s1) 
		  s2 
		  (cons-stream (scar s1) 
					   (interleave s2 (scdr s1))))) 
(define (pairs s t) 
		(cons-stream 
		  (list (scar s) (scar t)) 
		  (interleave 
			(smap (lambda (x) (list (scar s) x)) 
				  (scdr t)) 
			(pairs (scdr s) (scdr t)))))
(define int-pairs (pairs integers integers))

; EX 3.67
(define (all-pairs s t) 
		(cons-stream 
		  (list (scar s) (scar t)) 
		  (interleave 
			(interleave
			  (smap (lambda (x) (list (scar s) x)) 
					(scdr t)) 
			  (smap (lambda (x) (list x (scar t))) 
					(scdr s)))
			(pairs (scdr s) (scdr t)))))
(define all-int-pairs (all-pairs integers integers))

; EX 3.68
; infinite loop
(define (louis-pairs s t) 
  (interleave 
   (smap (lambda (x) (list (scar s) x)) 
               t) 
   (louis-pairs (scdr s) (scdr t)))) 
;(define louis-ints (louis-pairs integers integers))
;(take 10 int-pairs)
;(take 10 louis-ints)

; EX 3.73
(define (integral integrand initial-value dt) 
		(define int 
				(cons-stream initial-value 
							 (add-streams (scale-stream integrand dt) 
										  int)))
		int)

(define (RC R C dt)
		(lambda (i-s v0) 
				(add-streams 
				  (scale-stream i-s R)
				  (scale-stream (integral i-s v0 dt) (/ 1 C)))))

; EX 3.78
(define (d-integral delayed-integrand initial-value dt) 
		(define int 
				(cons-stream initial-value 
							 (let ((integrand (force delayed-integrand)))
							   (add-streams (scale-stream integrand dt) 
											int))))
		int)

(define (solve-2nd a b dt y0 dy0)
  (define ys (d-integral (delay dys) y0 dt))
  (define dys (d-integral 
				(delay
				  (add-streams
					(scale-stream ys b)
					(scale-stream dys a)))
				dy0
				dt))
  ys)

; EX 3.79
(define (solve-2nd-gen f dt y0 dy0)
  (define ys (d-integral (delay dys) y0 dt))
  (define dys (d-integral 
				(delay (smap f ys dys))
				dy0
				dt))
  ys)

; EX 3.80 cool
;
; in MIT scheme I wouldn't have to use a delay in the vCs definition but in
; Racket I do because it handles internal definitions like the example in SICP
; EX 4.18, evaluating the definition value for vCs cant use the value of other
; internal definitions.  (here that other definition is iLs) But after the
; definitions are finished being processed they will be in the same environment
; and will have access to each other by name. So the delayed call in vCs to iLs
; will work. Perhaps I wouldn't have to use the delay in vCs if had I used
; letrec, which I think would allow me use values of a previous binding (iLs) in
; the evaluation of the value expression for vCs. 
;
(define (RLC R L C dt)
  (lambda (vC0 iL0)
	(define iLs (d-integral 
				  (delay
					(add-streams 
					  (scale-stream iLs (* -1 (/ R L)))
					  (scale-stream vCs (/ 1 L))))
				  iL0 dt))
	(define vCs (d-integral 
				  (delay (scale-stream iLs (/ -1 C)))
				  vC0 dt))
	(cons vCs iLs)))
;(define c (RLC 1 1 .2 .1))
;(define c-out (c 10 0))
;(apply map cons (map (lambda (x) (take 40 x)) (list (car c-out) (cdr c-out))))




; EX 3.81
; the iterative solution 
(define rand-init 7)
(define (rand-update x)
  (let ((a 3) (b 6) (m 47))
	(modulo (+ (* a x) 
			   b)
			m)))
; helper function part
(define (random-numbers-gen request-stream last-val) 
  (if (stream-null? request-stream)
	the-empty-stream
	(let ((req (scar request-stream)))
	  (cond ((eq? req 'generate) 
			 (let ((nval (rand-update last-val)))
			   (cons-stream nval
							(random-numbers-gen (scdr request-stream) nval))))
			((and (pair? req) (eq? (car req) 'reset))
			 (let ((nval (cadr req)))
			   (cons-stream nval
							(random-numbers-gen (scdr request-stream) nval))))
			(else the-empty-stream)))))

(define (rand-gen request-stream)
  (random-numbers-gen request-stream rand-init))

(define rand-reqs
  (list->stream '(generate generate (reset 8) generate generate (reset 9) (reset 7) 
						   generate generate)))

(define my-rands (rand-gen rand-reqs))
;(take-all my-rands)


; EX 3.81
; the recursive solution
(define (rand-gen-2 request-stream)
  (define (proc-rand-req req last-val) 
	(cond ((eq? req 'generate) (rand-update last-val))
		  ((and (pair? req) (eq? (car req) 'reset)) (cadr req))
		  (else '())))
  (letrec ((rands (smap proc-rand-req 
					  request-stream 
					  (cons-stream rand-init rands))))
	rands))

(define my-rands (rand-gen-2 rand-reqs))
;(take-all my-rands)

; EX 3.82
(define (random-in-range low high) 
  (let ((range (- high low))) 
    (+ low (random range)))) 

(define (monte-carlo experiment-stream passed failed) 
  (define (next passed failed) 
    (cons-stream 
     (/ passed (+ passed failed)) 
     (monte-carlo 
      (scdr experiment-stream) passed failed))) 
  (if (scar experiment-stream) 
      (next (+ passed 1) failed) 
      (next passed (+ failed 1)))) 

(define (estimate-integral P x1 x2 y1 y2)
  (define (gen-test-point)
	(cons (random-in-range x1 x2) (random-in-range y1 y2)))
  (define (gen-test-points) 
	(cons-stream (gen-test-point)
				 (gen-test-points)))
  (let ((experiment-results 
		  (smap (lambda (x) (P (car x) (cdr x)))
				(gen-test-points))))
	(scale-stream (monte-carlo experiment-results 0 0)
				  (* (- x2 x1) (- y2 y1)))))

(define (test-fn x y)
  (<= (+ (expt (- x 5) 2) 
		 (expt (- y 7) 2))
	  (expt 3 2)))
;(take 100 (estimate-integral test-fn 2 8 4 10))
