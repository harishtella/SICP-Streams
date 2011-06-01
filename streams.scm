; written in Scheme just to grok some concepts  
; from the book

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
(define (louis-pairs s t) 
  (interleave 
   (smap (lambda (x) (list (scar s) x)) 
               t) 
   (pairs (scdr s) (scdr t)))) 
(define louis-ints (louis-pairs integers integers))
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



