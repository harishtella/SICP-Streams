; written in Scheme just to grok some concepts  
; from the book

(define (scar s) (car s))
(define (scdr s) (force (cdr s)))

(define (sref s n) 
  (if (= n 0) 
      (stream-car s) 
      (stream-ref (stream-cdr s) (- n 1)))) 

(define (sget n s)
  (if (= n 0)
	'()
	(cons (scar s) (sget (- n 1) (scdr s)))))
 
(define (stream-for-each proc s) 
  (if (stream-null? s) 
      'done 
      (begin (proc (stream-car s)) 
             (stream-for-each proc (stream-cdr s))))) 

(define (memo-proc proc) 
  (let ((already-run? false) (result false)) 
    (lambda () 
      (if (not already-run?) 
          (begin (set! result (proc)) 
                 (set! already-run? true) 
                 result)
           result)))) 

(define (stream-map proc s1 s2) 
  (if (or (stream-null? s1) 
		  (stream-null? s2)) 
      the-empty-stream 
	  (cons-stream (proc (scar s1) (scar s2)) 
				   (stream-map proc (scdr s1) (scdr s2)))))

(define (add-streams s1 s2) 
  (stream-map + s1 s2)) 

(define (mul-streams s1 s2)
		(stream-map * s1 s2))

(define ones (cons-stream 1 ones)) 
(define integers (cons-stream 1 (add-streams ones integers))) 

;EX 3.54
(define factorials (cons-stream 1 (mul-streams factorials (scdr integers))))



