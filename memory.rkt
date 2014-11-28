#lang racket

;; Refer to LICENSE.txt for licensing information (GPL 3.0)

(provide memory memory-deref memory-shift memory-inc memory-set)

(struct memory (left right bytes))

(define (memory-deref m)
	(cond
		[(empty? (memory-right m)) 0]
		[else (first (memory-right m))]))

(define (memory-shiftr-once m)
	(cond
		[(empty? (memory-right m)) (memory (cons 0 (memory-left m)) empty (memory-bytes m))]
		[else (memory (cons (first (memory-right m)) (memory-left m)) (rest (memory-right m)) (memory-bytes m))]))

(define (memory-shiftl-once m)
	(cond
		[(empty? (memory-left m)) (memory empty (cons 0 (memory-right m)) (memory-bytes m))]
		[else (memory (rest (memory-left m)) (cons (first (memory-left m)) (memory-right m)) (memory-bytes m))]))

(define (memory-shift m n)
	(cond
		[(zero? n) m]
		[(< n 0) (memory-shiftl-once (memory-shift m (add1 n)))]
		[(> n 0) (memory-shiftr-once (memory-shift m (sub1 n)))]
		[else (error "invariant violated")]))

(define (memory-inc m n)
	(cond
		[(empty? (memory-right m)) (memory (memory-left m) (list n) (memory-bytes m))]
		[else (memory-set m (+ n (memory-deref m)))]))

(define (memory-set m n)
	(cond
		[(empty? (memory-right m)) (memory (memory-left m) (list (hbytes m n)) (memory-bytes m))]
		[else (memory (memory-left m) (cons (hbytes m n) (rest (memory-right m))) (memory-bytes m))]))

(define (hbytes m n)
	(cond
		[(memory-bytes m) (modulo n 256)]
		[else n]))
