#lang racket

(require "common.rkt")
(require "memory.rkt")

(provide run)

(struct state (munit stdin stdout))

(define (run program stdin stdout [bytes true])
	(state-stdout
		(run-from-state program
			(state
				(memory empty empty bytes)
				stdin
				stdout))))

(define (run-from-state program stat)
  (foldr run-step stat program))

(define (run-step oc stat)
  (match oc
    [(struct operation ('add count)) (state (memory-inc (state-munit stat) count) (state-stdin stat) (state-stdout stat))]
    [(struct operation ('shift count)) (state (memory-shift (state-munit stat) count) (state-stdin stat) (state-stdout stat))]
    [(struct operation ('in count)) (foldl (lambda (x s) (input s)) stat (build-list count values))]
    [(struct operation ('out count)) (begin (map (lambda (x) (output stat)) (build-list count values)) stat)]
    [(struct operation ('loop body)) (until-zero body stat)]
	[_ (error "invariant violated")]))
  

(define (input stat)
	(state
		(let ([c (read-byte (state-stdin stat))])
			(cond
				[(equal? c eof) (state-munit stat)]
				[else (memory-set (state-munit stat) c)]))
		(state-stdin stat)
		(state-stdout stat)))

(define (output stat)
	(write-byte (modulo (memory-deref (state-munit stat)) 256) (state-stdout stat)))

(define (until-zero body stat)
  (cond
    [(zero? (memory-deref (state-munit stat))) stat]
    [else (until-zero body (run-from-state body stat))]))
