#lang racket

;; Refer to LICENSE.txt for licensing information (GPL 3.0)

(require "common.rkt")
(require "memory.rkt")

(provide run)

;; A state is the state of a given program throughout its execution.
;; A state is of the form (state m pi po),
;; for a memory m, an input port pi, and an output port po.
;; The interpreter, based on the instructions within its program,
;; manipulates the data in m (with '+', '-', '>', '<', ',' operations),
;; receives data from pi (with ',', handling EOF gracefully),
;; and sends data through po (with '.').
(struct state (munit stdin stdout))

;; run: (listof operation) input-port output-port boolean -> output-port
;; Executes 
(define (run program stdin stdout [bytes true])
	(state-stdout
		(run-from-state program
			(state
				(memory empty empty bytes)
				stdin
				stdout))))

;; run: (listof operation) state -> state
(define (run-from-state program stat)
	(foldr run-step stat program))

;; run-step: operation state -> state
(define (run-step oc stat)
	(match oc
		[(struct operation ('add count)) (state (memory-inc (state-munit stat) count) (state-stdin stat) (state-stdout stat))]
		[(struct operation ('shift count)) (state (memory-shift (state-munit stat) count) (state-stdin stat) (state-stdout stat))]
		[(struct operation ('in count)) (foldl (lambda (x s) (input s)) stat (build-list count values))]
		[(struct operation ('out count)) (begin (map (lambda (x) (output stat)) (build-list count values)) stat)]
		[(struct operation ('loop body)) (until-zero body stat)]
		[_ (error "invariant violated")]))

;; input: state -> state
(define (input stat)
	(state
		(let ([c (read-byte (state-stdin stat))])
			(cond
				[(equal? c eof) (state-munit stat)]
				[else (memory-set (state-munit stat) c)]))
		(state-stdin stat)
		(state-stdout stat)))

;; output: state -> void
(define (output stat)
	(write-byte (modulo (memory-deref (state-munit stat)) 256) (state-stdout stat)))

;; until-zero: (listof operation) state -> state
(define (until-zero body stat)
	(cond
		[(zero? (memory-deref (state-munit stat))) stat]
		[else (until-zero body (run-from-state body stat))]))
