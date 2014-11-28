#lang racket

(require "compiler.rkt")
(require "interpreter.rkt")

(cond
	[(= (vector-length (current-command-line-arguments)) 1)
		(run 
			(compile (open-input-file (vector-ref (current-command-line-arguments) 0)))
			(current-input-port)
			(current-output-port))]
	[else (display "usage: ")])
