#lang racket

;; Refer to LICENSE.txt for licensing information (GPL 3.0)

;; compiler.rkt:
;;	implements compile, a function with converts a textual brainfuck program
;;	to the internal (listof operation) representation.

;; Need operation
(require "common.rkt")

;; compile is public;; everything else is internal to compiler
(provide compile)

;; compile: string -> listof operation
;; Compiles a textual Brainfuck program to the interpreter's internal operation representation.
;; Performs some trivial optimizations (folding together + and -, for instance);;
;; also detects bracket matching issues, throwing errors when they are encountered.
;; However, the primary motivation of compilation is to handle the difficult issue of nested loops.
(define (compile file)
	;; Compile the code by foldr'ing over the text with compile-step; then, error-check right brackets
	;; foldr, rather than foldl, iterates over the text backwards;
	;; this allows us to build up the program with simple cons operations and execute it forward with foldl later
	(compile-check-left (sequence-fold (lambda (i c) (compile-step (integer->char c) i)) empty file)))

;; compile-check-right: listof operation -> listof operation
;; Checks whether the given compiled program has unmatched closing brackets.
;; If so, an error is thrown;; if not, the program is returned unchanged.
(define (compile-check-left prog)
	(match prog
		[(cons (struct operation ('unf-loop _)) thus) ;; The right bracket is unmatched iff there's an unclosed loop
			(error "unmatched brackets: too many ['s")]
		[(cons (struct operation (_ 0)) pprog) pprog] ;; The most trivial of optimizations - snip off a leading null operation
		[_ prog])) ;; Otherwise just return the program untouched

;; compile-step: char (listof operation) -> (listof operation)
;; Performs a single step of compilation by folding a given character of Brainfuck into a given program.
;; Error detection for opening brackets is done here.
(define (compile-step c prog)
	(match prog
		['() (compile-fresh-operation c prog)] ;; If we have an empty program, we can shove the new op on indiscriminately
		[(cons (struct operation ('loop _)) thus) (compile-fresh-operation c prog)] ;; We can't add to a closed loop

		;; If the last operation was nullified ("++--" or "<>", for instance), we can just trim it off and tack on a fresh one
		[(cons (struct operation (_ 0)) thus) (compile-fresh-operation c thus)]

		;; For an add operation, we can fold consecutive '+' and '-' together
		[(cons (struct operation ('add count)) thus)
			(cond
				[(equal? c #\+) (cons (operation 'add (add1 count)) thus)]
				[(equal? c #\-) (cons (operation 'add (sub1 count)) thus)]
				[else (compile-fresh-operation c prog)])] ;; If we can't fold together, tack on a fresh op

		;; For a shift operation, we can fold '>'/'<'
		[(cons (struct operation ('shift count)) thus)
			(cond
				[(equal? c #\>) (cons (operation 'shift (add1 count)) thus)]
				[(equal? c #\<) (cons (operation 'shift (sub1 count)) thus)]
				[else (compile-fresh-operation c prog)])] ;; If we can't fold together, tack on a fresh op

		;; We fold together output operations just for consistency. Less useful
		[(cons (struct operation ('out count)) thus) 
			(cond
				[(equal? c #\.) (cons (operation 'out (add1 count)) thus)]
				[else (compile-fresh-operation c prog)])]

		;; Folding just for consistency, again
		[(cons (struct operation ('in count)) thus)
			(cond
				[(equal? c #\,) (cons (operation 'in (add1 count)) thus)]
				[else (compile-fresh-operation c prog)])]

		;; The meat of the compile step is in handling the loops
		[(cons (struct operation ('unf-loop loop)) thus)
			(cond
				;; If the character is a loop close, we need to close the deepest-nested open loop
				[(equal? c #\])
					(match loop
						[(cons (struct operation ('unf-loop _)) ithus) ;; Match a nested open loop
							(cons (operation 'unf-loop (compile-step #\] loop)) thus)] ;; Recur; close the inner open loop before the outer one

						[_ ;; If there's not nested open loop...
							(cons (operation 'loop loop) thus)])] ;; Just close this one

				;; Otherwise, we recur, again placing the operation at the head of the deepest-nested open loop
				[else (cons (operation 'unf-loop (compile-step c loop)) thus)])]
	 
		;; This shouldn't happen
		[_ (error "invariant violated")]))

;; compile-fresh-operation: char (listof operation) -> listof operation
;; Tacks a fresh (ie. not foldable) operation onto the head of a program.
;; It's slightly more succint to have the cons in here rather than in compile-step.
;; Also detects unmatched opening brackets.
(define (compile-fresh-operation c prog)
	(cond
		[(equal? c #\+) (cons (operation 'add 1) prog)]
		[(equal? c #\-) (cons (operation 'add -1) prog)]
		[(equal? c #\>) (cons (operation 'shift 1) prog)]
		[(equal? c #\<) (cons (operation 'shift -1) prog)]
		[(equal? c #\.) (cons (operation 'out 1) prog)]
		[(equal? c #\,) (cons (operation 'in 1) prog)]
		[(equal? c #\[) (cons (operation 'unf-loop empty) prog)]
		[(equal? c #\]) (error "unmatched brackets: too many ]'s")]
		[else prog])) ;; Characters not in the above set are ignored in Brainfuck
