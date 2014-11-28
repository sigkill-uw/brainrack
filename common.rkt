#lang racket

;; Refer to LICENSE.txt for licensing information (GPL 3.0)

(provide empty operation)

;; Empty list
(define empty (list))

;; operation structure
;;	A program, produced by compile and interpreted by run, is a (listof operation).
;;	An operation is one of:
;;		(operation 'add n)
;;		(operation 'shift n)
;;		(operation 'in n)
;;		(operation 'out n)
;;		(operation 'unf-loop prog)
;;		(operation 'loop prog)
;;	for arbitrary integers n and for arbitrary programs (ie. (listof operation)) prog.
;;	The 'add form represents a string of consecutive '+' or '-' operations.
;;	The 'shift form represents a string of consecutive '>' or '<' operations.
;;	The 'in form represents a string of consecutive ',' (input) operations;
;;	in contrast to 'add and 'shift, there is no optimisation in compressing such a string;
;;	the compression is performed only for consistency.
;;	The 'out operation represents a string of consecutive '.' (output) operations,
;;	compressed for only the same reasons as ','.
;;	The 'unf-loop form appears only in the intermediate stages of compilation,
;;	where it signfies a loop not yet closed by ']'. The 'loop form represents
;;	a closed loop. Both forms allow nesting of code, making it possible
;;	to interpret loops in a simple, "atomic" fold operation.
;;	A program is executed from right to left, recursively executing its loops as necessary;
;;	this can be implemented intuitively with foldr.
(struct operation (code data) #:transparent)
