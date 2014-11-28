#lang racket

(provide empty operation)

(define empty (list))
(struct operation (code data) #:transparent)
