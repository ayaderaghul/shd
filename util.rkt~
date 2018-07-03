#lang racket
(provide (all-defined-out))

(define (randomise probability)
  (define r (random))
  (for/last
      ([s (in-naturals)]
       [p (in-list (list probability 1))]
       #:final (< r p))
    s))

(define (round5 n)
  (/ (round (* 100000 n))
     100000))

(define (round1 n)
  (/ (round (* 10 n))
     10))

(define (sum l)
  (apply + l))

(define (shuffle-vector vec)
  (define lst (vector->list vec))
  (define l2 (shuffle lst))
  (list->vector l2))


(define (average lst)
  (exact->inexact (/ (sum lst) (length lst))))

