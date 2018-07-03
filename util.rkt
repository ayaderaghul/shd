#lang racket
(provide (all-defined-out))


;; the number of nodes N
;; the number of nodes to be connected c: 1 < c <= N
;; the nodes are indexed from 0 to N-1

;; from nodes#, calculate dashes#
;; for example: 5 nodes
;; node 0 can connect to the rest (4)
;; node 1 can connect to the rest (3)
;; ..
;; 4 + 3 + 2 + 1 + 0 = 10

(define (how-many-dashes? nodes#)
  (apply + (build-list nodes# values)))
(define (dashes nodes#)
  (for*/list ([i (in-range (- nodes# 1))]
              [j (in-range (+ i 1) nodes#)])
    (list i j)))
    



(define (random-member a-list)
  (define l (length a-list))
  (define r (random l))
  (list-ref a-list r))
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

