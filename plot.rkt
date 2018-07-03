#lang racket
(require plot)
(plot-new-window? #t)

(provide (all-defined-out))

(define b-l 1)
(define b-m 13)
(define b-h 21)

(define (population-mean->lines data)
  (define coors
    (for/list ([d (in-list data)]
               [n (in-naturals)])
      (list n d)))
  (lines coors))

(define (mean->lines-i data from to)
  (define coors
    (for/list ([d (in-list data)]
               [n (in-range from to)])
      (list n d)))
  (lines coors))

(define (compound d r)
  (foldl (lambda (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))

(define (plot-mean-p data delta rounds)
  (define low (* b-l (compound delta rounds)))
  (define medium (* b-m (compound delta rounds)))
  (define high (* b-h (compound delta rounds)))
  (define line1
    (function (lambda (x) low) #:color "blue"))
  (define line2
    (function (lambda (x) medium) #:color "green"))
  (define line3
    (function (lambda (x) high) #:color "red"))
  (plot (list line1 line2 line3
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 high) #:width 1200 #:height 800))

(define (plot-mean data delta rounds pic tit)
  (define low (* b-l (compound delta rounds)))
  (define medium (* b-m (compound delta rounds)))
  (define high (* b-h (compound delta rounds)))
  (define line1
    (function (lambda (x) low) #:color "blue"))
  (define line2
    (function (lambda (x) medium) #:color "green"))
  (define line3
    (function (lambda (x) high) #:color "red"))
  (plot (list line1 line2 line3
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 high) #:width 1200 #:height 800
        #:out-file pic #:title tit))


(define (plot-mean-i data from to delta rounds pic tit)
  (define low (* b-l (compound delta rounds)))
  (define medium (* b-m (compound delta rounds)))
  (define high (* b-h (compound delta rounds)))
  (define line1
    (function (lambda (x) low) #:color "blue"))
  (define line2
    (function (lambda (x) medium) #:color "green"))
  (define line3
    (function (lambda (x) high) #:color "red"))
  (plot (list line1 line2 line3
              (mean->lines-i data from to))
        #:y-min 0 #:y-max (+ 5 high) #:width 1200 #:height 800
        #:out-file pic #:title tit))
(define (pays->lines data label color)
  (define coors
    (for/list ([d (in-list data)]
               [n (in-naturals)])
      (list n d)))
  (lines coors #:label label #:color color))
(define (plot-pays wl tm th wa ta ts delta rounds pic tit)
  (define low (* b-l (compound delta rounds)))
  (define medium (* b-m (compound delta rounds)))
  (define high (* b-h (compound delta rounds)))
  (define line1
    (function (lambda (x) low) #:color "blue"))
  (define line2
    (function (lambda (x) medium) #:color "green"))
  (define line3
    (function (lambda (x) high) #:color "red"))
  (plot (list line1 line2 line3
              (pays->lines wl "w low" 'red)
              (pays->lines tm "t medium" 'green)
              (pays->lines th "t high" 'yellow)
              (pays->lines wa "w acc" 'blue)
              (pays->lines ta "t acc" 'purple)
              (pays->lines ts "t tough" 'black))
        #:y-min 0 #:y-max (+ 5 high) #:width 1200 #:height 800
        #:out-file pic #:title tit))
