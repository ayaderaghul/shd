#lang racket
(provide (all-defined-out))
;; CONFIGURATION
(define SIM-ID 1)
(define LOCATION 1) ;; 0 home 1 school lab

(define N 33)
(define CYCLES 5000) ;; careful, you change the cycles here
(define SPEED 10)

(define ROUNDS 2) ;; not here
(define DELTA .01)

(define DELTAstr (string-trim (number->string (* DELTA 100)) ".0"))
(define MUTATION 1) ;; 3x3 game needs more mutation

(define DELTAS (build-list ROUNDS (lambda (x) (expt DELTA x))))

(define OUTLABstr "/Users/linhchi.nguyen/Dropbox/shd/")

(define (gen-out id)
  (string-append "/Users/linhchi.nguyen/Dropbox/shd/"
                 DELTAstr (number->string id)))
(define (gen-outs id)
  (string-append "/Users/linhchi.nguyen/Dropbox/shd/"
                 DELTAstr (number->string id) "o"))
;(define OUTFILE (gen-out 1))
(define (gen-in id)
  (string-append "/Users/linhchi.nguyen/Dropbox/shd/"
                 DELTAstr (number->string id) "rank"))


(define (gen-name location id name)
  (format "~a~a~a~a"
	(if (= location 1) OUTLABstr "")
          DELTAstr (number->string id) name))
(define (gen-name-rep location id name)
  (format "~a~a~a/~a"
          (if (= location 1) OUTLABstr "")
          "rep-tm-" (number->string id) name))

(define (gen-pic-title)
  (format "ID = ~s, N = ~s, s = ~s, r = ~s, d = ~s, m = ~s" SIM-ID N SPEED ROUNDS DELTA MUTATION))
