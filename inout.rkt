#lang racket

(require "csv.rkt")
(require "auto.rkt")
(provide (all-defined-out))
(require csv-reading)
(require racket/string)

(define (csvfile->list filename)
  (call-with-input-file filename
                        csv->list))

(define (input->numbers data)
  (map string->number (flatten data)))

(define (input->coors data)
(for/list ([i (in-list data)])
(map string->number i)))

(define (get-position n ls)
  (define l (length ls))
  (define ls2
    (for/list ([i (in-range l)][j (in-list ls)])
      #:break (equal? n j)
      (equal? n j)))
  (length ls2))

(define (get-posn-next-number ls)
  (define l (length ls))
  (define ls2
    (for/list ([i (in-range 1 l)])
      #:final (string->number (list-ref ls i))
      (string->number (list-ref ls i))))
  (length ls2))

(define (population-at data cycle total)
  (define x (- total cycle))
  (define raw  (flatten data))
  (define start (get-position (number->string x) raw))
  (define x2 (- x 100))
  (define end  (get-position (number->string x2) raw))
  (drop (take raw end) (add1 start)))
#|
(define (resurrect-helper auto)
  (define h (take auto 2))
  (define b (drop auto 2))
  (define l (/ (length b) 8))
  (define head (hash 'PAY (first h) 'INIT (second h)))
  (define (resurrect-state s)
    (list
     (first s)
     (state (second s)
            (apply hash (drop s 2)))))
  (define body
    (apply hash
           (flatten
            (for/list ([i (in-range l)])
              (resurrect-state (take (drop b (* i 8)) 8))))))
  (automaton head body)) 

(define (resurrect automata)
  (for/list ([i (in-list automata)])
    (define z (first (string-split i "((")))
    (define a (string-split z ")"))
    (match-define (list b1 b2) (map string-split a))
    (define d (map string->number b1))
    (resurrect-helper d)))

(define (resurrect-n automata)
  (for/list ([i (in-list automata)])
    (define z (first (string-split i "((" )))
    (define a (string-split z ")" ))
    (match-define (list b1 b2) (map string-split a))
    (define d1 (map string->number b1))
    (define d2 (string->number (second b2)))
    (cons (resurrect-helper d1) d2)))

(define (resurrect-ethnic string)
  (define z (first (string-split string "((")))
  (define a (string-split z ")"))
  (match-define (list b1 b2) (map string-split a))
  (define c2 (drop b2 1))
  (define d1 (map string->number b1))
  (define this-many (string->number (first c2)))
  (build-vector this-many (lambda (_) (resurrect-helper d1))))

(define (resurrect-p data)
  (define a (drop (flatten data) 1))
  (define ethnics (map resurrect-ethnic a))
  (apply vector-append ethnics))

(define (resurrect-po data)
(define ethnics (map resurrect-ethnic data))
(apply vector-append ethnics)) 
|#
;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

(define (out-rank cycles rankings rank-file)
  (define sorted (sort (hash->list rankings) > #:key cdr))
  (define l (length sorted))
  (define export-this (if (> l 20) (take sorted 20) sorted))
  (out-data rank-file (list (list cycles)))
  (out-data rank-file (map list export-this)))

(define (out-population cycles rankings p-file)
  (out-data p-file (list (list cycles)))
  (out-data p-file (map list (sort (hash->list rankings) > #:key cdr))))
