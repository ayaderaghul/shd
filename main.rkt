#lang racket
(require "auto.rkt" "util.rkt" "inout.rkt" "cons.rkt" "plot.rkt")

(require racket/hash)
;;(require racket/future)
(provide (all-defined-out))
;;todo
;; fsm

(define (build-random-population n)
  (list
   (build-vector n (lambda (_) (make-player 1)))
   (build-vector n (lambda (_) (make-player 2)))
   (build-vector n (lambda (_) (make-player 4)))))

(define (population-payoffs population)
  (for/list
      ([auto population])
    (player-payoff auto)))

(define (match-population population)
  ;(population-reset population)
  (define po1 (first population))
  (define po2 (second population))
  (define po3 (third population))
  (define l (vector-length po1))
  (for
      ([i (in-range l)])
    (define auto1 (vector-ref po1 i))
    (define auto2 (vector-ref po2 i))
    (define auto3 (vector-ref po3 i))
    (match-define (list a1 a2 a3)
                  (interact auto1 auto2 auto3))
    (vector-set! po1 i a1)
    (vector-set! po2 i a2)
    (vector-set! po3 i a3))
  (list po1 po2 po3))

(define (payoff->fitness population)
  (define payoffs (population-payoffs population))
  (define total (sum payoffs))
  (for/list ([p (in-list payoffs)])
    (/ p total)))

(define (accumulate-fitness probabilities)
  (let relative->absolute
      ([payoffs probabilities] [so-far #i0.0])
    (cond
      [(empty? payoffs) '()]
      [else (define nxt (+ so-far (first payoffs)))
            (cons nxt (relative->absolute (rest payoffs) nxt))])))

(define (randomise-s probabilities speed)
  (define fitness (accumulate-fitness probabilities))
  (for/list ([n (in-range speed)])
    (define r (random))
    (for/last ([p (in-naturals)]
               [% (in-list fitness)]
               #:final (< r %)) p)))

(define (regenerate population rate)
  (define probabilities (payoff->fitness population))
  (define substitutes (randomise-s probabilities rate))
  (for ([i (in-range rate)]
        [auto (in-list substitutes)])
    (vector-set! population i
                 (vector-ref population auto)))
  (shuffle-vector population))

(define (population-reset population)
  (for ([auto population]
        [i (in-naturals)])
    (vector-set! population i (reset auto))))


(define (scan population)
  (define p (vector->list population))
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   p))

#|
(define (scan-f population)
  (define p (vector->list population))
  (foldl
   (lambda (au h)
     (hash-update h (flatten-automaton au) add1 0))
   (hash)
   p))
|#
(define (sort-population p)
 (sort (hash->list (scan (vector-map reset p)))
       > #:key cdr))

;; MUTATE
(define (mutate-population population rate)
  (for ([i (in-range rate)])
    (define auto (vector-ref population i))
    (vector-set! population i (mutate auto))))

(define (evolve population cycles speed mutation mean1 mean2 mean3 rank-file p-file sim-id)
  (cond
    [(zero? cycles) 
     (list
      (out-population sim-id (scan (first population)) p-file)
      (out-population sim-id (scan (second population)) p-file)
      (out-population sim-id (scan (third population)) p-file))]
    [else
     (and (zero? (modulo cycles 100)) (print (number->string cycles)))
     (define p2 (match-population population))
;;     (print "matched\n")
     (define (evolve-h po mean-file)
       (define pp (population-payoffs po))
       ;;   (print "pp-ed\n")
       (define p3 (regenerate po speed))
       ;; (print "regenerated\n")
       (define p4 (vector-map reset p3))
       ;;(print "reset-ed\n")
       (and (zero? (modulo cycles 100)) (out-rank cycles (scan p4) rank-file))
       ;;(print "out-ranked\n")
       (mutate-population p4 mutation)
       ;;(print "mutated\n")
       (out-data mean-file (list (list (average pp))))
       ;;(print "out-meaned\n")
       p4)
     (define p5
;       (for/list ([pop population])
       (list
        (evolve-h (first population) mean1)
        (evolve-h (second population) mean2)
        (evolve-h (third population) mean3)))
     (evolve p5 (- cycles 1)
             speed mutation mean1 mean2 mean3 rank-file p-file sim-id)]))

(define (evolve-p population cycles speed mutation)
  (cond
    [(zero? cycles) (list population)]
    [else
     (define p2 (match-population population))
     (define (evolve-h po)
       (define pp (population-payoffs po))
       (define p3 (regenerate po speed))
       (define auto (vector-ref p3 0))
       ;;     (and (zero? (modulo cycles 100)) (out-rank cycles (scan p3) rank-file))
       (mutate-population p3 mutation)    
       ;;     (out-data mean-file (list (list (average pp))))
       pp
       )
     (define pps
       (for/list ([pop population])
         (evolve-h pop)))
     (cons (map average pps)
           (evolve-p (vector-map reset p3) (- cycles 1)
                   speed mutation))]))


(define (main)
  (collect-garbage)
  (define POPU
    (gen-name LOCATION SIM-ID "p.txt"))
  (define p-POPU (gen-name LOCATION (- SIM-ID 1) "p.txt"))
  (define POPULATION
   ; (if (= SIM-ID 1)
        (build-random-population N))
   ;     (resurrect-p (csvfile->list p-POPU))))
  (define MEAN1 (gen-name LOCATION SIM-ID "mean1"))
  (define MEAN2 (gen-name LOCATION SIM-ID "mean2"))
  (define MEAN3 (gen-name LOCATION SIM-ID "mean3"))
  (define RANK (gen-name LOCATION SIM-ID "rank"))
  (time (evolve POPULATION CYCLES SPEED MUTATION MEAN1 MEAN2 MEAN3 RANK POPU SIM-ID))
  (define DATA1 (csvfile->list MEAN1))
    (define DATA2 (csvfile->list MEAN2))
  (define DATA3 (csvfile->list MEAN3))

  (define PIC (gen-name LOCATION SIM-ID "pic.png"))
  (define TIT (gen-pic-title))
  (plot-mean (input->numbers DATA1) DELTA ROUNDS PIC TIT)
  (plot-mean (input->numbers DATA2) DELTA ROUNDS PIC TIT)
  (plot-mean (input->numbers DATA3) DELTA ROUNDS PIC TIT))

