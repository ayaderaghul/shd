#lang racket
(require "auto.rkt" "util.rkt" "inout.rkt" "cons.rkt" "plot.rkt")

(require racket/hash)
;;(require racket/future)
(provide (all-defined-out))
;;todo
;; fsm

(define (build-random-population n)
  (build-vector n (lambda (_) (make-random-automaton 1))))
(define (population-payoffs population)
  (for/list
      ([auto population])
    (hash-ref (automaton-head auto) 'PAY)))
(define (match-population population)
  ;(population-reset population)
  (for
      ([i (in-range 0 (- (vector-length population) 1) 2)])
    (define auto1 (vector-ref population i))
    (define auto2 (vector-ref population (+ i 1)))
    (define-values (a1 a2)
      (interact auto1 auto2))
    (vector-set! population i a1)
    (vector-set! population (+ i 1) a2))
  population)

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
(define (scan-f population)
  (define p (vector->list population))
  (foldl
   (lambda (au h)
     (hash-update h (flatten-automaton au) add1 0))
   (hash)
   p))

(define (sort-population p)
 (sort (hash->list (scan (vector-map reset p)))
       > #:key cdr))

;; MUTATE
(define (mutate-population population rate)
  (for ([i (in-range rate)])

    (define auto (vector-ref population i))
    (vector-set! population i (mutate auto))))

(define (evolve population cycles speed mutation mean-file rank-file p-file sim-id)
  (cond
    [(zero? cycles) (out-population sim-id (scan-f population) p-file)]
    [else
     (and (zero? (modulo cycles 100)) (print (number->string cycles)))
     (define p2 (match-population population))
;;     (print "matched\n")
     (define pp (population-payoffs p2))
  ;;   (print "pp-ed\n")
     (define p3 (regenerate p2 speed))
    ;; (print "regenerated\n")
     (define p4 (vector-map reset p3))
     ;;(print "reset-ed\n")
     (and (zero? (modulo cycles 100)) (out-rank cycles (scan-f p4) rank-file))
     ;;(print "out-ranked\n")
     (mutate-population p4 mutation)
     ;;(print "mutated\n")
     (out-data mean-file (list (list (average pp))))
     ;;(print "out-meaned\n")
     (evolve p4 (- cycles 1)
             speed mutation mean-file rank-file p-file sim-id)]))

(define (evolve-p population cycles speed mutation)
  (cond
    [(zero? cycles) (list population)]
    [else
     (define p2 (match-population population))
     (define pp (population-payoffs p2))
     (define p3 (regenerate p2 speed))
     (define auto (vector-ref p3 0))
     
;;     (and (zero? (modulo cycles 100)) (out-rank cycles (scan p3) rank-file))
     (mutate-population p3 mutation)    
;;     (out-data mean-file (list (list (average pp))))
     (cons (average pp)
           (evolve-p (vector-map reset p3) (- cycles 1)
                   speed mutation))]))


(define (main)
  (collect-garbage)
  (define POPU
(gen-name LOCATION SIM-ID "p.txt"))
	(define p-POPU (gen-name LOCATION (- SIM-ID 1) "p.txt"))
  (define POPULATION
    (if (= SIM-ID 1)
        (build-random-population N)
        (resurrect-p (csvfile->list p-POPU))))
  (define MEAN (gen-name LOCATION SIM-ID "mean"))
  (define RANK (gen-name LOCATION SIM-ID "rank"))
  (time (evolve POPULATION CYCLES SPEED MUTATION MEAN RANK POPU SIM-ID))
  (define DATA (csvfile->list MEAN))
  (define PIC (gen-name LOCATION SIM-ID "pic.png"))
  (define TIT (gen-pic-title))
  (plot-mean (input->numbers DATA) DELTA ROUNDS PIC TIT))

