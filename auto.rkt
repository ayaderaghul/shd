#lang racket

(require racket/hash "util.rkt" )

(provide (all-defined-out))

(define ACTIONS# 2)
(define ACTIONS (list 'H 'D))
(define (random-action)
  (random ACTIONS#))

(struct player (payoff dash node) #:transparent)

;; 4 nodes: 0 1 2 3
;; 6 possible dashes: 0-1 0-2 0-3 1-2 1-3 2-3
;; randomise over this to choose which dashes are activated

(define (activate-these-dashes nodes#)
  (define dash-list (dashes nodes#))
  (define dashes# (length dash-list))
  (define connect-this-many (random (+ 1 dashes#)))
  (define (dash-ids counter ids a-list)
    (cond [(zero? counter) ids]
          [else
           (define id (random-member a-list))
           (dash-ids (- counter 1) (cons id ids)
                     (remove id a-list))]))
  (sort (dash-ids connect-this-many '() dash-list) #:key car <))
    
(define (make-informed-player nodes#)
  (define nodes
    (for/list ([i (in-range nodes#)])
      (cons i (random ACTIONS#))))
  (player 0 '() (make-hash nodes)))

;; some of the dashes are redundant
        
(define (member? list1 list2)
  (define res
    (for/list ([i (in-list list1)]
               #:break (member i (flatten list2)))
      i))
  (if (= (length res) (length list1)) #f #t))

(define (continue? a-list)
  (cond [(null? a-list) #f]
        [else
         (define init (first a-list))
         (define test
           (for/list ([nxt (in-list (rest a-list))]
                      #:break (member? nxt init))
             (if (member? nxt init)
                 #t #f)))
         (if (= (length test) (length (rest a-list))) #f #t)]))

(define (simplify a-list)
  (if (continue? a-list) 
      (simplify-h a-list)
      a-list))
 
(define (simplify-h a-list)
  (foldl 
   (lambda 
       (next init)
     (define updated-init
       (for/list ([i (in-list init)])
         (if (member? next i)
             (remove-duplicates (append i next))
             i)))
     (if (continue? a-list) 
         updated-init (cons next updated-init)))
   (list (first a-list))
   (rest a-list)))

(define (uninform-player pl nodes#)
  (cond [(= nodes# 1) pl]
        [else
         (match-define (player p d n) pl)
         (define to-activate (flatten (simplify 
                                       (activate-these-dashes nodes#))))
         (cond [(null? to-activate) pl]
               [else
                (for ([i (in-list (rest to-activate))])
                  (define a (hash-ref n (first to-activate)))
                  (hash-set! n i a))
                (player p to-activate n)])]))

(define (make-player nodes#)
  (define pl (make-informed-player nodes#))
  (uninform-player pl nodes#))


      
;; MUTATION
;; mutate action or mutate the ignorance state

(define (mutate-action pl)
  (match-define (player p d n) pl)
  (define nodes# (hash-count n))
  (define r (random nodes#))
  (define a (random ACTIONS#))
  (if (member r d) 
      (for ([i (in-list d)])
        (hash-set! n i a))
      (hash-set! n r a))
  pl)

;; mutate ignorance state

(define (mutate-ignorance pl)
  (match-define (player p d n) pl)
  (define nodes# (hash-count n))
  (define l (length d))
  (define new-d
    (cond [(= l nodes#) (remove (random l) d)]
          [(= l 0) (

;; IMMUTABLE MUTATION

(define PAYOFF-TABLE
  (list
   (list (cons 1 1) (cons 6 2))
   (list (cons 2 6) (cons 4 4))
   ))


(define (payoff action1 action2)
  (list-ref
   (list-ref PAYOFF-TABLE action1)
   action2))

(define (interact au1 au2)
  (match-define (automaton head1 body1) au1)
  (match-define (automaton head2 body2) au2)
  (define-values (next1 next2 pay1 pay2 results)
    (for/fold ([current1 (hash-ref head1 'INIT)]
               [current2 (hash-ref head2 'INIT)]
               [payoff1 (hash-ref head1 'PAY)]
               [payoff2 (hash-ref head2 'PAY)]
               [results '()])
              ([_ (in-list DELTAS)])
      (match-define (state action1 dispatch1) (hash-ref body1 current1))
      (match-define (state action2 dispatch2) (hash-ref body2 current2))
      (match-define (cons pay1 pay2) (payoff action1 action2))
      (define n1 (hash-ref dispatch1 action2))
      (define n2 (hash-ref dispatch2 action1))
      (define result (list pay1 pay2))
      (values n1 n2
              (+ payoff1 (* _ pay1))
              (+ payoff2 (* _ pay2))
              (cons result results))))
  (values 
   ;; (reverse results)
   (automaton (hash-set head1 'PAY (round5 pay1)) body1)
   (automaton (hash-set head2 'PAY (round5 pay2)) body2)))

(define (interact-r au1 au2)
  (match-define (automaton head1 body1) au1)
  (match-define (automaton head2 body2) au2)
  (define-values (next1 next2 pay1 pay2 results)
    (for/fold ([current1 (hash-ref head1 'INIT)]
               [current2 (hash-ref head2 'INIT)]
               [payoff1 (hash-ref head1 'PAY)]
               [payoff2 (hash-ref head2 'PAY)]
               [results '()])
              ([_ (in-list DELTAS)])
      (match-define (state action1 dispatch1) (hash-ref body1 current1))
      (match-define (state action2 dispatch2) (hash-ref body2 current2))
      (match-define (cons pay1 pay2) (payoff action1 action2))
      (define n1 (hash-ref dispatch1 action2))
      (define n2 (hash-ref dispatch2 action1))
      (define result (list pay1 pay2))
      (values n1 n2
              (+ payoff1 (* _ pay1))
              (+ payoff2 (* _ pay2))
              (cons result results))))
  (cons
   ;; (reverse results)
(round1 pay1) (round1 pay2))) 

(define (interact-s au1 au2)
  (match-define (automaton head1 body1) au1)
  (match-define (automaton head2 body2) au2)
  (define-values (next1 next2 pay1 pay2 results)
    (for/fold ([current1 (hash-ref head1 'INIT)]
               [current2 (hash-ref head2 'INIT)]
               [payoff1 (hash-ref head1 'PAY)]
               [payoff2 (hash-ref head2 'PAY)]
               [results '()])
              ([_ (in-list DELTAS)])
      (match-define (state action1 dispatch1) (hash-ref body1 current1))
      (match-define (state action2 dispatch2) (hash-ref body2 current2))
      (match-define (cons pay1 pay2) (payoff action1 action2))
      (define n1 (hash-ref dispatch1 action2))
      (define n2 (hash-ref dispatch2 action1))
      (define result (list pay1 pay2))
      (values n1 n2
              (+ payoff1 (* _ pay1))
              (+ payoff2 (* _ pay2))
              (cons result results))))
  (values 
   (take (reverse results) 20)
   (automaton (hash-set head1 'PAY (round5 pay1)) body1)
   (automaton (hash-set head2 'PAY (round5 pay2)) body2)))
    

;;benchmark

(define BENCHMARKS (list (H) (D)   ))

(define (benchmark au)
  (cons (interact-r au au)
        (for/list ([i (in-list BENCHMARKS)])
          (interact-r au i))))

(define (interact-g au aus)
  (for/list ([i (in-list aus)])
    (interact-r au i)))

(define (create-matrix au)
  (define ls (cons au BENCHMARKS))
  (for/list ([i (in-list ls)])
    (interact-g i ls)))

(define (create-matrix-l ls)
  (for/list ([i (in-list ls)])
    (interact-g i ls)))

(define (reverse-matrix mat)
  (define l (length mat))
  (define (col x) (map (lambda (ls) (list-ref ls x)) mat))
  (for/list ([i (in-range l)])
    (col i)))

(define (create-cell pair m-r m-c)
  (match-define (cons p1 p2) pair)
  (if (= p2 m-r)
      (if (= p1 m-c)
          (format "*~a ~a*" p1 p2)
          (format " ~a ~a*" p1 p2))
      (if (= p1 m-c)
          (format "*~a ~a " p1 p2)
          (format " ~a ~a " p1 p2))))

(define (create-row pairs m-r m-c-s)
  (for/list ([i (in-list pairs)]
             [m-c (in-list m-c-s)])
    (~a (create-cell i m-r m-c) #:min-width 15 #:align 'center)))
  
(define (print-matrix mat)
  (define r (length mat))
  (define c (length (first mat)))
  (define ms 
    (for/list ([row (in-list mat)])
      (define ls (map cdr row))
      (define m (apply max ls))
      m))
  (for/list ([i (in-list mat)]
             [m-r (in-list ms)])
    (apply string-append (create-row i m-r ms))))

(define (interact-m au aus num)
  (define res
    (for/list ([i (in-list aus)]
               [j (in-list num)])
      (cons
       (* (car (interact-r au i)) j)
       (* (cdr (interact-r au i)) j))))
  (cons
   (round1 (/ (apply + (map car res)) 100))
   (round1 (/ (apply + (map cdr res)) 100))))

(define (interact-m-r aus num au)
  (define res (interact-m au aus num))
  (reverse-p res))

(define (interact-m-itself aus num)
  (define res-m-h
    (for/list ([i (in-list aus)]
               [j (in-list num)])
      (* j (car (interact-m i aus num)))))
  (define res-m
    (round1 (/ (apply + res-m-h) 100)))
  (cons res-m res-m))

(define (benchmark-m mix)
  (define aus (map car mix))
  (define num (map cdr mix))
  (list
   (interact-m-itself aus num)
   (interact-m-r aus num (H))
   (interact-m-r aus num (D))
   
   ))

(define (reverse-p pair)
  (match-define (cons a b) pair)
  (cons b a))
  
(define (create-matrix-m mix)
  (define aus (map car mix))
  (define num (map cdr mix))
  (cons
   (benchmark-m mix)
   (for/list ([i (in-list BENCHMARKS)])
     (cons (interact-m i aus num)
           (interact-g i BENCHMARKS)))))
  
  
|#
