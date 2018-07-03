#lang racket
(require racket/hash "util.rkt" "cons.rkt")
(provide (all-defined-out))

(define ACTIONS# 2)
(define ACTIONS (list 'H 'D))
(define (random-action)
  (random ACTIONS#))

(struct automaton (head body) #:transparent)
(struct state (action dispatch) #:transparent)
(define (make-random-automaton states#)
  (define init (random states#))
  (define to-detach (random states#))
  (define (make-head) (hash 'INIT init
                            'PAY 0))
  (define ids (build-list states# values))
  (define (make-body) 
    (apply hash (flatten (map list ids (make-states)))))
  (define (make-states)
    (build-list states# make-state))
  (define (make-state _) (state (random-action) (make-transition)))
  (define (make-transition)
    (hash 0 (random states#)
          1 (random states#)
          ))
  (automaton (make-head) (make-body)))

(define (reset a)
  (match-define (automaton head body) a)
  (define new-head (hash-set head 'PAY 0))
  (automaton new-head body))

(define (flatten-automaton a)
  (match-define (automaton head body) a)
  (define flatten-head
    (list
     (hash-ref head 'PAY)
     (hash-ref head 'INIT)))
  (define l (hash-count body))
  (define (flatten-state s)
    (match-define (state action dispatch) s)
    (list 
     action
     0
     (hash-ref dispatch 0)
     1
     (hash-ref dispatch 1)
     ))
  (define flatten-body
    (for/list ([i (in-range l)])
      (list
       i
       (flatten-state (hash-ref body i)))))
  (flatten (append flatten-head flatten-body)))

;; CLASICS AUTOMATA

(define (H)
  (automaton (hash 'INIT 0 'PAY 0)
             (hash 0 (state 0 (hash 0 0 1 0)))))
(define (D)
  (automaton (hash 'INIT 0 'PAY 0)
             (hash 0 (state 1 (hash 0 0 1 0)))))

;; IMMUTABLE MUTATION

(define (mutate-marginally a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (define mutate-initial (random l))
  (define mutate-state (random l))
  (match-define (state action dispatch) 
                (hash-ref body mutate-state))
  (define r (random 3))
  (define new-head
    (cond [(zero? r) 
           (hash-set head 'INIT mutate-initial)]
          [else head])) ; leave head unchanged (change body)
  (define new-body
    (cond [(zero? r) body] ; leave body unchanged (change head)
          [(= r 1)
           (hash-set body mutate-state ; mutate the action in the state
                     (state (random-action) dispatch))]
          [(= r 2)
           (hash-set body mutate-state ; mutate the dispatching rule in the state
                     (state action
                            (hash-set dispatch (random-action) (random l))))]))
  (automaton new-head new-body))

(define (add-state a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (define (make-transition)
    (hash 0 (random (+ l 1))
          1 (random (+ l 1))
          ))
  (define (make-state) (state (random-action) (make-transition)))
  (define mutate-state (random l))
  (match-define (state action dispatch) (hash-ref body mutate-state))
  (define new-body                                                              
    (hash-union
     (hash-set body mutate-state
               (state action
                      (hash-set dispatch (random-action) l)))
     (hash l (make-state))))
  (automaton head new-body))

(define (random-mem l)
  (list-ref l (random (length l))))

(define (detach-state a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (cond
   [(= l 1) (mutate-marginally a)]
   [else (begin
           (define (random-but n r)
             (random-mem (remq mutate-state (build-list n values))))
           (define mutate-state (random l))
           (define (check-rule rule)
             (match-define (cons opponent-action reaction) rule)
             (if (= mutate-state reaction)
                 (cons opponent-action (random-but l mutate-state))
                 rule))
           (define (check-dispatch rules)
             (apply hash
                    (flatten
                     (map check-rule (hash->list rules)))))
           (define (check-state a-state)
             (match-define (state action rules) a-state)
             (struct-copy state a-state [dispatch (check-dispatch rules)]))
           (define new-body                                                     
             (for/list([i (in-range l)])
               (list i
                     (check-state (hash-ref body i)))))
           (automaton head (apply hash (flatten new-body))))]))

(define (mutate a)
  (define r (random 3))
  (cond [(zero? r) (mutate-marginally a)]
        [(= r 1) (add-state a)]
        [(= r 2) (detach-state a)]))

(define (mutates au n)
  (cond [(zero? n) '()]
        [else
         (define new (mutate au))
         (cons au (mutates new (- n 1)))]))

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
  
  
