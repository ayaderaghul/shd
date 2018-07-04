#lang racket

(require racket/hash "util.rkt" )

(provide (all-defined-out))

(define ACTIONS# 2)
(define ACTIONS (list 'H 'D))
(define (random-action)
  (random ACTIONS#))

(struct player (payoff dash node) #:transparent)
;; MUTATION
;; mutate action or mutate the ignorance state
;; CHI THIS IS NTH TIMES I REPEAT THIS
;; NO MATTER WHAT TEMPTING JUSTIFICATION YOU START WITH
;; MUTATION ALWAYS !!! LEADS TO SUFFERING
;; define an immutable mutation

(define (new-n new-nodes old-nodes)
  (apply hash (flatten (append new-nodes old-nodes))))
(define (set-a-into-d a d n v)
  (define new-d-nodes
    (for/list ([i (in-list d)])
      (list i a)))
;;  (print "after new d nodes")
  (define old (remove* d v))
  (define old-nodes
    (for/list ([i old])
      (list i (hash-ref n i))))
;;  (print "after old-nodes")
  (new-n new-d-nodes old-nodes))
(define (set-a-into-node r a n v)
  (define new-node (list r a))
  (define old (remove r v))
  (define old-nodes
    (for/list ([i old])
      (list i (hash-ref n i))))
  (new-n new-node old-nodes))

;; 4 nodes: 0 1 2 3
;; 6 possible dashes: 0-1 0-2 0-3 1-2 1-3 2-3
;; randomise over this to choose which dashes are activated

(define (activate-these-dashes nodes#)
  (define dash-list (dashes nodes#))
  (define dashes# (length dash-list))
;;  (print "after dash-list")
  (define r
    (random (+ dashes# 1)))
;;  (print "after random")
  (define connect-this-many
    (if (and (> dashes# 5) (> r 4))
        (- r 2)
        r))
;;  (print connect-this-many)
  (define (dash-ids counter ids a-list)
    (cond [(zero? counter) ids]
          [else
           (define id (random-member a-list))
;;           (print "after id")
           (dash-ids (- counter 1) (cons id ids)
                     (remove id a-list))]))
  (sort (dash-ids connect-this-many '() dash-list) #:key car <))
    
(define (make-informed-player nodes#)
  (define nodes
    (for/list ([i (in-range nodes#)])
      (list i (random ACTIONS#))))
  (player 0 '() (apply hash (flatten nodes))))

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
;;         (print "after to-act")
         (cond [(null? to-activate) pl]
               [else
         ;;       (map print to-activate)
                (define a (hash-ref n (first to-activate)))
          ;;      (print "after a")
                (define v (build-list nodes# values))
                (define new-nodes
                  (set-a-into-d a to-activate n v))
         ;;       (print "after new-nodes")
                (player p to-activate new-nodes)])]))

(define (make-player nodes#)
  (define pl (make-informed-player nodes#))
;;  (print "after making informed")
  (uninform-player pl nodes#))

;(define (p1) (make-player 1))
;(define (p2) (make-player 2))
;(define (p3) (make-player 4))

;(define pl1 (p1))
;(define pl2 (p2))
;(define pl3 (p3))

(define (reset pl)
(match-define (player p d n) pl)
(player 0 d n))      


(define (mutate-action pl)
;;  (print "mutate action")
  (match-define (player p d n) pl)
  (define nodes# (hash-count n))
  (define v (build-list nodes# values))
  (define r (random nodes#))
  (define a (random ACTIONS#))
  (cond [(member r d)
         (player p d (set-a-into-d a d n v))]
        [else
         (player p d (set-a-into-node r a n v))]))

;; mutate ignorance state

(define (delete ls counter)
  (define len (length ls))
  (cond [(zero? counter) ls]
        [else
         (delete (remove (list-ref ls (random len)) ls) (- counter 1))]))

(define (delete-node a-list)
  (define l (length a-list))
  (define can-delete (random (+ 1 l)))
  (define l2 (delete a-list can-delete))
  (if (= 1 (length l2)) '() l2))

(define (add ls counter from)
  (define len (length from))
  (cond [(zero? counter) ls]  
        [else
         (define r (random len))
         (define s (list-ref from r))
         (add (cons s ls) (- counter 1) (remove s from))]))
  
(define (add-node a-list full)
 ;; (print "add-node")
  (define can-add (remove* a-list full))
;;(print "remove*")
  (define l (length can-add))
  (define r# (random (+ 1 l)))
  (add a-list r# can-add))
  

(define (mutate-ignorance pl)
 ;; (print "mutate ignorance")
  (match-define (player p d n) pl)
  (define nodes# (hash-count n))
  (define v (build-list nodes# values))
  (define l (length d))
  (define new-d
    (cond [(= l nodes#) (delete-node d)]
          [(= l 0)
           (define r (random nodes#))
           (define r2 (random-member (remove r v)))
           (list r r2)]
          [(= l 2)
           (cons (random-member (remove* d v)) d)]
          [else
           (define r3 (random 2))
           (if (zero? r3) (add-node d v) (delete-node d))]))
  (cond [(null? new-d) (player p new-d n)]
        [else
         (define a (first new-d))
         (define act (hash-ref n a))
         (define new-nodes
           (set-a-into-d act new-d n v))
         (player p new-d new-nodes)]))
           

(define (mutate pl)
  (match-define (player p d n) pl)
  (if (= 1 (hash-count n)) (mutate-action pl)
      (if (zero? (random 2)) (mutate-action pl) (mutate-ignorance pl))))

;; IMMUTABLE MUTATION

(define PAYOFF-TABLE
  (list
   (list
    (list (list -8/3 -8/3 -8/3) (list -1 -1 0))
    (list (list -1 0 -1) (list 4 0 0)))
   (list
    (list (list 0 -1 -1) (list 0 4 0))
    (list (list 0 0 4) (list 4/3 4/3 4/3)))
   ))

(define PAYOFF-TABLE1
  (list
   (list
    (list (list 1 1 1) (list 6 6 9))
    (list (list 6 9 6) (list 21 9 9)))
   (list
    (list (list 9 6 6) (list 9 21 9))
    (list (list 9 9 21) (list 13 13 13)))
   ))

(define (payoff action1 action2 action3)
  (list-ref
   (list-ref
    (list-ref PAYOFF-TABLE action1)
    action2)
  action3))

(define (convert a1 a2)
  (cond [(and (zero? a1) (zero? a2)) 0]
        [(and (zero? a1) (= 1 a2)) 1]
        [(and (= 1 a1) (zero? a2)) 2]
        [(and (= 1 a1) (= 1 a2)) 3]))
        
(define (interact au1 au2 au3)
  (match-define (player p1 d1 n1) au1)
  (match-define (player p2 d2 n2) au2)
  (match-define (player p3 d3 n3) au3)
  (define a1 (hash-ref n1 0))
  (define a2 (hash-ref n2 a1))
  (define a3 (hash-ref n3 (convert a1 a2)))
  (match-define (list pay1 pay2 pay3) (payoff a1 a2 a3))
  (list
   (player pay1 d1 n1)
   (player pay2 d2 n2)
   (player pay3 d3 n3)))

#|
;;benchmark

(define BENCHMARKS (list (H) (D)))

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
