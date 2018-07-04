(require "inout.rkt")
(require "util.rkt")
(define d1 (input->numbers (csvfile->list "/home/chi/Downloads/shd/11mean1")))
(define d2 (input->numbers (csvfile->list "/home/chi/Downloads/shd/11mean2")))
(define d3 (input->numbers (csvfile->list "/home/chi/Downloads/shd/11mean3")))
(average d1)
(average d2)
(average d3)



