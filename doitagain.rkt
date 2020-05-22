#lang racket/base

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

(lat? '(a b c))
(lat? '(a b (d) c))


'member
(define (member? a lat)
  (cond
    ((null? lat) #f)
    ((eq? a (car lat)) #t)
    (else (member? a (cdr lat)))))

(member? 'd '(a b c))
(member? 'c '(a b c))



