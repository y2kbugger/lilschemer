#lang racket/base

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

'member
(define (member? a lat)
  (cond
    ((null? lat) #f)
    ((eq? a (car lat)) #t)
    (else (member? a (cdr lat)))))


'rember
(define (rember a lat)
  (cond
    ((null? lat) '())
    (else (cond
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat)
        (rember a (cdr lat))))))))

(rember 'dog '(moose cat chicken dog fish))
(rember 'fun '(moose cat chicken dog fish))

(define (rember2 a lat)
  (cond
    ((null? lat) '())
    ((eq? a (car lat)) (cdr lat))
    (else (cons (car lat)
      (rember2 a (cdr lat))))))

(rember2 'dog '(moose cat chicken dog fish))
(rember2 'fun '(moose cat chicken dog fish))

'firsts
(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons (car (car l)) (firsts (cdr l))))))

(firsts '((a b) (c d) (e f)))

'insertR
(define (insertR a b l)
  (cond
    ((null? l)
      '())
    ((eq? a (car l))
      (cons (car l) (cons b (cdr l))))
    (else (cons (car l)
      (insertR a b (cdr l))))))

(insertR 'peanut 'butter '(a peanut and jelly sandwich))
(insertR 'peanut 'butter '(a peanut and jelly sandwich with crunchy peanut pieces))

'insertL
(define (insertL a b l)
  (cond
    ((null? l)
      '())
    ((eq? a (car l))
      (cons b l))
    (else (cons (car l)
      (insertL a b (cdr l))))))

(insertL 'peanut 'big '(a peanut and jelly sandwich))
(insertL 'peanut 'big '(a peanut and jelly sandwich with crunchy peanut pieces))

'subst
(define (subst a b l)
  (cond
    ((null? l)
      '())
    ((eq? b (car l))
      (cons a (cdr l)))
    (else (cons (car l)
      (subst a b (cdr l))))))

(subst 'walnut 'peanut '(a peanut and jelly sandwich))
(subst 'walnut 'peanut '(a peanut and jelly sandwich with crunchy peanut pieces))

'subst2
(define (subst2 new a b l)
  (cond
    ((null? l)
      '())
    ((or
        (eq? a (car l))
        (eq? b (car l)))
      (cons new (cdr l)))
    (else (cons (car l)
      (subst2 new a b (cdr l))))))

(subst2 'walnut 'jelly 'peanut '(a peanut and jelly sandwich))
(subst2 'walnut 'jelly 'a '(a peanut and jelly sandwich with crunchy peanut pieces))

'multirember
(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? a (car lat)) (multirember a (cdr lat)))
    (else (cons (car lat)
      (multirember a (cdr lat))))))

(multirember 'dog '(moose cat chicken dog fish dog cat hog dog fucking her))


'multiinsertR
(define (multiinsertR a b l)
  (cond
    ((null? l)
      '())
    ((eq? a (car l))
      (cons (car l) (cons b (multiinsertR a b (cdr l)))))
    (else (cons (car l)
      (multiinsertR a b (cdr l))))))

(multiinsertR 'peanut 'butter '(a peanut and jelly sandwich))
(multiinsertR 'peanut 'butter '(a peanut and jelly sandwich with crunchy peanut pieces))

'multiinsertL
(define (multiinsertL a b l)
  (cond
    ((null? l)
      '())
    ((eq? a (car l))
      (cons b (cons a (multiinsertL a b (cdr l)))))
    (else (cons (car l)
      (multiinsertL a b (cdr l))))))

(multiinsertL 'peanut 'big '(a peanut and jelly sandwich))
(multiinsertL 'peanut 'big '(a peanut and jelly sandwich with crunchy peanut pieces))

'multisubst
(define (multisubst a b l)
  (cond
    ((null? l)
      '())
    ((eq? b (car l))
      (cons a (multisubst a b (cdr l))))
    (else (cons (car l)
      (multisubst a b (cdr l))))))

(multisubst 'walnut 'peanut '(a peanut and jelly sandwich))
(multisubst 'walnut 'peanut '(a peanut and jelly sandwich with crunchy peanut pieces))
