#lang racket/base
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

; quick way to see if a function works as expected
(define (test actual expected)
  (cond
    ((equal? actual expected) (display "."))
    (else
      (display "f")
      (display "  ")
      (write actual)
      (display " : ")
      (write expected)
      (display "\n")
    )
  )
)

; numbers
(define add1
  (lambda (x) (+ x 1)))

(define sub1
  (lambda (x) (- x 1)))

(define o+
  (lambda (a b)
    (cond
      ((zero? a) b)
      (else (o+ (sub1 a) (add1 b))))))

(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (o- (sub1 a) (sub1 b))))))

(define x
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (o+ m (x m (sub1 n)))))))

(define >
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (> (sub1 m) (sub1 n))))))

(define <
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (< (sub1 m) (sub1 n))))))

(define (^ n m)
  (cond
    ((zero? m) 1)
    (else
      (x n (^ n (sub1 m))))))

(define (numbered? aexp)
  (cond
    ((atom? aexp) (number? aexp))
    ((or
       (eq? '+ (cadr aexp))
       (eq? 'x (cadr aexp))
       (eq? '^ (cadr aexp)))
     (and
       (numbered? (car aexp))
       (numbered? (caddr aexp))))))

(test (numbered? #t) #f)
(test (numbered? 3) #t)
(test (numbered? 'cat) #f)
(test (numbered? '(3 ^ 3)) #t)
(test (numbered? '(3 + 4)) #t)
(test (numbered? '(3 + (5 x 4))) #t)
(test (numbered? '(3 + (frog x 4))) #f)

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? '+ (cadr nexp)) (o+ (value (car nexp)) (value (caddr nexp))))
    ((eq? 'x (cadr nexp)) (x (value (car nexp)) (value (caddr nexp))))
    ((eq? '^ (cadr nexp)) (^ (value (car nexp)) (value (caddr nexp))))))

(test (value 13) 13)
(test (value '(1 + 3)) 4)
(test (value '(1 + (3 ^ 4))) 82)
(test (value '(1 + (3 ^ (2 + 2)))) 82)

(define (value2 nexp)
  (cond
    ((atom? nexp) nexp)
    (else
      (define op (eval (cadr nexp) ns))
      (op (value2 (car nexp)) (value2 (caddr nexp))))))

(test (value2 13) 13)
(test (value2 '(1 + 3)) 4)
(test (value2 '(1 + (3 ^ 4))) 82)
(test (value2 '(1 + (3 ^ (2 + 2)))) 82)
