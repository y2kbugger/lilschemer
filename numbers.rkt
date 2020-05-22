#lang racket/base

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

(define add1
  (lambda (x) (+ x 1)))

(define sub1
  (lambda (x) (- x 1)))

(add1 4)
(sub1 4)

'o+
(define o+
  (lambda (a b)
    (cond
      ((zero? a) b)
      (else (o+ (sub1 a) (add1 b))))))

(o+ 3 4)

'o-
(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (o- (sub1 a) (sub1 b))))))

(o- 14 3)
(o- 17 9)

'addtup
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
        (o+ (car tup) (addtup (cdr tup)))))))

(addtup '(3 5 6))

'x
(define x
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (o+ m (x m (sub1 n)))))))

(x 4 5)

'tup+
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2) '()))
      (else
        (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
(tup+ '(3 4) '(1 1))

'>
(define >
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (> (sub1 m) (sub1 n))))))

(> 3 3)
(> 12 133)
(> 120 11)

'<
(define <
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (< (sub1 m) (sub1 n))))))

(cons (< 4 6) '(#t))
(cons (< 8 3) '(#f))
(cons (< 6 6) '(#f))

; quick way to see if a function works as expected
(define (test actual expected)
  (cond
    ((equal? actual expected) (display "."))
    (else (display "f"))
  )
  (display "  ")
  (write actual)
  (display " : ")
  (write expected)
  (display "\n")
)

(test (< 4 6) #t)
(test (< 8 3) #f)
(test (< 6 6) #f)

(define (= n m)
  (cond
    ((< n m) #f)
    ((> n m) #f)
    (else #t)))

(test (= 1 1) #t)
(test (= 3 1) #f)
(test (= 3 5) #f)


(define (^ n m)
  (cond
    ((zero? m) 1)
    (else
      (x n (^ n (sub1 m))))))

(test (^ 1 1) 1)
(test (^ 2 3) 8)
(test (^ 5 3) 125)

'div
(define (div n m)
  (cond
    ((< n m) 0)
    (else
      (add1 (div (- n m) m)))))


(test (div 12 3) 4)


'length
(define (len lat)
  (cond
    ((null? lat) 0)
    (else
      (add1 (len (cdr lat))))))

(test (len '(hotdogs with mustard)) 3)

'pick
(define (pick n lat)
  (cond
    ((zero? (sub1 n)) (car lat))
    (else (pick (sub1 n) (cdr lat)))))

(test (pick 3 '(hotdogs with mustard and sauce)) 'mustard)

'rempick
(define (rempick n lat)
  (cond
    ((zero? (sub1 n)) (cdr lat))
    (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(test (rempick 3 '(hotdogs with mustard and sauce)) '(hotdogs with and sauce))


(test (number? 'tomato) #f)
(test (number? 5) #t)


'no-nums
(define (no-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat))
     (no-nums (cdr lat)))
    (else
      (cons (car lat) (no-nums (cdr lat))))))

(test (no-nums '(5 shits 9 giggles 12 fools-on-hill)) '(shits giggles fools-on-hill))

'all-nums
(define (all-nums lat)
  (cond
    ((null? lat) '())
    ((not (number? (car lat)))
     (all-nums (cdr lat)))
    (else
      (cons (car lat) (all-nums (cdr lat))))))

(test (all-nums '(5 shits 9 giggles 12 fools-on-hill)) '(5 9 12))

'eqan
(define (eqan a b)
  (cond
    ((and (number? a) (number? b))
     (= a b))
    ((or (number? a) (number? b))
     #f)
    (else
      (eq? a b))))

'occur
(define (occur a lat)
  (cond
    ((null? lat) 0)
    ((eq? a (car lat))
     (add1 (occur a (cdr lat))))
    (else
      (occur a (cdr lat)))))

(test (occur 'know '(do i need to know what i i know)) 2)
(test (occur 'i '(do i need to know what i i know)) 3)

'one?
(define (one? n)
  (= 1 n))

(test (one? 1) #t)
(test (one? 3) #f)


