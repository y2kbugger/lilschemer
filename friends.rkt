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

(define (t? actual_exp expected)
  (cond
    ((equal? (eval actual_exp ns) expected) (display "."))
    (else
      (display "f")
      (display "\nInput:\n\t")
      (display actual_exp)
      (display "\nActual:\n\t")
      (write (eval actual_exp ns))
      (display "\nExpected:\n\t")
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

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
        (o+ (car tup) (addtup (cdr tup)))))))

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

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? '+ (cadr nexp)) (o+ (value (car nexp)) (value (caddr nexp))))
    ((eq? 'x (cadr nexp)) (x (value (car nexp)) (value (caddr nexp))))
    ((eq? '^ (cadr nexp)) (^ (value (car nexp)) (value (caddr nexp))))))

(define (member? a lat)
  (cond
    ((null? lat) #f)
    ((eq? a (car lat)) #t)
    (else (member? a (cdr lat)))))

(define (set? lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))

(test (set? '(apple peaches apple plum)) #f)
(test (set? '(apple peaches pears plum)) #t)
(test (set? '()) #t)
(test (set? '(5 apple 7 peaches 3 pears 2 plum)) #t)
(test (set? '(5 apple 7 peaches 5 pears 2 plum)) #f)

(define (makeset lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
    (else 
      (cons (car lat) (makeset (cdr lat))))))

(test (makeset '(apple peaches apple plum)) '(peaches apple plum))
(test (makeset '(apple peaches pears plum)) '(apple peaches pears plum))
(test (makeset '()) '())
(test (makeset '(5 apple 7 peaches 3 pears 2 plum)) '(5 apple 7 peaches 3 pears 2 plum))
(test (makeset '(5 apple 7 peaches 5 pears 2 plum)) '(apple 7 peaches 5 pears 2 plum))

(define (subset? set1 set2)
  (cond
    ((null? set1) #t)
    ((not (member? (car set1) set2)) #f)
    (else (subset? (cdr set1) set2))))

(test (subset? '(5 chicken wings) '(5 hamburgers 2 pieces of fried chicken and light duckling wings)) #t)
(test (subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish)) #f)


(define (eqset? set1 set2)
  (and
    (subset? set1 set2)
    (subset? set2 set1)))

(test (eqset? '(5 apple 7 peaches 3 pears 2 plum) '(5 apple 7 peaches 3 pears 2 plum)) #t)
(test (eqset? '(9 apple 7 peaches 3 pears 2 plum) '(5 apple 7 peaches 3 pears 2 plum)) #f)


(define (intersect? set1 set2)
  (cond
    ((null? set1) #f)
    ((member? (car set1) set2) #t)
    (else (intersect? (cdr set1) set2))))

(t? '(intersect? '(we like to eat apples) '(5 apples 7 peaches 3 pears 2 plum)) #t)
(t? '(intersect? '(we like to eat cake) '(5 apples 7 peaches 3 pears 2 plum)) #f)


(define (intersect set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2))))

(t? '(intersect '(stewed tomatos and macroni) '(macroni and cheese)) '(and macroni))
(t? '(intersect '(we like to eat cake) '(5 apples 7 peaches 3 pears 2 plum)) '())


(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2) (union (cdr set1) set2))
    (else (cons (car set1) (union (cdr set1) set2)))))

(t? '(union '(stewed tomatos and macroni) '(macroni and cheese)) '(stewed tomatos macroni and cheese))

(define (intersectall l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else
      (intersect (car l-set) (intersectall (cdr l-set))))))

(t? '(intersectall '((a b c) (c a d e) (e f g h a b))) '(a))

(define (a-pair? x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cddr x)) #t)
    (else #f)))

(t? '(a-pair? '(pear pear)) #t)
(t? '(a-pair? '(3 7)) #t)
(t? '(a-pair? '(full (house))) #t)
(t? '(a-pair? '(a full (house))) #f)
(t? '(a-pair? '(a)) #f)

(define (first p)
  (car p))

(define (second p)
  (cadr p))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

; (define (rel? 

; (t? (rel? '(apples peaches pumpkin pie)) #f)
; (t? (rel? '((apples peaches) (pumpkin pie) (apples peaches))) #f)
; (t? (rel? '((pumpkin pie) (apples peaches))) #t)
; (t? (rel? '((4 3) (4 2) (7 6) (6 2) (3 4))) #t)

(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons (car (car l)) (firsts (cdr l))))))

(define (fun? rel)
  (set? (firsts rel)))

(t? '(fun? '((4 3) (4 2) (7 6) (6 2) (3 4))) #f)
(t? '(fun? '((8 3) (4 2) (7 6) (6 2) (3 4))) #t)

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else
      (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel))))))

(t? '(revrel '((8 a) (pumpkin pie) (got sick))) '((a 8) (pie pumpkin) (sick got)))

