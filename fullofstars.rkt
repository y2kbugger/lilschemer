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


(define (rember* a l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? a (car l)) (rember* a (cdr l)))
       (else
         (cons (car l) (rember* a (cdr l))))))
    (else
      (cons (rember* a (car l)) (rember* a (cdr l))))))


(test (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) '((coffee) ((tea)) (and (hick))))
(test (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))) '(((tomato)) ((bean)) (and ((flying)))))

(define (insertR* a b l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? b (car l))
        (cons (car l) (cons a (insertR* a b (cdr l)))))
       (else
         (cons (car l) (insertR* a b (cdr l))))))
    (else ; (car l) is not an atom
      (cons (insertR* a b (car l)) (insertR* a b (cdr l))))))

(test (insertR* 'roast 'chuck '((how much (wood))
                                could
                                ((a (wood) chuck))
                                (((chuck)))
                                (if (a) ((wood chuck)))
                                could chuck wood))
        '((how much (wood))
        could
        ((a (wood) chuck roast))
        (((chuck roast)))
        (if (a) ((wood chuck roast)))
        could chuck roast wood))

(define (occur* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))
     (cond
       ((eq? a (car l)) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
    (else
      (+ (occur* a (car l)) (occur* a (cdr l))))))

(test (occur* 'banana
              '((banana)
                (split ((((banana ice)))
                        (cream (banana))
                        sherbet))
                (banana)
                (bread)
                (banana brandy)))
      5)

(define (subst* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? old (car l))
        (cons new (subst* new old (cdr l))))
       (else
         (cons (car l) (subst* new old (cdr l))))))
    (else ; (car l) is not an atom
      (cons (subst* new old (car l)) (subst* new old (cdr l))))))

(test (subst* 'orange 'banana
              '((banana)
                (split ((((banana ice)))
                        (cream (banana))
                        sherbet))
                (banana)
                (bread)
                (banana brandy)))
      '((orange)
        (split ((((orange ice)))
                (cream (orange))
                sherbet))
        (orange)
        (bread)
        (orange brandy)))


(define (leftmost l)
  (cond
    ((null? l) '())
    ((atom? (car l)) (car l))
    (else (leftmost (car l)))))

(test (leftmost '((potato) (chips ((with) fish) (chips)))) 'potato)
(test (leftmost '(((hot) (tuna (and))) cheese)) 'hot)
