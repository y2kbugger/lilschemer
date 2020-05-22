#lang racket/base
8
(define test 99)
(+ 9 test)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'atom)
(atom? '())

; (car ())

(cons '((help) this) '(is very ((hard) to learn)))

'null
(null? '())

'eq?
(eq? 'Harry 'Harry)

(eq? 'margerine 'butter)
