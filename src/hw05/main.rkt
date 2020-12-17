#lang racket

(define-syntax colist
  (syntax-rules ()
    [(colist sym terms ...)
     (lambda () (list sym terms ...))]))

;;; unroll :: term? -> term?
(define (unroll t)
  (if (procedure? t) (t) t))

;;; hd :: term? -> symbol?
(define (hd t)
  (car (unroll t)))

;;; tl :: term? -> term?
(define (tl t)
  (cdr (unroll t)))
(struct exn:type-error exn:fail ())

(define (raise-type-error)
  (raise (exn:type-error
          "Invalid Arguments"
          (current-continuation-marks))))

;;; term? -> term? -> set? -> boolean?
(define (bisimilar?-aux t1 t2 asked)
  (cond
    [(not (or (procedure? t1) (list? t1))) (raise-type-error)]
    [(not (or (procedure? t2) (list? t2))) (raise-type-error)]
    [(equal? t1 '()) (raise-type-error)]
    [(equal? t2 '()) (raise-type-error)]
    [(equal? t1 t2) #t]
    [else
       (and
         (or
           (equal? (hd t1) (hd t2))
           (set-member? asked (set (hd t1) (hd t2))))
         (cond
           [(and (empty? (tl t1)) (empty? (tl t2))) #t]
           [else (bisimilar?-aux (tl t1) (tl t2) (set-add asked (set (tl t1) (tl t2))))]))]))

;;; term? -> term? -> boolean?
(define (bisimilar? t1 t2)
  (cond
    [(not (or (procedure? t1) (list? t1))) (raise-type-error)]
    [(not (or (procedure? t2) (list? t2))) (raise-type-error)]
    [else (bisimilar?-aux (cons (hd t1) (tl t1)) (cons (hd t2) (tl t2))
                          (set (set t1 t2)))]))

(provide bisimilar?)
(provide colist)
(provide (struct-out exn:type-error))
