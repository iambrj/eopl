#lang racket

(define (listmap fn lst)
  (cond
    [(empty? lst) empty]
    [else (cons (fn (car lst)) (listmap fn (cdr lst)))]))

(define (repeat n x)
  (cond
    [(= n 0) empty]
    [(> n 0) (cons x (repeat (- n 1) x))]
    [else (error "Bad n")]))

(define (invert lst)
  (cond
    [(empty? lst) empty]
    [else (cons (cons (car (cdr (car lst))) (car (car lst))) (invert (cdr lst)))]))

(define (count-occurrences s slist) 
  (cond
    [(empty? slist) 0]
    [else (+ (if (= s (car slist)) 1 0) (count-occurrences s (cdr slist)))]))

(define (product sos1 sos2)
  (cond
    [(and (empty? sos1) (empty? sos2)) empty]
    [(empty? sos1) sos2]
    [(empty? sos2) sos1]
    [(= (length sos1) 1) (listmap (lambda (x) (cons (car sos1) x)) sos2)]
    [else (append (listmap (lambda (x) (cons (car sos1) x)) sos2) (product (cdr sos1) sos2))]))

(define (every? pred lst)
  (cond
    [(empty? lst) #t]
    [else (and (pred (car lst)) (every? pred (cdr lst)))]))

(define (merge loi1 loi2)
  (cond
    [(empty? loi1) loi2]
    [(empty? loi2) loi1]
    [else
      (cond
        [(> (car loi1) (car loi2)) (cons (car loi2) (merge loi1 (cdr loi2)))]
        [else (cons (car loi1) (merge (cdr loi1) loi2))])]))

(define (flatten dlst)
  (cond
    [(empty? dlst) empty]
    [else
      (cond
        [(pair? dlst) (append (flatten (car dlst)) (flatten (cdr dlst)))]
        [else (list dlst)])]))
