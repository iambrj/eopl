#lang racket
(provide cps-letrec)

(define (ext-env u v env)
  (lambda (x)
    (if (eq? x u)
      v
      (env x))))

; pass the entire list?
(define (rec-ext-env bindings env)
  (lambda (x)
    (let ([p (assoc x bindings)])
      (if p
        (cps-letrec (second p) (rec-ext-env bindings env))
        (env x)))))

(define empty-env
  (lambda (x)
    (error x ": missing binding")))

(define init-env
  (foldr ext-env
         empty-env
         '(+ - * / zero? not)
         `(,(lambda (u v) (+ u v))
            ,(lambda (u v) (- u v))
            ,(lambda (u v) (* u v))
            ,(lambda (u v) (/ u v))
            ,(lambda (x) (= x 0))
            ,(lambda (x) (not x)))))

; Leads to dirty behaviour when let binding let, quote, lambda etc
; Not Dijkstra guards
(define (cps-letrec expr [env init-env])
  (match expr
    [(? number? expr) expr]
    [(? boolean? expr) expr]
    [(list 'quote expr) expr]
    [(? symbol? expr) (env expr)]
    [(list 'if condition then-clause else-clause)
     (if (cps-letrec condition env)
       (cps-letrec then-clause env)
       (cps-letrec else-clause env))]
    [(list 'let (list bindings ...) body)
     (cps-letrec body (foldr (lambda (binding env)
                               (ext-env (first binding)
                                        (cps-letrec (second binding) env)
                                        env))
                             env
                             bindings))]
    [(list 'letrec (list bindings ...) body)
     (cps-letrec body (rec-ext-env bindings env))]
    [`(lambda ,(list args ...) ,body)
      (lambda host-args
        (cps-letrec body
                    (foldr ext-env
                           env
                           args
                           host-args)))]
    [(list rator rands ...)
     (apply (cps-letrec rator env)
            (map (lambda (rand) (cps-letrec rand env)) rands))]))
