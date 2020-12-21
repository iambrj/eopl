#lang racket
(provide cps-letrec)

(define (ext-env u v env)
  (lambda (x)
    (if (eq? x u)
      v
      (env x))))

(define empty-env
  (lambda (x)
    (error x ": missing binding")))

(define init-env
  (foldr ext-env
         empty-env
         '(+ - * /)
         `(,(lambda (u v) (+ u v))
            ,(lambda (u v) (- u v))
            ,(lambda (u v) (* u v))
            ,(lambda (u v) (/ u v)))))

; Leads to dirty behaviour when let binding let, quote, lambda etc
; Not Dijkstra guards
(define (cps-letrec expr [env init-env])
  (match expr
    [(? number? expr) expr]
    [(list 'quote expr) expr]
    [(? symbol? expr) (env expr)]
    [(list 'let (list bindings ...) body)
      (cps-letrec body (foldr (lambda (binding env)
                                (ext-env (first binding)
                                         (cps-letrec (second binding) env)
                                         env))
                              env
                              bindings))]
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
