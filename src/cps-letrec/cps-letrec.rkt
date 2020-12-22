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
(define (cps-letrec expr [env init-env] [k identity])
  (match expr
    [(? number? expr) (k expr)]
    [(? boolean? expr) (k expr)]
    [(list 'quote expr) (k expr)]
    [(? symbol? expr) (k (env expr))]
    [(list 'if condition then-clause else-clause)
     (cps-letrec condition env
                 (lambda (b)
                   (if b
                     (cps-letrec then-clause env k)
                     (cps-letrec else-clause env k))))]
    [(list 'let (list bindings ...) body)
     (cps-letrec body
                 (foldr (lambda (binding env)
                          (ext-env (first binding)
                                   (cps-letrec (second binding) env)
                                   env))
                        env
                        bindings)
                 k)]
    [(list 'letrec (list bindings ...) body)
     (cps-letrec body (rec-ext-env bindings env) k)]
    [(list 'lambda (list args ...) body)
      (lambda (cont . host-args)
; Yes, indeed valid tail recursion!
; https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-6.html#%_sec_3.5
        (cps-letrec body
                    (foldr ext-env
                           env
                           args
                           host-args)
                    cont))]
    [(list rator rands ...)
     (k ((cps-letrec rator env)
         (map (lambda (rand) (cps-letrec rand env)) rands)))]))
