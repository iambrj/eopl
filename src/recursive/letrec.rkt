#lang racket
(provide letrec-eval)

(define (ext-env u v env)
  (lambda (x)
    (if (eq? x u)
      v
      (env x))))

(define (set-env! bind val env)
  (set-box! (env bind) val))

(define empty-env
  (lambda (x)
    (error x ": missing binding")))

(define init-env
  (foldr ext-env
         empty-env
         '(printf + - * / zero? not)
         `(,printf
            ,(lambda (u v) (+ u v))
            ,(lambda (u v) (- u v))
            ,(lambda (u v) (* u v))
            ,(lambda (u v) (/ u v))
            ,(lambda (x) (= x 0))
            ,(lambda (x) (not x)))))

; Not Dijkstra guards
(define (letrec-eval expr [env init-env])
  (match expr
    [(? number? expr) expr]
    [(? boolean? expr) expr]
    [(? string? expr) expr]
    [`(quote ,expr) expr]
    [(? symbol? expr)
     (let ([val (env expr)])
       (if (box? val)
         (unbox val)
         val))]
    [`(if ,condition ,then-clause ,else-clause)
     (if (letrec-eval condition env)
       (letrec-eval then-clause env)
       (letrec-eval else-clause env))]
    [`(let (,bindings ...) ,body)
     (letrec-eval body (foldr (lambda (binding env)
                               (ext-env (first binding)
                                        (letrec-eval (second binding) env)
                                        env))
                             env
                             bindings))]
    [`(letrec (,bindings ...) ,body) ; assumes unique identifiers
      (letrec-eval body
                   (let ([rec-env (foldr (lambda (binding env-so-far)
                                           (ext-env (first binding)
                                                    (box 'undefined)
                                                    env-so-far))
                                         env
                                         bindings)])
                     (let ([rec-vals (map (lambda (binding)
                                            (letrec-eval (second binding)
                                                         rec-env))
                                          bindings)])
                       (map (lambda (binding val)
                              (set-env! (first binding) val rec-env))
                            bindings
                            rec-vals))
                     rec-env))]
    [`(lambda (,args ...) ,body)
      (lambda host-args
        (letrec-eval body
                    (foldr ext-env
                           env
                           args
                           host-args)))]
    [`(,rator ,rands ...)
     (apply (letrec-eval rator env)
            (map (lambda (rand) (letrec-eval rand env)) rands))]))
