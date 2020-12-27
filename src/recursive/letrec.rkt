#lang racket
(provide letrec-eval)

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
        ; Incorrect! This evaluates each time the binding is called, semantics
        ; specify single evaluation
        (letrec-eval (second p) (rec-ext-env bindings env))
        (env x)))))

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

; Leads to dirty behaviour when let binding let, quote, lambda etc
; Not Dijkstra guards
(define (letrec-eval expr [env init-env])
  (match expr
    [(? number? expr) expr]
    [(? boolean? expr) expr]
    [(? string? expr) expr]
    [`(quote ,expr) expr]
    [(? symbol? expr) (env expr)]
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
    [`(letrec (,bindings ...) ,body) ; assumes unique identifier
      (define rec-env (map (lambda (binding)
                             (cons (car binding) 'undefined))
                           bindings))
      (letrec-eval body
                   (begin
                     (map (lambda (binding pos)
                            (let ([val (letrec-eval (second binding) rec-env)])
                              (set! rec-env (list-set rec-env pos (cons (car binding) val)))))
                          bindings
                          (build-list (length bindings) values))
                     (foldr (lambda (binding env-so-far)
                              (ext-env (car binding)
                                       (cdr binding)
                                       env-so-far))
                            env
                            rec-env)))]
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
