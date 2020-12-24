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
         `(,(lambda (k u v) (k (+ u v)))
            ,(lambda (k u v) (k (- u v)))
            ,(lambda (k u v) (k (* u v)))
            ,(lambda (k u v) (k (/ u v)))
            ,(lambda (k x) (k (= x 0)))
            ,(lambda (k x) (k (not x))))))

; Leads to dirty behaviour when let binding let, quote, lambda etc
(define (cps-letrec expr [env init-env] [k identity])
  (match expr
    [(? number? expr) (k expr)]
    [(? boolean? expr) (k expr)]
    [`(quote ,expr) (k expr)]
    [(? symbol? expr) (k (env expr))]
    [`(if ,condition ,then-clause ,else-clause)
     (cps-letrec condition env
                 (lambda (b)
                   (if b
                     (cps-letrec then-clause env k)
                     (cps-letrec else-clause env k))))]
    [`(let (,bindings ...) ,body)
      (letrec ([env-builder
                 (lambda (bindings env-so-far)
                   (match bindings
                     ['() (cps-letrec body env-so-far k)]
                     [`((,x ,e) . ,rest)
                       (cps-letrec e
                                   env
                                   (lambda (v)
                                     (env-builder rest
                                                  (ext-env x v env-so-far))))]))])
        (env-builder bindings env))]
    [`(letrec (,bindings ...) ,body)
     (letrec ([env-builder
                (lambda (bindings env-so-far)
                  (match bindings
                    ['() (cps-letrec body env-so-far k)]
                    [`((,x ,e) . ,rest)
                      (cps-letrec e
                                  (rec-ext-env bindings env)
                                  (lambda (v)
                                    (env-builder rest
                                                 (ext-env x v env-so-far))))]))])
       (env-builder bindings env))]
    [`(lambda (,args ...) ,body)
     (lambda (host-args)
       (cps-letrec body
                   (foldr ext-env
                          env
                          args
                          host-args)))]
    [`(,rator ,rands ...)
     (apply (cps-letrec rator env)
            (map (lambda (rand) (cps-letrec rand env k)) rands))]))
