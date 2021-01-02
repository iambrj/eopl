#lang racket

(provide cps-letrec
         init-env)

(define (ext-env u v env)
  (lambda (x)
    (if (eq? x u)
      v
      (env x))))

(define (set-env! u v env)
  (set-box! (env u) (box v)))

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
(define (cps-letrec expr env k)
  (match expr
    [`(+ ,x ,y)
      (cps-letrec x env
                  (lambda (x)
                    (cps-letrec y env
                                (lambda (y)
                                  (k (+ x y))))))]
    [`(- ,x ,y)
      (cps-letrec x env
                  (lambda (x)
                    (cps-letrec y env
                                (lambda (y)
                                  (k (- x y))))))]
    [`(* ,x ,y)
      (cps-letrec x env
                  (lambda (x)
                    (cps-letrec y env
                                (lambda (y)
                                  (k (* x y))))))]
    [`(/ ,x ,y)
      (cps-letrec x env
                  (lambda (x)
                    (cps-letrec y env
                                (lambda (y)
                                  (k (/ x y))))))]
    [`(zero? ,x)
      (cps-letrec x env
                  (lambda (x)
                    (k (zero? x))))]
    [`(not ,x)
      (cps-letrec x env
                  (lambda (x)
                    (k (not x))))]
    [`(and ,x ,y)
      (cps-letrec x env
                  (lambda (x)
                    (cps-letrec y env
                                (lambda (y)
                                  (k (and x y))))))]
    [`(or ,x ,y)
      (cps-letrec x env
                  (lambda (x)
                    (cps-letrec y env
                                (lambda (y)
                                  (k (or x y))))))]
    [(? number? expr) (k expr)]
    [(? boolean? expr) (k expr)]
    [`(quote ,expr) (k expr)]
    [(? symbol? expr)
     (let ([val (env expr)])
       (if (box? val) (k (unbox val)) (k val)))]
    [`(if ,condition ,then-clause ,else-clause)
      (cps-letrec condition env
                  (lambda (b)
                    (if b
                      (cps-letrec then-clause env k)
                      (cps-letrec else-clause env k))))]
    ; Wrong? Need to eval body, then pass result to continuation that creates a
    ; lambda expression
    [`(lambda (,args ...) ,body)
      (k (lambda (cont . host-args)
           (letrec ([env-builder
                      (lambda (args host-args env-so-far)
                        (match host-args
                          ['() (cps-letrec body env-so-far cont)]
                          [`(,a . ,d)
                            (cps-letrec a
                                        env
                                        (lambda (a-val)
                                          (env-builder (cdr args)
                                                       d
                                                       (ext-env (car args)
                                                                a-val
                                                                env-so-far))))]))])
             (env-builder args host-args env))))]
    [`(,rator ,rands ...)
      (cps-letrec rator
                  env
                  (lambda (rator)
                    (letrec ([eval-rands
                                  (lambda (rands rand-vals)
                                    (match rands
                                      ['()
                                       (apply rator (cons k rand-vals))]
                                      [`(,a . ,rest)
                                        (cps-letrec a
                                                    env
                                                    (lambda (a-val)
                                                      (eval-rands rest
                                                                  (cons a-val
                                                                        rand-vals))))]))])
                         (eval-rands rands '()))))]
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
      (let ([rec-env (foldr (lambda (binding env-so-far)
                              (ext-env (first binding)
                                       (box 'undefined)
                                       env-so-far))
                            env
                            bindings)])
        (letrec ([rec-env-builder (lambda (bindings)
                                    (match bindings
                                      ['() (cps-letrec body rec-env k)]
                                      [`((,bind ,val) ,rest)
                                        (cps-letrec val rec-env
                                                    (lambda (val)
                                                      (set-env! bind val rec-env)
                                                      (rec-env-builder rest)))]))])
          (rec-env-builder bindings)))]))
