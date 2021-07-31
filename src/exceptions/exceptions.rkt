#lang racket

(provide eval-exceptions
         init-env
         init-cont
         init-cont-e)

(define (ext-env u v env)
  (lambda (x)
    (if (eq? x u)
      v
      (env x))))

(define (set-env! u v env)
  (set-box! (env u) v))

(define empty-env
  (lambda (x)
    (error x ": missing binding")))

(define init-env
  (foldr ext-env
         empty-env
         '(+ - * / zero? < not and or identity empty? eq? car cdr)
         `(,(lambda (k ke u v) (k (+ u v)))
            ,(lambda (k ke u v) (k (- u v)))
            ,(lambda (k ke u v) (k (* u v)))
            ,(lambda (k ke u v) (k (/ u v)))
            ,(lambda (k ke x) (k (= x 0)))
            ,(lambda (k ke u v) (k (< u v)))
            ,(lambda (k ke x) (k (not x)))
            ,(lambda (k ke u v) (k (and u v)))
            ,(lambda (k ke u v) (k (or u v)))
            ,(lambda (k ke u) (k u))
            ,(lambda (k ke u) (k (empty? u)))
            ,(lambda (k ke u v) (k (eq? u v)))
            ,(lambda (k ke u) (k (car u)))
            ,(lambda (k ke u) (k (cdr u))))))

(define init-cont identity)

(define init-cont-e (lambda (x) (error "Raising without installing a handler")))

; Leads to dirty behaviour when let binding let, quote, lambda etc
(define (eval-exceptions expr env k k-e)
  (match expr
    [(? number? expr) (k expr)]
    [(? boolean? expr) (k expr)]
    [`(quote ,expr) (k expr)]
    [(? symbol? expr)
     (let ([val (env expr)])
       (if (box? val) (k (unbox val)) (k val)))]
    [`(if ,condition ,then-clause ,else-clause)
      (eval-exceptions condition env
                  (lambda (b)
                    (if b
                      (eval-exceptions then-clause env k k-e)
                      (eval-exceptions else-clause env k k-e)))
                  k-e)]
    [`(let ,bindings ,body)
      (letrec ([env-builder
                 (lambda (bindings env-so-far)
                   (match bindings
                     ['() (eval-exceptions body env-so-far k k-e)]
                     [`((,x ,e) . ,rest)
                       (eval-exceptions e
                                        env
                                        (lambda (v)
                                          (env-builder rest
                                                       (ext-env x v env-so-far)))
                                        k-e)]))])
        (env-builder bindings env))]
    [`(letrec ,bindings ,body)
      ; see http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.4.6
      (let ([rec-env (foldr (lambda (binding env-so-far)
                              (ext-env (first binding)
                                       (box 'undefined)
                                       env-so-far))
                            env
                            bindings)])
        (letrec ([rec-env-builder (lambda (bindings)
                                    (match bindings
                                      ['() (eval-exceptions body rec-env k k-e)]
                                      [`((,bind ,val) . ,rest)
                                        (eval-exceptions val
                                                         rec-env
                                                         (lambda (val)
                                                           (set-env! bind val rec-env)
                                                           (rec-env-builder rest))
                                                         k-e)]))])
          (rec-env-builder bindings)))]
    [`(try ,e ,var ,handler)
      (eval-exceptions e env k
                       (lambda (x)
                         (eval-exceptions handler (ext-env var x env) k k-e)))]
    [`(raise ,e)
      (eval-exceptions e env (lambda (e)
                               (k-e e)) k-e)]
    ; semantics are to eval try-e and return if no raise, but eval catch-e if
    ; try-e evals to raise
    ; Wrong? Need to eval body, then pass result to continuation that creates a
    ; lambda expression
    [`(lambda ,args ,body)
      (k (lambda (cont cont-e . host-args)
           (unless (equal? (length args) (length host-args))
             (error "param/arg lengths don't match, params:~a, args~a\n"
                    args
                    host-args))
           (letrec ([env-builder
                      (lambda (args host-args env-so-far)
                        (match host-args
                          ['() (eval-exceptions body env-so-far cont cont-e)]
                          [`(,a . ,d)
                            (eval-exceptions a
                                             env
                                             (lambda (a-val)
                                               (env-builder (cdr args)
                                                            d
                                                            (ext-env (car args)
                                                                     a-val
                                                                     env-so-far)))
                                             cont-e)]))])
             (env-builder args host-args env))))]
    [`(,rator . ,rands)
      (eval-exceptions rator
                       env
                       (lambda (rator)
                         (letrec ([eval-rands
                                    (lambda (rands rand-vals)
                                      (match rands
                                        ['()
                                         (apply rator `(,k ,k-e . ,rand-vals))]
                                        [`(,a . ,rest)
                                          (eval-exceptions a
                                                           env
                                                           (lambda (a-val)
                                                             (eval-rands rest
                                                                         (append rand-vals
                                                                                 `(,a-val))))
                                                           k-e)]))])
                           (eval-rands rands '())))
                       k-e)]))
