#lang racket

(require eopl)
(require rackunit)
(require racket/match)
(provide (all-defined-out))

(define-datatype ast ast?
  [num (datum number?)]
  [bool (datum boolean?)]
  [ifte (test ast?) (then ast?) (else-ast ast?)]
  [function
   (formals (list-of id?))
   (body ast?)]
  [app (rator ast?) (rands (list-of ast?))]
  [id-ref (sym id?)]
  [assume (binds  (list-of bind?)) (body ast?)])

(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

(define-datatype proc proc?
  [prim-proc
    ;; prim refers to a scheme procedure
    (prim procedure?)
    ;; sig is the signature
    (sig (list-of procedure?))] 
  [closure
    (formals (list-of symbol?))
    (body ast?)
    (env env?)])

;;; prim? : proc? -> boolean?
(define prim-proc?
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #t]
      [else #f])))

(define closure? 
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #f]
      [else #t])))

;;; bind-id : bind? -> id?
(define bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

;;; bind-ast : bind? -> ast?
(define bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))

(define *keywords*
  '(ifte function assume))

(define id?
  (lambda (x)
    (and
     (symbol? x)
     (not (memq x *keywords*)))))

(define env? procedure?)


;;; lookup-env: [env?  symbol?] -> any/c
;;; lookup-env: throws "unbound identifier" error
(define lookup-env
  (lambda (e x)
    (e x)))

;;; empty-env : () -> env?
(define empty-env
  (lambda ()
    (lambda (x)
      (error 'empty-env "unbound identifier ~a" x))))

;;; extended-env :
;;;    [(list-of symbol?) (list-of any/c) env?] -> env?
(define extended-env
  (lambda (syms vals outer-env)
    (lambda (x)
      (let ([j (list-index syms x)])
        (cond
          [(= j -1) (lookup-env outer-env x)]
          [#t (list-ref vals j)])))))

;;; Returns the loction of the element in a list, -1 if the
;;; element is absent.

;;; list-index : [(listof any/c)  any/c] -> 
(define list-index
  (lambda (ls a)
    (letrec ([loop
               (lambda (ls ans)
                 (cond
                   [(null? ls) -1]
                   [(eq? (first ls) a) ans]
                   [#t (loop (rest ls) (+ 1 ans))]))])
      (loop ls 0))))

(define (+p x y) (+ x y))
(define (-p x y) (- x y))
(define (*p x y) (* x y))
(define (/p x y) (/ x y))
(define (<p x y) (< x y))
(define (<=p x y) (<= x y))
(define (eq?p x y) (eq? x y))
(define (0?p x) (= 0 x))
(define (!p x) (not x))

(define *init-env*
  (extended-env
   '(+ - * / < <= eq? 0? !)
   (list +p -p *p /p <p <=p eq?p 0?p !p)
   (empty-env)))

(define (parse expr)
  (cond
    [(empty? expr) (error "Empty expression")]
    [(number? expr) (num expr)]
    [(boolean? expr) (bool expr)]
    [(id? expr) (id-ref expr)]
    [else (let ([head (first expr)])
      (cond
        [(equal? head 'ifte)
         (ifte (parse (second expr)
                      (third expr)
                      (fourth expr)))]
        [(equal? head 'assume)
         (assume
           (map
             (lambda (binding)
               (make-bind (first binding) (parse (second binding))))
             (second expr))
           (parse (third expr)))]
        [(equal? head 'function)
         (function (second expr) (parse (third expr)))]
        [else
          (let ([rator (parse head)])
            (app rator 
                 (map
                   (lambda (x) (parse x))
                   (rest expr))))]))]))

(define get-formals
  (lambda (c)
    (cases proc c
      [closure (formals body env) formals]
      [else (error "Non-closure parameter")])))

(define get-body
  (lambda (c)
    (cases proc c
     [closure (formals body env) body]
     [else (error "Non-closure parameter")])))

(define get-bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

(define get-bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))

(define eval-ast
  (lambda (a e)
    (cases ast a
           [num (n) n]
           [bool (b) b]
           [id-ref (id) (lookup-env e id)]
           [ifte (test then-clause else-clause)
             (if
               (eval-ast test e)
               (eval-ast then-clause e)
               (eval-ast else-clause e))]
           [assume (binds body)
             (eval-ast
               body
               (extended-env
                 (map (lambda (x) (get-bind-id x)) binds)
                 (map (lambda (x) (eval-ast (get-bind-ast x) e)) binds)
                 e))]
           [function (formals body) (closure formals body e)]
           [app (rator rands)
                (let ([rator (eval-ast rator e)]
                      [rands (map (lambda (x) (eval-ast x e)) rands)])
                  (if (procedure? rator)
                    (apply rator rands)
                    (eval-ast
                      (get-body rator)
                      (extended-env (get-formals rator) rands e))))]
           )))

(define iden-fn '(function (x) x))
(define apply-iden-fn '((function (x) x) 10))

(define const-fn '(function () 1))
(define apply-const-fn '((function () 1)))

(define sum-fn '(function (p q) (+ p q)))
(define apply-sum-fn '((function (p q) (+ p q)) 10 30))

(define fn-with-assume '(assume ([sub-2 (function (x) (- x 2))]) (sub-2 10)))

(define fn-override-binding '(assume ([a 10])((function (a b) (+ a b)) 30 40)))

(eval-ast (app (id-ref '+) (list (num 1) (num 2))) *init-env*)
