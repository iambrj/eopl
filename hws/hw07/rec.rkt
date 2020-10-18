#lang racket

(require eopl)

(define-datatype ast ast?
  [num (datum number?)]
  [bool (datum boolean?)]
  [ifte (test ast?) (then ast?) (else-ast ast?)]
  [function
   (formals (list-of id?))
   (body ast?)]
  [recursive (fbinds (list-of fbind?)) (body ast?)]
  [app (rator ast?) (rands (list-of ast?))]
  [id-ref (sym id?)]
  [assume (binds  (list-of bind?)) (body ast?)])

(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

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

(define-datatype fbind fbind?
  [make-fbind (fb-id id?)
              (fb-formals (list-of id?))
              (fb-body ast?)])

;;; fbind-id : fbind? -> id?
(define fbind-id
  (lambda (b)
    (cases fbind b
      [make-fbind (fb-id fb-formals fb-body) fb-id])))

;;; fbind-formals : fbind? -> (list-of id?)
(define fbind-formals
  (lambda (b)
    (cases fbind b
      [make-fbind (fb-id fb-formals fb-body) fb-formals])))

;;; fbind-body : fbind? -> ast?
(define fbind-body
  (lambda (b)
    (cases fbind b
      [make-fbind (fb-id fb-formals fb-body) fb-body])))

(define *keywords*
  '(ifte assume function recursive))

(define id?
  (lambda (x)
    (and
     (symbol? x)
     (not (memq x *keywords*)))))

;;; parse :: any/c -> ast?  Raises exception exn?
;;; Fill in the function parse here
(define (parse expr)
  (cond
    [(empty? expr) (error "Empty expression")]
    [(number? expr) (num expr)]
    [(boolean? expr) (bool expr)]
    [(id? expr) (id-ref expr)]
    [else (let ([head (first expr)])
      (cond
        [(equal? head 'ifte)
         (ifte (parse (second expr))
               (parse (third expr))
               (parse (fourth expr)))]
        [(equal? head 'assume)
         (assume
           (map
             (lambda (binding)
               (make-bind (first binding) (parse (second binding))))
             (second expr))
           (parse (third expr)))]
        [(equal? head 'function)
         (function (second expr) (parse (third expr)))]
        [(equal? head 'recursive)
         (let ([fbinds (second expr)]
               [body (third expr)])
              (recursive
                (map
                  (lambda (f)
                    (make-fbind (first f) (second f) (parse (third f))))
                  fbinds)
                (parse body)))]
        [else
          (let ([rator (parse head)])
            (app rator 
                 (map
                   (lambda (x) (parse x))
                   (rest expr))))]))]))
(define-datatype env env?
  [empty-env]
  [extended-env
    (syms (list-of symbol?))
    (vals (list-of denotable-value?))
    (outer-env env?)]
  [extended-rec-env
    (fsyms (list-of symbol?))
    (lformals (list-of (list-of symbol?)))
    (bodies (list-of ast?))
    (outer-env env?)])
;;; empty-env? : env? -> boolean?
(define empty-env?
  (lambda (e)
    (cases env e
      [empty-env () #t]
      [else #f])))

;;; extended-env? : env? -> boolean?
(define extended-env?
  (lambda (e)
    (cases env e
      [extended-env (syms vals outer-env) #t]
      [else #f])))

;;; extended-rec-env? : env? -> boolean?
(define extended-rec-env?
  (lambda (e)
    (cases env e
      [extended-rec-env (fsyms lformals bodies outer-env) #t]
      [else #f])))

(define *init-env*
  (extended-env
   '(+ - * / < <= eq? 0? !)
   (list +p -p *p /p <p <=p eq?p 0?p !p)
   (empty-env)))

(define lookup-env
  (lambda (e x) 
    #f))

; (define eval-ast
;   (lambda (a e)
;     (cases ast a
;            [num (n) n]
;            [bool (b) b]
;            [id-ref (id) (lookup-env e id)]
;            [ifte (test then-clause else-clause)
;              (if
;                (eval-ast test e)
;                (eval-ast then-clause e)
;                (eval-ast else-clause e))]
;            [assume (binds body)
;              (eval-ast
;                body
;                (extended-env
;                  (map (lambda (x) (get-bind-id x)) binds)
;                  (map (lambda (x) (eval-ast (get-bind-ast x) e)) binds)
;                  e))]
;            [function (formals body) (closure formals body e)]
;            [app (rator rands)
;                 (let ([rator (eval-ast rator e)]
;                       [rands (map (lambda (x) (eval-ast x e)) rands)])
;                   (if (procedure? rator)
;                     (apply rator rands)
;                     (eval-ast
;                       (get-body rator)
;                       (extended-env (get-formals rator) rands e))))]
;            )))
(parse '(recursive ([f1 (x) (< 5 x)])(ifte (f1 2) 0 10)))
