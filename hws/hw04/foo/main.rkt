#lang racket

(require eopl)
(require rackunit)
(require racket/match)
(provide (all-defined-out))


(define-datatype ast ast?
  [unaryop (op unaryop?) (rand ast?)]
  [binop (op binop?) (rand1 ast?) (rand2 ast?)]
  [ifte (c ast?) (t ast?) (e ast?)]
  [num (n number?)]
  [bool (b boolean?)]
  [id-ref (sym id?)]
  [assume (bindings (list-of bind?)) (body ast?)])
(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

(define id? symbol?)
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
;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))
;;; denotable-value? :any/c -> boolean?
(define denotable-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))
(struct exn:parse-error exn:fail ())
(define raise-parse-error 
 (lambda (err-msg)
   (raise (exn:parse-error err-msg (current-continuation-marks)))))
(define (op->ast b)
  (match b
    ['+ 'add]
    ['- 'sub]
    ['* 'mul]
    ['/ 'div]
    ['< 'ltj]
    ['== 'eq]
    [_ 'error]
    ))

(define (is-binop? b)
  (match b
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    ['< #t]
    ['== #t]
    [_ #f]
    ))

(define (is-unaryop? b)
  (match b
    ['! #t]
    [_ #f]
    ))
;;; parse :: any/c -> ast?  Raises exception exn:parse-error?
;;; Fill in the function parse here
(define (parse exp)
    (cond
      [(number? exp) (num exp)]
      [(boolean? exp) (bool exp)]
      [(symbol? exp) (id-ref exp)]
      [(equal? (first exp) 'if) (if (< (length exp) 4)
				    (raise-parse-error "Missing body")
				    (ifte (parse (second exp)) (parse (third exp)) (parse (fourth exp))))]
      [(equal? (first exp) 'assume) (if (< (length exp) 3)
				    (raise-parse-error "Missing body")
				    (assume (map
					(lambda (binding)
					    (make-bind (first binding) (parse (second binding))))
					(second exp))
				    (parse (third exp))))]
      [(is-unaryop? (first exp)) (if (< (length exp) 2)
				     (raise-parse-error "Missing body")
				     (unaryop 'neg (parse (second exp))))]
      [(is-binop? (first exp)) (if (< (length exp) 2)
				      (raise-parse-error "Missing body")
				      (binop (op->ast (first exp)) (parse (second exp)) (parse (third exp))))]
      [else (raise-parse-error "Parser error")]
      ))
(define-datatype env env?
  [empty-env]
  [extended-env
    (syms (list-of symbol?))
    (vals (list-of denotable-value?))
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
      [empty-env () #f]
      [else #t])))
;;; lookup-env: [env?  symbol?] -> any/c || exn:lookup-err?
(define lookup-env
  (lambda (e x)
    (cases env e
           [empty-env () (raise-lookup-error)]
           [extended-env (syms vals outer)
                         (let ([idx (index-of syms x)])
                           (if idx
                               (list-ref vals idx)
                               (lookup-env outer x)))]))) ;; your solution here.
(struct exn:lookup-error exn:fail ())
(define raise-lookup-error 
  (lambda ()
    (raise (exn:lookup-error "unbound identifier" (current-continuation-marks)))))
(struct exn:exec-div-by-zero exn:fail ())
(define raise-exec-div-by-zero
  (lambda ()
    (raise (exn:exec-div-by-zero "div-by-0!" (current-continuation-marks)))))

(struct exn:exec-type-mismatch exn:fail ())
(define raise-exec-type-mismatch
  (lambda ()
    (raise (exn:exec-type-mismatch "type mismatch!" (current-continuation-marks)))))
;;; runtime-check :: [expressible? -> boolean?], exn? -> [expressible? -> expressible? || exn?] 
(define runtime-check
  (lambda (pred? exn)
    (lambda (v)
      (if (pred? v)
          v
          (exn)))))

(define typecheck-num
  (runtime-check number?  raise-exec-type-mismatch))

(define typecheck-bool 
  (runtime-check boolean? raise-exec-type-mismatch))

(define check-non-zero
  (runtime-check (not/c zero?) raise-exec-div-by-zero))
(define op-interpretation
  (lambda (op)
    (match op
      ['add +]
      ['sub -]
      ['mul *]
      ['div /]
      ['lt? <]
      ['eq? =]
      ['neg not]
      [_ error 'op-interpretation "unknown op"])))

;;; eval-ast :: [ast? env?] -> expressible-value? 
;;;                         || (or/c exn:exec-div-by-zero  exn:exec-type-mismatch exn:lookup-error)
(define eval-ast
  (lambda (a e)
    ;; your solution here
    (cases ast a
           [unaryop (op arg) (not (typecheck-bool (eval-ast arg e)))]
           [binop (op arg1 arg2) ((op-interpretation op)
                                  (typecheck-num (eval-ast arg1 e))
                                  (if (equal? op 'div)
                                      (check-non-zero (typecheck-num
                                                        (eval-ast arg2 e)))
                                      (typecheck-num (eval-ast arg2 e))))]
           [ifte (pred then otherwise) (if (typecheck-bool (eval-ast pred e))
                                           (eval-ast then e)
                                           (eval-ast otherwise e))]
           [num (n) n]
           [bool (b) b]
           [assume (binds expr) (eval-ast
                                  expr
                                  (extended-env
                                    (map (lambda (x) (bind-id x)) binds)
                                    (map (lambda (x) (bind-ast x)) binds) e))]
           [id-ref (id) (lookup-env e id)])))
(define unaryop?
  (lambda (x)
    (match x
      ['neg #t]
      [_ #f])))
(define binop?
  (lambda (x)
    (match x
      ['add #t]
      ['sub #t]
      ['mul #t]
      ['div #t]
      ['lt? #t]
      ['eq? #t]
      [_ #f])))
(define ts-numop-incorrect-param-rand1
  (test-suite 
   "wrongly typed rand1 parameters"
   (for/list ([numerical-op '(add sub mul div lt? eq?)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda () 
                    (eval-ast (binop numerical-op
                                     (binop 'lt? (num 10) (num 20)) ; boolean
                                     (num 10))
                              (empty-env))))))))

(define ts-numop-incorrect-param-rand2
  (test-suite
   "wrongly typed rand2 parameters"
   (for/list ([numerical-op '(add sub mul div)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda () 
                    (eval-ast (binop numerical-op (num 10)
                                     (binop 'lt? (num 10) (num 20)))
                              (empty-env))))))))
