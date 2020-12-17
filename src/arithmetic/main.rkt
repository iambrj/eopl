#lang racket
(require eopl)
(require rackunit)
(require racket/match)
(provide (all-defined-out))

(define-datatype ast ast?
 [binop (op binop?) (rand1 ast?) (rand2 ast?)]
 [ifte (c ast?) (t ast?) (e ast?)]
 [num (n number?)]
 [bool (b boolean?)])

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

(struct exn:parse-error exn:fail ())

(define raise-parse-error
 (lambda (err-msg)
   (raise (exn:parse-error err-msg (current-continuation-marks)))))

;;; parse :: any/c -> ast?  Raises exception exn:parse-error?
;;; Fill in the function parse here
(define (parse exp)
  (cond
  [(number? exp) (num exp)]
  [(boolean? exp) (bool exp)]
  [(is-binop? (first exp)) (binop (binop->ast (first exp)) (parse (second exp)) (parse (third exp)))]
  [(eq? (first exp) 'if) (ifte (parse (second exp)) (parse (third exp)) (parse (fourth exp)))]
  [else (raise-parse-error "Parser error")]))

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
      [_ error "unknown op"])))

;;; eval-ast :: ast? -> expressible? || (or/c exn:exec-div-by-zero  exn:exec-type-mismatch)
(define eval-ast
  (lambda (a)
  (cases ast a
      (binop (op l r) (if (and (equal? op 'div) (equal? (eval-ast r) 0))
			    (raise-exec-div-by-zero)
			    ((op-interpretation op)
			      (typecheck-num (eval-ast l))
			      (typecheck-num (eval-ast r)))))
      (ifte (c t e) (if (typecheck-bool (eval-ast c)) (eval-ast t) (eval-ast e)))
      (num (n) n)
      (bool (b) b))))

(define ts-numop-incorrect-param-rand1
  (test-suite
   "wrongly typed rand1 parameters"
   (for/list ([numerical-op '(add sub mul div lt? eq?)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda ()
                    (eval-ast (binop numerical-op
                                     (binop 'lt? (num 10) (num 20)) ; boolean
                                     (num 10)))))))))

(define ts-numop-incorrect-param-rand2
  (test-suite
   "wrongly typed rand2 parameters"
   (for/list ([numerical-op '(add sub mul div)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda ()
                    (eval-ast (binop numerical-op (num 10)
                                     (binop 'lt? (num 10) (num 20))))))))))

(define (is-binop? t)
  (match t
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    ['< #t]
    ['== #t]
    [_ #f]))

(define (binop->ast b)
  (match b
    ['+ 'add]
    ['- 'sub]
    ['* 'mul]
    ['/ 'div]
    ['< 'ltj]
    ['== 'eq]
    [_ 'error]))
