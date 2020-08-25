#lang racket
(require eopl)

(define-datatype ast ast?
  (binop (type binop?) (left ast?) (right ast?))
  (ifte (c ast?) (t ast?) (e ast?))
  (num (n number?))
  (bool (b boolean?)))

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
  (lambda (err)
    (raise (exn:parse-error err (current-continuation-marks)))))

(struct exn:exec-div-by-zero exn:fail ())
(define raise-exec-div-by-zero
  (lambda ()
    (raise (exn:exec-div-by-zero "div-by-0!" (current-continuation-marks)))))

(struct exn:exec-type-error exn:fail ())
(define raise-exec-type-error
  (lambda ()
    (raise (exn:exec-type-error "types errored!" (current-continuation-marks)))))

#|
(define ts-parsing
  (test-suite "parsing"
              (test-case "num" (check-equal? (parse 10) (ast-num 10)))
              (test-case "add" (check-equal? (parse '(+ 10 20)) (ast-binop 'add (ast-num 10) (ast-num 20))))
              (test-case "sub" (check-equal? (parse '(- 10 20)) (ast-binop 'sub (ast-num 10) (ast-num 20))))
              (test-case "mul" (check-equal? (parse '(* 10 20)) (ast-binop 'mul (ast-num 10) (ast-num 20))))
              (test-case "div" (check-equal? (parse '(/ 10 20)) (ast-binop 'div (ast-num 10) (ast-num 20))))
              (test-case "bool-t" (check-equal? (parse #t) (ast-bool #t)))
              (test-case "bool-f" (check-equal? (parse #f) (ast-bool #f)))
              (test-case "if" (check-equal? (parse '(if #t 10 20)) (ast-if (ast-bool #t) (ast-num 10) (ast-num 20))))
              (test-case "failure"
                (check-exn exn:parse-error?
                           (lambda () (parse '(** 10 20)))))
              (test-case "recur" (check-equal?
                                  (parse '(+ (- 10 20) (* 20 30)))
                                  (ast-binop 'add
                                             (ast-binop 'sub (ast-num 10) (ast-num 20))
                                             (ast-binop 'mul (ast-num 20) (ast-num 30)))))
              ))
|#

(define (is-binop? t)
  (match t
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    ['< #t]
    ['== #t]
    [_ #f]
    ))

(define (binop->ast b)
  (match b
    ['+ 'add]
    ['- 'sub]
    ['* 'mul]
    ['/ 'div]
    ['< 'ltj]
    ['== 'eq]
    [_ 'error]
    ))

(define (parse exp)
      (cond
      [(number? exp) (num exp)]
      [(boolean? exp) (bool exp)]
      [(is-binop? (first exp)) (binop (binop->ast (first exp)) (parse (second exp)) (parse (third exp)))]
      [(eq? (first exp) 'if) (ifte (parse (second exp)) (parse (third exp)) (parse (fourth exp)))]
      [else raise-parse-error]
      ))
