#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "./cps-letrec.rkt")

(define value-tests
  (test-suite
    "Values"
    (test-case
      "Numbers"
      (check-equal? (cps-letrec '5 init-env identity) 5))
    (test-case
      "Symbols"
      (check-equal? (cps-letrec '(quote x) init-env identity) (quote x)))))

(define expression-tests
  (test-suite
    "Lookup (+,-,*,/ should be in the environment)"
    (test-case
      "+"
      (check-equal? (cps-letrec '(+ 2 3) init-env identity) 5))
    (test-case
      "-"
      (check-equal? (cps-letrec '(- 5 2) init-env identity) 3))
    (test-case
      "*"
      (check-equal? (cps-letrec '(* 3 4) init-env identity) 12))
    (test-case
      "/"
      (check-equal? (cps-letrec '(/ 14 7) init-env identity) 2))))

(define lambda-tests
  (test-suite
    "Application tests"
    (test-case
      "Identity application"
      (check-equal? (cps-letrec '(identity 5) init-env identity) 5))
    (test-case
      "Square application"
      (check-equal? (cps-letrec '((lambda (x) (* x x)) 5)
                                init-env
                                identity) 25))
    (test-case
      "Thunk evaluation"
      (check-equal? (cps-letrec '((lambda () (* 5 (/ 10 2)))) init-env identity) 25))))

(define let-tests
  (test-suite
    "let binding tests"
    (test-case
      "let binding a number"
      (check-equal? (cps-letrec '(let ([x 5]) x) init-env identity) 5))
    (test-case
      "let binding a symbol"
      (check-equal? (cps-letrec '(let ([x (quote x)]) x) init-env identity) (quote x)))
    (test-case
      "let binding infiltrating a lambda abstraction"
      (check-equal? (cps-letrec '(let ([x (quote x)]) ((lambda (y) x) 5))
                                init-env
                                identity)
                    (quote x)))
    (test-case
      "let binding in a thunk"
      (check-equal? (cps-letrec '(let ([x 3]) ((lambda () x)))
                                init-env
                                identity)
                    3))
    (test-case
      "let binding a lambda expression"
      (check-equal? (cps-letrec '(let ([identity (lambda (x) x)]) (identity 5))
                                init-env
                                identity)
                    5))))

(define letrec-tests
  (test-suite
    "letrec binding tests"
    (test-case
      "letrec double"
      (check-equal? (cps-letrec '(letrec ([double (lambda (x)
                                                    (if (zero? x)
                                                      0
                                                      (+ 2 (double (- x 1)))))])
                                   (double 3)) init-env identity)
                    6))
    (test-case
      "letrec mutual recursion 1"
      (check-equal? (cps-letrec '(letrec ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
                                          [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
                                   (even? 4)) init-env identity)
                    #t))
    (test-case
      "letrec mutual recursion 2"
      (check-equal? (cps-letrec '(letrec ([even? (lambda (x) (if (zero? x) #t (not (- x 1))))]
                                          [odd? (lambda (x) (if (zero? x) #f (not (- x 1))))])
                                   (odd? 4)) init-env identity)
                    #f))
    (test-case
      "letrec mutual recursion 3"
      (check-equal? (cps-letrec '(letrec ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
                                          [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
                                   (even? 5)) init-env identity)
                    #f))
    (test-case
      "letrec mutual recursion 4"
      (check-equal? (cps-letrec '(letrec ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
                                          [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
                                   (odd? 5))
                                init-env
                                identity)
                    #t))))

(run-tests value-tests 'verbose)
(run-tests expression-tests 'verbose)
(run-tests lambda-tests 'verbose)
(run-tests let-tests 'verbose)
(run-tests letrec-tests 'verbose)
