#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "./letrec.rkt")

(define value-tests
  (test-suite
    "Values"
    (test-case
      "Numbers"
      (check-equal?  (letrec-eval '5) 5))
    (test-case
      "Symbols"
      (check-equal?  (letrec-eval '(quote x)) (quote x)))))

(define expression-tests
  (test-suite
    "Lookup (+,-,*,/ should be in the environment)"
    (test-case
      "+"
      (check-equal? (letrec-eval '(+ 2 3)) 5))
    (test-case
      "-"
      (check-equal? (letrec-eval '(- 5 2)) 3))
    (test-case
      "*"
      (check-equal? (letrec-eval '(* 3 4)) 12))
    (test-case
      "/"
      (check-equal? (letrec-eval '(/ 14 7)) 2))))

(define lambda-tests
  (test-suite
    "Application tests"
    (test-case
      "Identity application"
      (check-equal? (letrec-eval '((lambda (x) x) 5)) 5))
    (test-case
      "Square application"
      (check-equal? (letrec-eval '((lambda (x) (* x x)) 5)) 25))
    (test-case
      "Thunk evaluation"
      (check-equal? (letrec-eval '((lambda () (* 5 (/ 10 2))))) 25))))

(define let-tests
  (test-suite
    "let binding tests"
    (test-case
      "let binding a number"
      (check-equal? (letrec-eval '(let ([x 5]) x)) 5))
    (test-case
      "let binding a symbol"
      (check-equal? (letrec-eval '(let ([x (quote x)]) x)) (quote x)))
    (test-case
      "let binding infiltrating a lambda abstraction"
      (check-equal? (letrec-eval '(let ([x (quote x)]) ((lambda (y) x) 5))) (quote x)))
    (test-case
      "let binding in a thunk"
      (check-equal? (letrec-eval '(let ([x 3]) ((lambda () x)))) 3))
    (test-case
      "let binding a lambda expression"
      (check-equal? (letrec-eval '(let ([identity (lambda (x) x)]) (identity 5)))
                    5))
    (test-case
      "nested let bindings"
      (check-equal? (letrec-eval '(let ([x 200])
                                    (let ([f (lambda (z) (- z x))])
                                      (let ([x 100])
                                        (let ([g (lambda (z) (- z x))])
                                          (- (f 1) (g 1)))))))
                    -100))))

(define letrec-tests
  (test-suite
    "letrec binding tests"
    (test-case
      "letrec double"
      (check-equal? (letrec-eval '(letrec ([double (lambda (x)
                                                    (if (zero? x)
                                                      0
                                                      (+ 2 (double (- x 1)))))])
                                   (double 3)))
                    6))
    (test-case
      "letrec mutual recursion 1"
      (check-equal? (letrec-eval '(letrec ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
                                          [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
                                   (even? 4)))
                    #t))
    (test-case
      "letrec mutual recursion 2"
      (check-equal? (letrec-eval '(letrec ([even? (lambda (x) (if (zero? x) #t (not (- x 1))))]
                                          [odd? (lambda (x) (if (zero? x) #f (not (- x 1))))])
                                   (odd? 4)))
                    #f))
    (test-case
      "letrec mutual recursion 3"
      (check-equal? (letrec-eval '(letrec ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
                                          [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
                                   (even? 5)))
                    #f))
    (test-case
      "letrec mutual recursion 4"
      (check-equal? (letrec-eval '(letrec ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
                                          [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
                                   (odd? 5)))
                    #t))))

(run-tests value-tests 'verbose)
(run-tests expression-tests 'verbose)
(run-tests lambda-tests 'verbose)
(run-tests let-tests 'verbose)
(run-tests letrec-tests 'verbose)
