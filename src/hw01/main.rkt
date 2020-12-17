#lang racket

(require eopl)

(require "datatypes.rkt")

(define (repeat n x)
  (cond
    [(= n 0) empty]
    [(> n 0) (cons x (repeat (- n 1) x))]
    [else (error "Bad n")]))
(define (invert lst)
  (cond
    [(empty? lst) empty]
    [else (cons (cons (second (car lst)) (cons (first (car lst)) empty)) (invert (cdr lst)))]))
(define (count-occurrences s slist)
  (cond
    [(empty? slist) 0]
    [else (+ (if (= s (car slist)) 1 0) (count-occurrences s (cdr slist)))]))
(define (listmap fn lst)
  (cond
    [(empty? lst) empty]
    [else (cons (fn (car lst)) (listmap fn (cdr lst)))]))
(define (product sos1 sos2)
  (cond
    [(and (empty? sos1) (empty? sos2)) empty]
    [(empty? sos1) sos2]
    [(empty? sos2) sos1]
    [(= (length sos1) 1) (listmap (lambda (x) (list (car sos1) x)) sos2)]
    [else (append (listmap (lambda (x) (list (car sos1) x)) sos2) (product (cdr sos1) sos2))]))
(define (every pred lst)
  (cond
    [(empty? lst) #t]
    [else (and (pred (car lst)) (every pred (cdr lst)))]))
(define (merge loi1 loi2)
  (cond
    [(empty? loi1) loi2]
    [(empty? loi2) loi1]
    [else
      (cond
        [(> (car loi1) (car loi2)) (cons (car loi2) (merge loi1 (cdr loi2)))]
        [else (cons (car loi1) (merge (cdr loi1) loi2))])]))

(define (flatten dlst)
  (cond
    [(empty? dlst) empty]
    [else
      (cond
        [(pair? dlst) (append (flatten (car dlst)) (flatten (cdr dlst)))]
        [else (list dlst)])]))
(define (traverse/preorder tree)
  (cases full-binary-tree tree
         (internal-node (v l r) (append (list v) (traverse/preorder l) (traverse/preorder r)))
         (leaf-node (v) (list v))))
(define (traverse/inorder tree)
  (cases full-binary-tree tree
         (internal-node (v l r) (append (traverse/inorder l) (list v) (traverse/inorder r)))
         (leaf-node (v) (list v))))
(define (traverse/postorder tree)
  (cases full-binary-tree tree
         (internal-node (v l r) (append (traverse/postorder l) (traverse/postorder r) (list v)))
         (leaf-node (v) (list v))))
(define (count-nodes tree)
  (cases full-binary-tree tree
         (internal-node (v l r) (+ 1 (count-nodes l) (count-nodes r)))
         (leaf-node (v) (+ 1))))
(define (count-leaves tree)
  (cases full-binary-tree tree
         (internal-node (v l r) (+ (count-leaves l) (count-leaves r)))
         (leaf-node (v) (+ 1))))
(define (count-internal tree)
  (cases full-binary-tree tree
         (internal-node (v l r) (+ 1 (count-internal l) (count-internal r)))
         (leaf-node (v) (+ 0))))
(define (tree/map fn tr)
  (cases full-binary-tree tr
         (internal-node (v l r) (inode (fn v) (tree/map fn l) (tree/map fn r)))
         (leaf-node (v) (lnode (fn v)))))
(define (value-at-path path tree)
  (cond
    [(empty? path)
     (cases full-binary-tree tree
        (internal-node (v l r) v)
        (leaf-node (v) v))]
    [(string=? (car path) "left") (value-at-path (cdr path) (left-subtree tree))]
    [(string=? (car path) "right") (value-at-path (cdr path) (right-subtree tree))]
    [else (error "Invalid path")]))
(define (left-subtree tr)
  (cases full-binary-tree tr
         (internal-node (v l r) l)
         (leaf-node (v) #f)))
(define (right-subtree tr)
  (cases full-binary-tree tr
         (internal-node (v l r) r)
         (leaf-node (v) #f)))
(define (treeval tr)
  (cases full-binary-tree tr
         (internal-node (v l r) v)
         (leaf-node (v) v)))
(define (boolsearch val tree)
  (cases full-binary-tree tree
         (internal-node (v l r)
           (cond
             [(= val v) #t]
             [else (or (boolsearch val (left-subtree tree))
                       (boolsearch val (right-subtree tree))
                       #f)]))
         (leaf-node (v)
           (cond
             [(= val v) #t]
             [else #f]))))
(define (search val tree)
  (cases full-binary-tree tree
         (internal-node (v l r)
           (cond
             [(= val v) (list)]
             [(boolsearch val (left-subtree tree)) (append (list "left") (search val (left-subtree tree)))]
             [(boolsearch val (right-subtree tree)) (append (list "right") (search val (right-subtree tree)))]
             [else (error "Not found")]))
         (leaf-node (v) (list))))
(define (badpath path tree)
  (if (empty? path) #f
    (cond
    [(equal? (car path) "left") (if (boolean? (left-subtree tree))
				    #t
				    (badpath (cdr path) (left-subtree tree)))]
    [(equal? (car path) "right") (if (boolean? (right-subtree tree))
				     #t
				     (badpath (cdr path) (right-subtree tree)))]
    [else #t])))

(define (update path fn tree)
(if (badpath path tree) tree
  (cond
    [(empty? path)
     (cases full-binary-tree tree
        (internal-node (v l r) (inode (fn v) l r))
        (leaf-node (v) (lnode (fn v))))]
    [(equal? (car path) "left") (inode (treeval tree) (update (cdr path) fn (left-subtree tree)) (right-subtree tree))]
    [(equal? (car path) "right") (inode (treeval tree) (left-subtree tree) (update (cdr path) fn (right-subtree tree)))]
    [else tree])))
(define (tree/insert path left-st right-st tree)
  (cond
    [(empty? path)
     (cases full-binary-tree tree
        (internal-node (v l r) (error "Path doesn't lead to leaf"))
        (leaf-node (v) (inode v left-st right-st)))]
    [(string=? (car path) "left") (inode (treeval tree) (tree/insert (cdr path) left-st right-st (left-subtree tree)) (right-subtree tree))]
    [(string=? (car path) "right") (inode (treeval tree) (left-subtree tree) (tree/insert (cdr path) left-st right-st (right-subtree tree)))]
    [else (error "Invalid path")]))
(provide (all-defined-out))
