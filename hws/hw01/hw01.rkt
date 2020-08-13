#lang racket

(require eopl)

(define (listmap fn lst)
  (cond
    [(empty? lst) empty]
    [else (cons (fn (car lst)) (listmap fn (cdr lst)))]))

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

(define (product sos1 sos2)
  (cond
    [(and (empty? sos1) (empty? sos2)) empty]
    [(empty? sos1) sos2]
    [(empty? sos2) sos1]
    [(= (length sos1) 1) (listmap (lambda (x) (cons (car sos1) x)) sos2)]
    [else (append (listmap (lambda (x) (cons (car sos1) x)) sos2) (product (cdr sos1) sos2))]))

(define (every? pred lst)
  (cond
    [(empty? lst) #t]
    [else (and (pred (car lst)) (every? pred (cdr lst)))]))

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

(define-datatype full-binary-tree full-binary-tree?
  (internal-node (value integer?)
                 (left-child full-binary-tree?)
                 (right-child full-binary-tree?))
  (leaf-node (value integer?)))


(define inode
  (lambda (v l r)(internal-node v l r)))

(define lnode
  (lambda (v)(leaf-node v)))

(define (getleaves tree) 
  (cases full-binary-tree tree
         (internal-node (v l r) (append (getleaves l) (getleaves r)))
         (leaf-node (v) (list v))))

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

(define path-item (list "left" "right"))

(define (left-subtree tr)
  (cases full-binary-tree tr
         (internal-node (v l r) l)
         (leaf-node (v) (error "Invalid subtree access"))))

(define (right-subtree tr)
  (cases full-binary-tree tr
         (internal-node (v l r) r)
         (leaf-node (v) (error "Invalid subtree access"))))

(define (treeval tr)
  (cases full-binary-tree tr
         (internal-node (v l r) v)
         (leaf-node (v) v)))

(define (value-at-path path tree)
  (cond
    [(empty? path)
     (cases full-binary-tree tree
        (internal-node (v l r) v)
        (leaf-node (v) v))]
    [(string=? (car path) "left") (value-at-path (cdr path) (left-subtree tree))]
    [(string=? (car path) "right") (value-at-path (cdr path) (right-subtree tree))]
    [else (error "Invalid path")]))

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

(define (update path fn tree)
  (cond
    [(empty? path)
     (cases full-binary-tree tree
        (internal-node (v l r) (inode (fn v) l r))
        (leaf-node (v) (lnode (fn v))))]
    [(string=? (car path) "left") (inode (treeval tree) (update (cdr path) fn (left-subtree tree)) (right-subtree tree))]
    [(string=? (car path) "right") (inode (treeval tree) (left-subtree tree) (update (cdr path) fn (right-subtree tree)))]
    [else (error "Invalid path")]))

(define (tree/insert path left-st right-st tree)
  (cond
    [(empty? path)
     (cases full-binary-tree tree
        (internal-node (v l r) (error "Path doesn't lead to leaf"))
        (leaf-node (v) (inode v left-st right-st)))]
    [(string=? (car path) "left") (inode (treeval tree) (tree/insert (cdr path) left-st right-st (left-subtree tree)) (right-subtree tree))]
    [(string=? (car path) "right") (inode (treeval tree) (left-subtree tree) (tree/insert (cdr path) left-st right-st (right-subtree tree)))]
    [else (error "Invalid path")]))

(define singleton-tree (lnode 10))

(define small-tree
  (inode 20
         (lnode 30)
         (lnode 40)))

(define larger-tree
  (inode 100
         (inode 50
                (inode 200
                       (lnode 5)
                       (lnode 6))
                (lnode 80))
         (lnode 40)))

(provide full-binary-tree)
(provide full-binary-tree?)
(provide inode)
(provide lnode)
