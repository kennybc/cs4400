;;; starter code for assignment #1
#lang plait

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my-max : (Number Number -> Number))
(define (my-max a b)
  (if (> a b) a b))

; add at least 3 test cases here
(test (my-max 2 5) 5)
(test (my-max 4 5) 5)
(test (my-max 6 5) 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; returns #t if all elements of l are greater than k and #f otherwise
(define (all-greater? [l : (Listof Number)] [k : Number])
  (type-case (Listof Number) l
    [empty #t]
    [(cons h t)
     (and
      (> h k)
      (all-greater? t k))]))

; add at least 3 test cases here
(test (all-greater? '() 200) #t)
(test (all-greater? '(1 2 3) 2) #f)
(test (all-greater? '(1 2 3) 0) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sorted? : ((Listof Number) -> Boolean))
(define (sorted? l)
  (type-case (Listof Number) l
    [empty #t]
    [(cons h t)
     (and
      (sorted-first? h t)
      (sorted? t))]))

(sorted-first? : (Number (Listof Number) -> Boolean))
(define (sorted-first? h t)
  (type-case (Listof Number) t
    [empty #t]
    [(cons next _) (>= next h)]))


; add at least 3 test cases here
(test (sorted? '()) #t)
(test (sorted? '(2 2 2)) #t)
(test (sorted? '(4 3 5)) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Tree
  (leaf [value : Number])
  (node [left : Tree] [right : Tree]))

(sum-tree : (Tree -> Number))
(define (sum-tree t)
  (type-case Tree t
    [(leaf v) v]
    [(node l r) (+ (sum-tree l) (sum-tree r))]))

; add at least 3 test cases here
(test (sum-tree (node (leaf 10) (leaf 20))) 30)
(test (sum-tree (node (leaf 5) (node (leaf 5) (leaf 5)))) 15)
(test (sum-tree (leaf 20)) 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(right-linear? : (Tree -> Boolean))
(define (right-linear? t)
  (type-case Tree t
    [(leaf v) #t]
    [(node l r)
     (and
      (type-case Tree l
        [(leaf v) #t]
        [(node _l _r) #f])
      (right-linear? r))]))

; add at least 3 test cases here
(test (right-linear? (node (leaf 10) (leaf 20))) #t)
(test (right-linear? (node (leaf 5) (node (leaf 5) (leaf 5)))) #t)
(test (right-linear? (node (leaf 5) (node (node (leaf 2) (leaf 2)) (leaf 5)))) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dfs-list : (Tree -> (Listof Number)))
(define (dfs-list t)
  (type-case Tree t
    [(leaf v) (cons v '())]
    [(node l r) (append (dfs-list l) (dfs-list r))]))

; add at least 3 test cases here
(test (dfs-list (node (node (leaf 1) (leaf 2)) (leaf 3))) '(1 2 3))
(test (dfs-list (node (leaf 5) (node (node (leaf 2) (leaf 3)) (leaf 4)))) '(5 2 3 4))
(test (dfs-list (node (leaf 3) (node (leaf 5) (leaf 4)))) '(3 5 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(subst : (Tree Number Number -> Tree))
(define (subst t old n)
  (type-case Tree t
    [(leaf v) (leaf (if (= v old) n v))]
    [(node l r) (node (subst l old n) (subst r old n))]))

; add at least 3 test cases here
(test (subst (node (leaf 10) (leaf 20)) 20 30) (node (leaf 10) (leaf 30)))
(test (subst (node (leaf 5) (node (node (leaf 2) (leaf 2)) (leaf 5))) 5 8)
      (node (leaf 8) (node (node (leaf 2) (leaf 2)) (leaf 8))))
(test (subst (node (leaf 1) (node (node (leaf 2) (leaf 6)) (leaf 7))) 3 6)
      (node (leaf 1) (node (node (leaf 2) (leaf 6)) (leaf 7))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(multi-subst : (Tree (Hashof Number Number) -> Tree))
(define (multi-subst t tbl)
  (type-case Tree t
    [(leaf v) (leaf
               (type-case (Optionof Number) (hash-ref tbl v)
                [(none) v]
                [(some n) n]))]
    [(node l r) (node (multi-subst l tbl) (multi-subst r tbl))]))

; add at least 3 test cases here
(define test-tbl (make-hash (list (pair 1 3) (pair 2 4))))
(test (multi-subst (node (leaf 1) (leaf 20)) test-tbl)
      (node (leaf 3) (leaf 20)))
(test (multi-subst (node (leaf 1) (leaf 2)) test-tbl)
      (node (leaf 3) (leaf 4)))
(test (multi-subst (node (leaf 0) (leaf 4)) test-tbl)
      (node (leaf 0) (leaf 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my-filter : ((Listof Number) (Number -> Boolean) -> (Listof Number)))
(define (my-filter l f)
  (type-case (Listof Number) l
    [empty '()]
    [(cons h t)
     (if (f h)
         (cons h (my-filter t f))
         (my-filter t f))]))

; add at least 3 test cases here
(test (my-filter '(1 2 3) (lambda (x) (< x 3))) '(1 2))
(test (my-filter '(5 6 7 8 6 9 6) (lambda (x) (= x 6))) '(6 6 6))
(test (my-filter '(-4 -3 -2 -1 0 1 2 3 4) (lambda (x) (= 4 (* x x)))) '(-2 2))