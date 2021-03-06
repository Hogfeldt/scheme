(define (not x)         (if x #f #t))
(define (null? obj)     (if (eqv? obj '()) #t #f))        
(define (list . objs)   objs)
(define (id obj)        obj)

(define (flip f)        (lambda (x y) (f y x)))
(define (curry f x)     (lambda (y) (apply f (cons x (list y)))))
(define curry2 (lambda (f) (lambda (arg1) (lambda (arg2) (f arg1 arg2)))))
(define curry3 (lambda (f) (lambda (arg1) (lambda (arg2) (lambda (arg3) (f arg1 arg2 arg3))))))
(define (compose f g)   (lambda (x) (f (apply g x))))

(define zero?           (curry = 0))
(define positive?       (curry < 0))
(define negative?       (curry > 0))
(define (odd? num)      (= (mod num 2) 1))
(define (even? num)     (= (mod num 2) 0))

(define (foldr func end lst)
  (if (null? lst)
    end
    (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst)
    accum
    (foldl func (func accum (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

(define (unfold func init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold func (func init) pred))))

(define (sum . lst)     (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst)     (fold && #t lst))
(define (or . lst)      (fold || #f lst))

(define (max first . rest) 
  (fold 
    (lambda (old new) (if (> old new) old new))
    first
    rest))
(define (min first . rest) 
  (fold 
    (lambda (old new) (if (< old new) old new))
    first
    rest))

(define (length lst)    (fold (lambda (x y) (+ x 1)) 0 lst))
(define (reverse lst)   (fold (flip cons) '() lst))

(define (append lst1 lst2) (foldr cons lst1 lst2))

(define (nth n lst)
  (if (zero? n)
    (car lst)
    (nth (- n 1) (cdr lst))))

(define (mem-helper pred op)    (lambda 
                                  (acc next)
                                  (if (and (not acc) (pred (op next))))))
(define (memq obj lst)      (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst)      (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst)    (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist)    (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)    (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist)   (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map func lst)      (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (filter func lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

(define (lookup key dict)
  (if (null? dict)
    dict
    (if (equal? key (car (car dict)))
      (cdr (car dict))
      (lookup key (cdr dict)))))

(define (zip a b)
  (if (or (null? a) (null? b))
    '()
    (cons 
      (list (car a) (car b))
      (zip (cdr a) (cdr b)))))

(define (! n)
  (if (not (integer? n))
    '("Factorial can only be used with integers")
    (if (= n 0)
      1
      (* n (! (- n 1))))))

(define (pi) (/ 355 113))
(define (root n r) (^ n (/ 1 r)))
(define (abs n) (if (< n 0) (- 0 n) n))
