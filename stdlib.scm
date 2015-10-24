(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

(define (list . objs) objs)
(define (id x) x)
(define (flip f) (lambda (a1 a2) (f a2 a1)))
(define (curry f a) (lambda (arg) (apply f (cons a (list arg)))))
(define (compose f g) (lambda (arg) (f (apply g arg))))

(define zero? (curry = 0))
(define positive? (curry > 0))
(define negative? (curry < 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

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

(define (sum . xs) (fold + 0 xs))
(define (product . xs) (fold * 1 xs))
(define (and . xs) (fold && #t xs))
(define (or . xs) (fold || #f xs))

(define (min first . rest) (fold (lambda (x y) (if (< x y) x y)) first rest))
(define (max first . rest) (fold (lambda (x y) (if (> x y) x y)) first rest))
(define (length lst) (fold (lambda (x y) (inc x)) 0 lst))
(define (reverse lst) (fold (flip cons) '() lst))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map f xs) (foldr (lambda (y ys) (cons (f y) ys)) '() xs))
(define (filter pred xs) (foldr (lambda (y ys) (if (pred y) (cons y ys) ys)) '() xs))
