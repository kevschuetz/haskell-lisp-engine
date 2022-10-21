1
2
true

(define x 1)
(define y 2)

x
y
(- 3)


(- x)

(== x y)
(== y 2)
(!= y 1)
(+ 1 2)
(* y 2)
(/ 5 2)
(% 5 2)
(< 5 2)
(> 5 2)
(<= 5 5)
(>= 5 5)
(&& true false)
(|| true false)
(! false)
(&& true (! false))

(quote x)
(quote (1 2 3))
(quote (+ x 1)

(eval (quote x))
(eval (quote (+ x 1)))



(cons 3 nil)
(cons 2 (cons 3 nil))
(cons 1(cons 2 (cons 3 nil)))
(cons 1 (quote (2 3)))
(car (quote (1 2 3)))
(cdr (quote (1 2 3)))
(car (cons 1(cons 2 (cons 3 nil))))
(car (cons 1(cons 2 (cons 3 nil))))
(car (cdr (cons 1(cons 2 (cons 3 nil)))))
(cdr (cdr (cons 1(cons 2 (cons 3 nil)))))
(car (cdr (cdr (cons 1(cons 2 (cons 3 nil))))))
(cdr (cdr (cdr (cons 1(cons 2 (cons 3 nil))))))
(car nil)  -- error
(cdr nil)  -- error

(define l (quote (2 7 8 2 1 9 4)))

(define sqr (lambda (x) (* x x)))
(define fac (lambda (x) (if (== x 1) 1 (* x (fac (- x 1))))))

(define ints (lambda (from to) (if (> from to) nil (cons from (ints (+ from 1) to)))))
(define ints1To5 (ints 1 5))

(define contains (lambda (x lst) (if (== lst nil)  false (if (== (car lst) x) true  (contains x (cdr lst))))))
(contains 3 ints1to5)

(define map (lambda (fn lst) (if (== lst nil) nil (cons (fn (car lst)) (map fn (cdr lst))))))
(map sqr ints1to5)

(define find (lambda (pred lst) (if (== lst nil) false (if (pred(car lst)) true (find pred (cdr lst))))))
(define filter (lambda (pred lst) (if (== lst nil) nil (if (pred(car lst)) (cons (car lst) (filter pred (cdr lst))) (filter pred (cdr lst))))))

(define even (lambda (x) (== (% x 2) 0)))
(define odd (lambda (x) (! (even x))))

(find even inst1to5)
(filter odd ints1to5)
(filter (lambda (x) (== (% x 2) 0)) (ints 1 99))
