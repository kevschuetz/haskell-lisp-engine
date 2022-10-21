(define sqr (lambda (x) (* x x)))

(define fac (lambda (x)
    (if (== x 1)
        1
        (* x (fac (- x 1))))))

(define fib (lambda (n)
    (if (== n 0)
        0
        (if (== n 1)
            1
            (+ (fib (- n 1)) (fib (- n 2)))))))

(define hanoi (lambda (n)
    (if (== n 1)
        1
        (+ (* 2 (hanoi (- n 1))) 1))))

(define npairs (lambda (n)
    (if (== n 0)
        nil
        (cons n (npairs (- n 1))))))


(define find (lambda (x lst)
                (if (== lst nil)
                    nil
                    (if (== (car lst) x)
                        lst
                        (find x (cdr lst))))))

(define contains (lambda (x lst)
                    (if (== lst nil)
                        false
                        (if (== (car lst) x)
                           true
                           (contains x (cdr lst))))))

(define drop (lambda (lst n)
                (if (== n 0)
                    lst
                    (drop (cdr lst) (- n 1)))))

(define concat (lambda (a b)
                (if (== a nil)
                    b
                    (cons (car a) (concat (cdr a) b)))))

(define map (lambda (fn lst)
                  (if (== lst nil)
                     nil
                     (cons (fn (car lst)) (map fn (cdr lst))))))

(define filter (lambda (pred lst)
                  (if (== lst nil)
                     nil
                     (if (pred (car lst))
                         (cons (car lst) (filter pred (cdr lst)))
                         (filter pred (cdr lst))))))

(define lst (quote (19 8 3 6 9 2 1 11 15 7)))