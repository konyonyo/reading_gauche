;; リスト操作
(define (list? obj)
    (or (null? obj)
        (and (pair? obj) (list? (cdr obj)))))

;; foldの使い方
(define (sum-of-numbers lis)
    (fold + 0 lis))

(define (product-of-numbers lis)
    (fold * 1 lis))

(define (max-number lis)
    (define (pick-greater a b)
        (if (> a b) a b))
    (fold pick-greater -inf.0 lis))

(define (max-number2 lis)
    (define (pick-greater a b)
        (if (> a b) a b))
    (if (null? lis)
        (error "max-number needs at least one number")
        (fold pick-greater (car lis) (cdr lis))))

(define (length lis)
    (define (increment2 a b) (+ b 1))
    (fold increment2 0 lis))

(define (print-elements lis)
    (define (print-one-element a b) (print a))
    (fold print-one-element #f lis))

;; foldの定義
(define (fold proc init lis)
    (if (null? lis)
        init
        (fold proc #?=(proc (car lis) init) #?=(cdr lis))))

(define (last-pair lis)
    (if (pair? (cdr lis))
        (last-pair (cdr lis))
        lis))
(define (copy-list lis)
    (if (pair? lis)
        (cons (car lis) (copy-list (cdr lis)))
        lis))

(define (deep-copy-list lis)
    (if (pair? lis)
        (if (pair? (car lis))
            (cons (deep-copy-list (car lis)) (deep-copy-list (cdr lis)))
            (cons (car lis) (deep-copy-list (cdr lis))))
        lis))

(define (append2 a b)
    (if (pair? a)
        (cons (car a) (append2 (cdr a) b))
        b))

(define (reverse lis)
    (if (null? (cdr lis))
        lis
        (append2 (reverse (cdr lis)) (list (car lis)))))

(define (find pred lis)
    (if (null? lis)
        #f
        (if (pred (car lis))
            (car lis)
            (find pred (cdr lis)))))

(define (find2 pred lis)
    (cond [(null? lis) #f]
          [(pred (car lis)) (car lis)]
          [else (find2 pred (cdr lis))]))

(define (length2 lis)
    (if (null? lis)
        0
        (+ 1 (length2 (cdr lis)))))

(define (filter pred lis)
    (if (null? lis)
        '()
        (if (pred (car lis))
            (cons (car lis) (filter pred (cdr lis)))
            (filter pred (cdr lis)))))

(define (filter2 pred lis)
    (cond [(null? lis) '()]
          [(pred (car lis)) (cons (car lis) (filter2 pred (cdr lis)))]
          [else (filter pred (cdr lis))]))

(define (length3 lis)
    (define (length-rec lis n)
        (if (null? lis)
            n
            (length-rec (cdr lis) (+ 1 n))))
    (length-rec lis 0))

(define (reverse2 lis)
    (define (reverse-rec lis0 lis1)
        (if (null? lis0)
            lis1
            (reverse-rec (cdr lis0) (cons (car lis0) lis1))))
    (reverse-rec lis '()))
