(define (tree-walk walker proc tree)
    (walker (lambda (elt)
                (if (list? elt)
                    (tree-walk walker proc elt)
                    (proc elt)))
        tree))

(define (reverse-for-each proc lis)
    (for-each proc (reverse lis)))

(define (reverse-map proc lis)
    (map proc (reverse lis)))

(define (reversed walker)
    (lambda (proc lis)
        (walker proc (reverse lis))))

(use srfi-1)

(define (for-each-numbers proc lis)
    (for-each proc (filter number? lis)))

(define (map-numbers proc lis)
    (map proc (filter number? lis)))

(define (numbers-only walker)
    (lambda (proc lis)
        (walker proc (filter number? lis))))

;; ローカルな再帰手続きをletrecで書く
(letrec ((sum (lambda (lis)
                  (cond [(null? lis) 0]
                        [(number? (car lis)) (+ (car lis) (sum (cdr lis)))]
                        [else (sum (cdr lis))]))))
    (sum '(1 3 #f 6 #t 9)))

;; ローカルな相互再帰手続きをletrecで書く
(letrec ((even? (lambda (n)
                    (cond [(= n 0) #t]
                          [(> n 0) (odd? (- n 1))]
                          [else (odd? (+ n 1))])))
         (odd?  (lambda (n)
                    (cond [(= n 0) #f]
                          [(> n 0) (even? (- n 1))]
                          [else (even? (+ n 1))]))))
    (even? 1280))
