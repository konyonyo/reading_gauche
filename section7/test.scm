(use util.match)

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

(define (mit-form->primitive-form expr)
    (cons (car expr)
       (cons (car (car (cdr expr)))
          (list (cons 'lambda
             (cons (cdr (car (cdr expr)))
                 (cdr (cdr expr))))))))

(define (primitive-form->mit-form expr)
    (cons (car expr)
        (cons (cons (car (cdr expr)) (car (cdr (car (cdr (cdr expr))))))
            (cdr (cdr (car (cdr (cdr expr))))))))

(define (append/log . args)
    (print "args=" args)
    (apply append args))

(define (make-logger func)
    (lambda args
        (print "args=" args)
        (apply func args)))

(define (append2 lis1 lis2)
    (if (pair? lis1)
        (cons (car lis1) (append2 (cdr lis1) lis2))
        lis2))

(define (append22 lis1 lis2)
    (define (append22-sub lisa lisb)
        (if (null? lisa)
            lisb
            (append22-sub (cdr lisa) (cons (car lisa) lisb))))
    (append22-sub (reverse lis1) lis2))

(define (append222 lis1 lis2)
    (fold cons lis2 (reverse lis1)))

(define (append . args)
    (cond [(null? args) '()]
          [(null? (cdr args)) (car args)]
          [else (append2 (car args) (apply append (cdr args)))]))

(define (append-match . args)
    (match args
        [() '()]
        [(a) a ]
        [(a . b) (append2 a (apply append b))]))

(define (make-list num . args)
    (define (maker n init)
        (if (= n 0)
            '()
            (cons init (maker (- n 1) init))))
    (maker num (if (null? args) #f (car args))))

(define (make-list2 num . args)
    (let-optionals* args ((init #f))
        (define (maker n)
            (if (= n 0)
                '()
                (cons init (maker (- n 1)))))
        (maker num)))

(define (person . args)
    (let-keywords args ((name "Anonymous")
                        (age  "unknown"))
        (print name " is " age " year(s) old.")))

(define (person2 . args)
    (let-keywords args ((name "Anonymous")
                        (age  "unknown")
                        . other-args)
        (print name " is " age " year(s) old.")
        (print "Other info: " other-args)))

