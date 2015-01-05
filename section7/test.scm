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
