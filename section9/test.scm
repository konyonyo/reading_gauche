(use srfi-1)

(define (member elt lis . options)
    (let-optionals* options ((cmp-fn equal?))
        (cond [(null? lis) #f]
              [(cmp-fn elt (car lis)) lis]
              [else (member elt (cdr lis) cmp-fn)])))

(define (member2 elt lis . options)
    (let-optionals* options ((cmp-fn equal?))
        (define (loop lis)
            (cond [(null? lis) #f]
                  [(cmp-fn elt (car lis)) lis]
                  [else (loop (cdr lis))]))
        (loop lis)))

(define (delete elt lis . options)
    (let-optionals* options ((cmp-fn equal?))
        (define (loop lis)
            (cond [(null? lis) '()]
                  [(cmp-fn elt (car lis)) (loop (cdr lis))]
                  [else (cons (car lis) (loop (cdr lis)))]))
        (loop lis)))

(define (delete-1 elt lis . options)
    (let-optionals* options ((cmp-fn equal?))
        (define (loop lis)
            (cond [(null? lis) '()]
                  [(cmp-fn elt (car lis)) (cdr lis)]
                  [else (cons (car lis) (loop (cdr lis)))]))
        (loop lis)))

