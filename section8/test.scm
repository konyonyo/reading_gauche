(define (any-pred . preds)
    (lambda (x)
        (fold (lambda (pred res)
                  (or (pred x) res))
            #f preds)))

(define (every-pred . preds)
    (lambda (x)
        (fold (lambda (pred res)
                  (and (pred x) res))
            #t preds)))

