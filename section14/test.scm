(define (write-to-string exp)
    (call-with-output-string
        (lambda (out)
            (write exp out))))

(define (read-from-string str)
    (call-with-input-string str
        (lambda (in)
            (read in))))

