(define map (lambda (f l) 
    (if (null? l) null 
        (cons (f (car l)) (map f (cdr l))))))


(define (displayln line)
    (begin
        (display line)
        (newline)))