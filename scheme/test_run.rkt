(define A (lambda (k x1 x2 x3 x4 x5) 
                    (define B (lambda () 
                        ;; (displayln k)
                        (set! k (- k 1)) (A k B x1 x2 x3 x4)))
                    (if (<= k 0) (+ (x4) (x5)) (B))))

(define (man-or-boy n) (A n (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0)))

(displayln (quote result))
(displayln (man-or-boy 4))

;; (exit)