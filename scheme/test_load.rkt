(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))

(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))

;; (exit)