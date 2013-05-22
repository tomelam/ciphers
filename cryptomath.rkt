#lang racket

(provide gcd modulo-inverse)

(define (gcd a b)
  (cond [(= a 0) b]
        [else (gcd (modulo b a) a)]))

(define (modulo-inverse a m)
  (cond [(not (= (gcd a m) 1)) #f]
        [else 
         (let loop ([u1 1]
                    [u2 0]
                    [u3 a]
                    [v1 0]
                    [v2 1]
                    [v3 m])
           (if (= v3 0)
               (modulo u1 m)
               (let ([q (quotient u3 v3)])
                 (loop v1 v2 v3 (- u1 (* q v1)) (- u2 (* q v2)) (- u3 (* q v3))))))]))