#lang racket

(require rackunit)

(define letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define (memberi v lst)
  (define (memberi-iter index xs)
    (cond [(empty? xs) #f]
          [(eq? v (first xs))
           index]
          [else (memberi-iter (+ index 1) (rest xs))]))
  (memberi-iter 0 lst))

;; string -> number -> string
(define (caesar input key)
  (let ([inp-list (string->list input)])
    (list->string (foldr (λ (v acc)
                           (let ([index (memberi v (string->list letters))])
                             (if index
                                 (cons (string-ref letters (modulo (+ index key) 26)) acc)
                                 (cons v acc))))
                         empty
                         inp-list))))

(define (caesar-decrypt input key)
  (caesar input (- (string-length letters) key)))

(check-equal? (caesar-decrypt (caesar "THIS IS A SECRET MESSAGE" 13) 13) 
              "THIS IS A SECRET MESSAGE")
