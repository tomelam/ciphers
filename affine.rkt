#lang racket

(require "cryptomath.rkt"
         "english.rkt")

(define SYMBOLS "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwzyz123456789")

(define (memberi v lst)
  (define (memberi-iter index xs)
    (cond [(empty? xs) #f]
          [(eq? v (first xs))
           index]
          [else (memberi-iter (+ index 1) (rest xs))]))
  (memberi-iter 0 lst))

(define (get-key-parts key)
  (let ([l (string-length SYMBOLS)])
    (quotient/remainder key l)))

(define (encrypt msg key)
  (let-values ([(key-a key-b) (get-key-parts key)])
    (let ([len-symbols (string-length SYMBOLS)])
      (if (not (= (gcd key-a len-symbols) 1))
          (error "Key A and symbol-size are not relatively prime. Choose a different key.")
          (let ([msg-list (string->list msg)]
                [symbol-list (string->list SYMBOLS)])
            (list->string (foldr (λ (v acc)
                                   (let ([index (memberi v symbol-list)])
                                     (if index
                                         (cons (string-ref SYMBOLS (modulo (+ (* index key-a) key-b) 
                                                                           len-symbols))
                                               acc)
                                         (cons v acc))))
                                 empty
                                 msg-list)))))))

(define (decrypt msg key)
  (let-values ([(key-a key-b) (get-key-parts key)])
    (let ([len-symbols (string-length SYMBOLS)])
      (if (not (= (gcd key-a len-symbols) 1))
          (error "Key A and symbol-size are not relatively prie. Choose a different key.")
          (let ([msg-list (string->list msg)]
                [symbol-list (string->list SYMBOLS)]
                [mod-inv-key-a (modulo-inverse key-a len-symbols)])
            (list->string (foldr (λ (v acc)
                                   (let ([index (memberi v symbol-list)])
                                     (if index
                                         (cons (string-ref SYMBOLS (modulo (* (- index key-b) mod-inv-key-a)
                                                                           len-symbols))
                                               acc)
                                         (cons v acc))))
                                 empty
                                 msg-list)))))))

(define (msg-crack msg)
  (let ([range (expt (string-length SYMBOLS) 2)])
    (let loop ([key 1])     
      (let-values ([(ka kb) (get-key-parts key)])
        (cond [(not (= (gcd ka (string-length SYMBOLS)) 1)) (loop (+ key 1))]
              [(english? (decrypt msg key) 0.8)
               (string-trim (decrypt msg key))]
              [else (if (= key range) 
                        #f
                        (loop (+ key 1)))])))))