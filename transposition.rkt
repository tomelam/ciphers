#lang racket

(require rackunit
         "english.rkt")

;; string -> char -> number -> string
(define (pad-string s pad-char n)
  (let ([l (string-length s)])
    (let ([r (remainder l n)])
      (if (= r 0)
          s
          (string-append s (make-string (- n r) pad-char))))))

;; string -> number -> (Listof string)
;; assumes that the input string is already padded to be of right length
(define (split-string s n)
  (cond [(string=? s "") empty]
        [else (cons (substring s 0 n)
                    (split-string (substring s n) n))]))

(define (transposition input key)
  (let ([chunks (split-string (pad-string input #\space key) key)])
    (let ([chunk-lists (map string->list chunks)])
      (string-append* (map list->string (apply map list chunk-lists))))))

(define (transposition-decrypt input enc-key)
  (let ([l (string-length input)])
    (let ([dec-key (if (= (remainder l enc-key) 0) 
                       enc-key
                       (* (+ (quotient l enc-key) 1) enc-key))])
      (transposition input dec-key))))

(define (msg-crack msg)
  (let loop ([key 1])
    (cond [(= key (string-length msg)) (error "could not crack the message")]
          [(english? (transposition-decrypt msg key) 0.8)
           (string-trim (transposition-decrypt msg key))]
          [else (loop (+ key 1))])))

(define (msg-crack2 msg)
  (for/list ([k (in-range 1 (string-length msg))]
             #:when (english? (transposition-decrypt msg k) 0.8))
    k))


