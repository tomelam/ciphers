#lang racket

(provide english?)

(define DICTIONARY "/Users/rkrishnan/Dropbox/crypto-challenge/extra/dictionary.txt")
(define LETTERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define (get-words-from-dict dict-filename)
  (map symbol->string (file->list dict-filename #:mode 'text)))

(define dict (get-words-from-dict DICTIONARY))

(define (valid-english-word? word)
  (let ([w-l (string->list word)])
    (let ([l-l (string->list LETTERS)])
      (if (string=? word (list->string (filter (λ(a) (member a l-l)) w-l)))
          #t
          #f))))

(define (drop-words-with-nonletters msg)
  (let ([msg-word-list (string-split msg)])
    (for/list ([word msg-word-list] #:when (valid-english-word? word))
      word)))

(define (lookup-dict word score)
  (if (member word dict)
      (+ score 1)
      score))

(define (get-english-count message)
  (let ([msg-up (string-upcase message)])
    (let ([possible-words-list (drop-words-with-nonletters msg-up)])
      (let ([msg-words (string-split message)])
        (cond [(empty? possible-words-list) 0]
              [else (/ (foldl lookup-dict 0 possible-words-list) (length msg-words))])))))

(define (english? msg word-percentage)
  (let ([percentage-english (* (get-english-count msg) 100)])
    (if (>= percentage-english word-percentage)
        #t
        #f)))