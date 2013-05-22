#lang racket

(provide get-word-pattern)

;; cracking
(define (get-word-pattern word)
  (let ([wl (string->list (string-upcase word))])
    (let-values ([(n wp next) (for/fold ([letter-nums (hash)]
                                         [word-pattern empty]
                                         [next-num 0])
                                ((letter word))
                                (if (not (hash-ref letter-nums letter #f))
                                    (values (hash-set letter-nums letter (number->string next-num))
                                            (append word-pattern (list (number->string next-num)))
                                            (+ next-num 1))
                                    (values letter-nums 
                                            (append word-pattern 
                                                    (list (hash-ref letter-nums letter))) 
                                            next-num)))])
      (string-join wp "."))))

(define (main)
  (let ([wl (file->list "dictionary.txt")])
    (let ([patterns
           (for/fold ([all-patterns (hash)])
             ([word (map symbol->string wl)])
             (let ([p (get-word-pattern word)])
               (if (hash-has-key? all-patterns p)
                   (hash-set all-patterns p (append (hash-ref all-patterns p) (list word)))
                   (hash-set all-patterns p (list word)))))])
      (parameterize ([print-hash-table #t])
        (let ([fo (open-output-file "word-patterns.rkt"
                                    #:mode 'text
                                    #:exists 'replace)])
          (pretty-write patterns fo)
          (close-output-port fo))))))

(module+ main
  (main))