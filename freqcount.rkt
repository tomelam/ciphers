#lang racket

(define letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define etaoin  "ETAOINSHRDLCUMWFGYPBVKJXQZ")

(define test-msg "Alan Mathison Turing was a British mathematician, logician, cryptanalyst, and computer scientist. He was highly influential in the development of computer science, providing a formalisation of the concepts of \"algorithm\" and \"computation\" with the Turing machine. Turing is widely considered to be the father of computer science and artificial intelligence. During World War II, Turing worked for the Government Code and Cypher School (GCCS) at Bletchley Park, Britain's codebreaking centre. For a time he was head of Hut 8, the section responsible for German naval cryptanalysis. He devised a number of techniques for breaking German ciphers, including the method of the bombe, an electromechanical machine that could find settings for the Enigma machine. After the war he worked at the National Physical Laboratory, where he created one of the first designs for a stored-program computer, the ACE. In 1948 Turing joined Max Newman's Computing Laboratory at Manchester University, where he assisted in the development of the Manchester computers and became interested in mathematical biology. He wrote a paper on the chemical basis of morphogenesis, and predicted oscillating chemical reactions such as the Belousov-Zhabotinsky reaction, which were first observed in the 1960s. Turing's homosexuality resulted in a criminal prosecution in 1952, when homosexual acts were still illegal in the United Kingdom. He accepted treatment with female hormones (chemical castration) as an alternative to prison. Turing died in 1954, just over two weeks before his 42nd birthday, from cyanide poisoning. An inquest determined that his death was suicide; his mother and some others believed his death was accidental. On 10 September 2009, following an Internet campaign, British Prime Minister Gordon Brown made an official public apology on behalf of the British government for \"the appalling way he was treated.\" As of May 2012 a private member's bill was before the House of Lords which would grant Turing a statutory pardon if enacted.")

(define (memberi v lst)
  (define (memberi-iter index xs)
    (cond [(empty? xs) #f]
          [(eq? v (first xs))
           index]
          [else (memberi-iter (+ index 1) (rest xs))]))
  (memberi-iter 0 lst))

(define (create-empty-letter-hash)
  (for/fold ([t (hash)])
    ([l (in-string letters)])
    (hash-set t l 0)))

;; String -> (Hashof char number)
(define (get-letter-count msg)
  (for/fold ([freq (create-empty-letter-hash)])
    ([l (in-string msg)])
    (let ([v (hash-ref freq (char-upcase l) #f)])
      (if v
          (hash-set freq (char-upcase l) (+ v 1))
          freq))))

(define (get-frequency-order msg)
  (let ([lc-table (get-letter-count msg)])
    (let ([freq-to-letter (for/fold ([f-to-l (hash)])
                            ([c (in-string letters)])
                            (let ([f (hash-ref lc-table c)])
                              (let ([v (hash-ref f-to-l f #f)])
                                (if v
                                    (hash-set f-to-l f (cons c v))
                                    (hash-set f-to-l f (list c))))))])
      ;; freq-to-letter
      ;; now sort each of the lists corresponding to the frequency counts
      ;; for eg: if E, A both have a freq count of 42, then we want to
      ;; have E first and then A. We sort in the ETAOIN order
      (let ([freq-to-letter-list 
             (hash->list (for/fold ([t freq-to-letter])
                           ([k (in-hash-keys freq-to-letter)])
                           (hash-set t k (sort (hash-ref t k) >
                                               #:key (λ (x)
                                                       (memberi x (string->list etaoin)))))))])
        (list->string (flatten (map rest (sort freq-to-letter-list
                                               >
                                               #:key first))))))))

(define (english-freq-match-score msg)
  (let ([freq-order (get-frequency-order msg)])
    (+ (for/sum ([c (substring etaoin 0 6)]
                 #:when (member c (string->list (substring freq-order 0 6))))
                1)
       (for/sum ([c (substring etaoin 20)]
                 #:when (member c (string->list (substring freq-order 20))))
                1))))
