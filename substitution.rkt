#lang racket

(require rackunit
         "make-word-patterns.rkt")

(define letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define key "LFWOAYUISVKMNXPBDCRJTQEGHZ")

(define (valid-key? key)
  (string=? (list->string (sort (string->list key) char<?)) letters))

(define (gen-random-key symbols)
  (list->string (shuffle (string->list symbols))))

(define (make-subst-table ks vs)
  (foldl (λ (k v acc)
           (hash-set acc k v))
         (hash)
         (string->list ks)
         (string->list vs)))

(define (encrypt msg key)
  (let ([t (make-subst-table letters key)])
    (list->string (map (λ (a)
                         (if (char-upper-case? a)
                             (char-upcase (hash-ref t (char-upcase a) a))
                             (char-downcase (hash-ref t (char-upcase a) a))))
                       (string->list msg)))))

;; decrypting
;; (look at make-word-patterns.rkt and the file it generates ("word-patterns.rkt")
;; one could read the generated pattern as 
;;    (define ht (read (open-input-file "word-patterns.rkt")))
;; steps to crack substitution cipher
;; 1. split cipher text into cipher words
;; 2. form pattern for each word
;; 3. lookup the dictionary patterns for words that have same pattern
;; 4. for each cipher word, step 3 will give one or more english words
;; 5. for each english word, form a cipher letter -> english letter mapping
;; 6. keep merging the mappings for a single cipher word to yield a map.
;; 6. step 6 will give a cipher letter that maps to zero or more letters in english.
;;    zero mapping implies any letter, since we don't know what it maps to yet.
;; 7. Do steps 4-6 for each cipher word.
;; 8. keep intersecting the maps when done with each cipher word.
;; 9. eliminate ambiguous mappings (i.e. solved letters (letters that map
;;    to just one english letter) should be removed from other mappings that map
;;    to multiple letters, yielding a more fine grained mapping.
;; 10. Use this new mapping at the end of step 9 to decrypt the cipher text.

(define wp-table (read (open-input-file "word-patterns.rkt")))

(define (make-empty-cipher-letter-mapping)
  (let ([letters-list (string->list letters)])
    (for/fold ([m (hash)])
      ([l letters-list])
      (hash-set m l empty))))

(define (add-letters-to-mapping base-table cipher-word candidate)
  (for/fold ([m base-table])
    ([cipher-char (in-string cipher-word)]
     [candidate-char (in-string candidate)])
    (let ([possible-chars (hash-ref m cipher-char)])
      (if (member candidate-char possible-chars)
          m
          (hash-set m cipher-char (cons candidate-char possible-chars))))))

(define (intersect-map m1 m2)
  ;; todo: check if keys of m1 and keys of m2 should be the same
  (let ([keys (hash-keys m1)])
    (for/fold ([m m1])
      ([k keys])
      (let ([val1 (hash-ref m1 k)]
            [val2 (hash-ref m2 k)])
        (cond [(and (empty? val1)
                    (empty? val2))
               (hash-set m k empty)]
              [(and (empty? val1)
                    (not (empty? val2)))
               (hash-set m k val2)]
              [(and (not (empty? val1))
                    (empty? val2))
               (hash-set m k val1)]
              [else
               (hash-set m k (set->list (set-intersect (list->set val1)
                                                       (list->set val2))))])))))

(define (remove-solved-letters-from-mapping map)
  (define (get-singles key-list)
    (cond [(empty? key-list) empty]
          [(= (length (hash-ref map (first key-list))) 1)
           (cons (first (hash-ref map (first key-list))) 
                 (get-singles (rest key-list)))]
          [else (get-singles (rest key-list))]))
  (let ([singles-list (get-singles (hash-keys map))])
    (for/fold ([m map])
      ([k (hash-keys map)] #:when (> (length (hash-ref m k)) 1))
      (let ([vals (hash-ref m k)])
        (hash-set m k (set->list (set-subtract (list->set vals)
                                               (list->set singles-list))))))))

(define (decrypt-with-mapping msg mapping)
  (let ([msg-list (string->list msg)])
    (list->string (map (λ (c)
                         (first (hash-ref mapping c (list c))))
                       msg-list))))

(define (crack-substitution msg)
  (let ([cipher-words (string-split msg)])
    (let ([cipher-word-patterns (map get-word-pattern cipher-words)])
      (let ([get-words-with-pattern (map (λ (pattern)
                                           (hash-ref wp-table pattern))
                                         cipher-word-patterns)])
        ;; get-words-with-pattern is a list of lists
        (let ([mapping (for/fold ([m (make-empty-cipher-letter-mapping)])
                         ([possibles-list get-words-with-pattern]
                          [cword cipher-words])
                         (intersect-map m (for/fold ([m (make-empty-cipher-letter-mapping)])
                                            ([p possibles-list])
                                            (add-letters-to-mapping m cword p))))])
          (let ([uniq-map (remove-solved-letters-from-mapping mapping)])
            (decrypt-with-mapping msg uniq-map)))))))