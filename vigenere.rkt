#lang racket

(define letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define (repeat-key key msg)
  (let ([kl (string-length key)]
        [ml (string-length msg)])
    (let-values ([(q r) (quotient/remainder ml kl)])
      (string-join (append (for/list ([i q])
                             key)
                         (list (substring key 0 r)))
                   ""))))

(define (get-index c s)
  (let ([str-list (string->list s)])
    (cond [(empty? str-list) (error "cannot find the given letter in the string")]
          [(char=? c (first str-list)) 0]
          [else (+ 1 (get-index c (list->string (rest str-list))))])))

(define (map-letter msg-list key-list key-index #:encrypt [encrypt? #t])
  (let ([operator (if encrypt? + -)])
    (cond [(empty? msg-list) empty]
          [else (let ([c (first msg-list)])
                  (if (not (member (char-upcase c) (string->list letters)))
                      (cons c (map-letter (rest msg-list)
                                          key-list
                                          key-index
                                          #:encrypt encrypt?))
                      (let ([mapped-c (string-ref letters 
                                                  (modulo (operator (get-index (char-upcase c) letters)
                                                                    (get-index (char-upcase (list-ref key-list key-index)) 
                                                                               letters))
                                                          (string-length letters)))])
                        (let ([mapped-rest (map-letter (rest msg-list)
                                                       key-list
                                                       (modulo (+ key-index 1) (length key-list))
                                                       #:encrypt encrypt?)])
                          (if (char-upper-case? c)
                              (cons (char-upcase mapped-c)
                                    mapped-rest)
                              (cons (char-downcase mapped-c)
                                    mapped-rest))))))])))


(define (translate msg key #:ignorespace [ignorespace? #f] #:encrypt [encrypt? #t])
  (let ([key-len (string-length key)]
        [msg-len (string-length (if ignorespace? (string-join (string-split msg) "") msg))])
    ;; assumption: key-len <<< msg-len
    (if (< key-len msg-len)
        (let ([key-str (repeat-key key msg)])
          (list->string (map-letter (string->list msg)
                                    (string->list key)
                                    0
                                    #:encrypt encrypt?)))
        (error "try a bigger message!"))))

;; vigenere
(define test-enc-msg "Adiz Avtzqeci Tmzubb wsa m Pmilqev halpqavtakuoi, lgouqdaf, kdmktsvmztsl, izr xoexghzr kkusitaaf. Vz wsa twbhdg ubalmmzhdad qz hce vmhsgohuqbo ox kaakulmd gxiwvos, krgdurdny i rcmmstugvtawz ca tzm ocicwxfg jf \"stscmilpy\" oid \"uwydptsbuci\" wabt hce Lcdwig eiovdnw. Bgfdny qe kddwtk qjnkqpsmev ba pz tzm roohwz at xoexghzr kkusicw izr vrlqrwxist uboedtuuznum. Pimifo Icmlv Emf DI, Lcdwig owdyzd xwd hce Ywhsmnemzh Xovm mby Cqxtsm Supacg (GUKE) oo Bdmfqclwg Bomk, Tzuhvif'a ocyetzqofifo ositjm. Rcm a lqys ce oie vzav wr Vpt 8, lpq gzclqab mekxabnittq tjr Ymdavn fihog cjgbhvnstkgds. Zm psqikmp o iuejqf jf lmoviiicqg aoj jdsvkavs Uzreiz qdpzmdg, dnutgrdny bts helpar jf lpq pjmtm, mb zlwkffjmwktoiiuix avczqzs ohsb ocplv nuby swbfwigk naf ohw Mzwbms umqcifm. Mtoej bts raj pq kjrcmp oo tzm Zooigvmz Khqauqvl Dincmalwdm, rhwzq vz cjmmhzd gvq ca tzm rwmsl lqgdgfa rcm a kbafzd-hzaumae kaakulmd, hce SKQ. Wi 1948 Tmzubb jgqzsy Msf Zsrmsv'e Qjmhcfwig Dincmalwdm vt Eizqcekbqf Pnadqfnilg, ivzrw pq onsaafsy if bts yenmxckmwvf ca tzm Yoiczmehzr uwydptwze oid tmoohe avfsmekbqr dn eifvzmsbuqvl tqazjgq. Pq kmolm m dvpwz ab ohw ktshiuix pvsaa at hojxtcbefmewn, afl bfzdakfsy okkuzgalqzu xhwuuqvl jmmqoigve gpcz ie hce Tmxcpsgd-Lvvbgbubnkq zqoxtawz, kciup isme xqdgo otaqfqev qz hce 1960k. Bgfdny'a tchokmjivlabk fzsmtfsy if i ofdmavmz krgaqqptawz wi 1952, wzmz vjmgaqlpad iohn wwzq goidt uzgeyix wi tzm Gbdtwl Wwigvwy. Vz aukqdoev bdsvtemzh rilp rshadm tcmmgvqg (xhwuuqvl uiehmalqab) vs sv mzoejvmhdvw ba dmikwz. Hpravs rdev qz 1954, xpsl whsm tow iszkk jqtjrw pug 42id tqdhcdsg, rfjm ugmbddw xawnofqzu. Vn avcizsl lqhzreqzsy tzif vds vmmhc wsa eidcalq; vds ewfvzr svp gjmw wfvzrk jqzdenmp vds vmmhc wsa mqxivmzhvl. Gv 10 Esktwunsm 2009, fgtxcrifo mb Dnlmdbzt uiydviyv, Nfdtaat Dmiem Ywiikbqf Bojlab Wrgez avdw iz cafakuog pmjxwx ahwxcby gv nscadn at ohw Jdwoikp scqejvysit xwd \"hce sxboglavs kvy zm ion tjmmhzd.\" Sa at Haq 2012 i bfdvsbq azmtmd'g widt ion bwnafz tzm Tcpsw wr Zjrva ivdcz eaigdyzmbo Tmzubb a kbmhptgzk dvrvwz wa efiohzd.")

