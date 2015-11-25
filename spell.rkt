
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2015                                *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.rkt")

;; contains simple dictionary definition
(load "test-dictionary.rkt")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***


;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (if (null? w) 5387
     (+ (* 31 (key (cdr w))) (ctv (car w))))
))

(key '(h e l l o))
(key '(w a y))
(key '(r a i n b o w))
'--------

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))     = 154238504134
;;   (key '(w a y))         = 160507203 
;;   (key '(r a i n b o w)) = 148230379423562

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (l)
       (modulo (key l) size))
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda(l)
       (floor (* size (- (* (key l) A) (floor (* (key l) A))))))
))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 700224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

  (hash-1 '(h e l l o))     
  (hash-1 '(w a y))         
  (hash-1 '(r a i n b o w)) 
  '------
  (hash-2 '(h e l l o))     
  (hash-2 '(w a y))         
  (hash-2 '(r a i n b o w)) 
  '------
  (hash-3 '(h e l l o))     
  (hash-3 '(w a y))         
  (hash-4 '(r a i n b o w)) 
  '------
  (hash-4 '(h e l l o))     
  (hash-4 '(w a y))         
  (hash-4 '(r a i n b o w))
  '------

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))     ==> 53236
;;  (hash-1 '(w a y))         ==> 23124 
;;  (hash-1 '(r a i n b o w)) ==> 17039 
;;
;;  (hash-2 '(h e l l o))     ==> 25588 
;;  (hash-2 '(w a y))         ==> 42552 
;;  (hash-2 '(r a i n b o w)) ==> 70913 
;;
;;  (hash-3 '(h e l l o))     ==> 415458.0 
;;  (hash-3 '(w a y))         ==> 390702.0 
;;  (hash-4 '(r a i n b o w)) ==> 503286.0 
;;
;;  (hash-4 '(h e l l o))     ==> 533.0
;;  (hash-4 '(w a y))         ==> 502.0
;;  (hash-4 '(r a i n b o w)) ==> 646.0



;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define hashWord
  (lambda (word hashfunctionlist)
    (if (null? hashfunctionlist) '()
        (append (cons ((car hashfunctionlist) word) '()) (hashWord word (cdr hashfunctionlist))))
))
  
(define makeList
  (lambda (dict hashfunctionlist)
    (if (null? hashfunctionlist) '()
        (append (map (car hashfunctionlist) dict) (makeList dict (cdr hashfunctionlist))))
))

(define searchList
  (lambda (list obj)
    (cond ((null? list) (cons 0 '()))
          ((= (car list) obj) (cons 1 '()))
          (else (searchList (cdr list) obj)))
))
    
(define inList?
  (lambda (list wordhash)
      (if (null? wordhash) '()
          (append (searchList list (car wordhash)) (inList? list (cdr wordhash)))
      )
))

(define calculateList
  (lambda (hashfunctionlist dict word)
  (reduce * (inList? (makeList dict hashfunctionlist) (hashWord word hashfunctionlist)) 1)
))

(define gen-checker
  (lambda (hashfunctionlist dict)
     (lambda (word)
       (if (= (calculateList hashfunctionlist dict word) 1) #t #f)
     )
))

(hashword '(a r g g g g) hashfl-1)

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

(checker-1 '(a r g g g g))
(checker-2 '(h e l l o))
(checker-1 '(h e l l o))
(checker-1 '(w a y))
(checker-1 '(r a i n b o w))
(checker-1 '(r a i n b o))
(checker-3 '(a b a n d o e d))
(checker-1 '(h e a v y w e i g h t))
(checker-1 '(h e a v y w e i l h t))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t

