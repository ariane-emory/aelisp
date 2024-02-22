;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nl)
(confirm that (feature? 'std)                  returns t)
(setq   lst  (unionql '(1 2 3) '(4 5 6)))
(confirm that lst                              returns '(6 5 4 3 2 1))
(confirm that (memql?   3 lst)                 returns '(3 2 1))
(confirm that (removeql 4 lst)                 returns '(6 5 3 2 1))
(confirm that (indexql  5 lst)                 returns 1)
(confirm that (mapcar! double lst)             returns '(12 10 8 6 4 2))

(confirm that (butlast '(1 2 3 4 5) 0)         returns '(1 2 3 4 5))
(confirm that (butlast '(1 2 3 4 5) 1)         returns '(1 2 3 4))
(confirm that (butlast '(1 2 3 4 5) 2)         returns '(1 2 3))
(confirm that (butlast '(1 2 3 4 5) 3)         returns '(1 2))
(confirm that (butlast '(1 2 3 4 5) 4)         returns '(1))
(confirm that (butlast '(1 2 3 4 5) 5)         returns nil)
(confirm that (butlast '(1 2 3 4 5) 6)         returns nil)
(confirm that (last '(1 2 3))                  returns '(3))
(confirm that (reverse '(1 2 3 4 5))           returns '(5 4 3 2 1))
(confirm that (reverse (butlast '(1 2 3 4 5))) returns '(4 3 2 1))
(confirm that (unionql '(1 2 3) '(4 5 6))      returns '(6 5 4 3 2 1))
(confirm that (append2 '(1 2 3) '(4 5 6))      returns '(1 2 3 4 5 6))

(setq   lst  (make-list 6 200))
(confirm that lst                              returns '(200 200 200 200 200 200))
(confirm that (list-set! lst 0 100)            returns 100)
(confirm that (list-ref  lst 0)                returns 100)
(confirm that lst                              returns '(100 200 200 200 200 200))
(confirm that (list-set! lst 1 101)            returns 101)
(confirm that (list-ref  lst 1)                returns 101)
(confirm that lst                              returns '(100 101 200 200 200 200))
(confirm that (list-set! lst 2 102)            returns 102)
(confirm that (list-ref  lst 2)                returns 102)
(confirm that lst                              returns '(100 101 102 200 200 200))
(confirm that (list-set! lst 3 103)            returns 103)
(confirm that (list-ref  lst 3)                returns 103)
(confirm that lst                              returns '(100 101 102 103 200 200))
(confirm that (list-set! lst 4 104)            returns 104)
(confirm that (list-ref  lst 4)                returns 104)
(confirm that lst                              returns '(100 101 102 103 104 200))
(confirm that (list-set! lst 5 105)            returns 105)
(confirm that (list-ref  lst 5)                returns 105)
(confirm that lst                              returns '(100 101 102 103 104 105))

(defun lshift4 (n) (<< n 4))
(add-logging-to lshift4)
(confirm that (lshift4 4) returns 64)

(unless *microbench-defuns*
 "For some reason, this test fails if *microbench-defuns* is set to t. Investigate."
 (confirm that (body lshift4)
  returns
  '(progn
    (princ "Applying " 'lshift4 " to parameters " (syms (env)) " with arguments " (vals (env)) ".") 
    (nl)
    (let ((result (progn (<< n 4))))
     (princ "Result of applying " 'lshift4 " was " result ".") (nl)
     result))))

(setq lst $(2 4 1 5 3 7 9 6 8))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 9)) lst) lst) returns '(9 2 4 1 5 3 7 6 8))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 8)) lst) lst) returns '(8 9 2 4 1 5 3 7 6))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 7)) lst) lst) returns '(7 8 9 2 4 1 5 3 6))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 6)) lst) lst) returns '(6 7 8 9 2 4 1 5 3))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 5)) lst) lst) returns '(5 6 7 8 9 2 4 1 3))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 4)) lst) lst) returns '(4 5 6 7 8 9 2 1 3))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 3)) lst) lst) returns '(3 4 5 6 7 8 9 2 1))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 2)) lst) lst) returns '(2 3 4 5 6 7 8 9 1))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 1)) lst) lst) returns '(1 2 3 4 5 6 7 8 9))

(confirm that (nthcdr 0 lst) returns '(1 2 3 4 5 6 7 8 9))
(confirm that (nthcdr 1 lst) returns '(2 3 4 5 6 7 8 9))
(confirm that (nthcdr 2 lst) returns '(3 4 5 6 7 8 9))
(setq c (curry1 nthcdr 3))
(confirm that (c lst) returns '(4 5 6 7 8 9))
(confirm that (pop lst)      returns 1)
(confirm that lst            returns '(2 3 4 5 6 7 8 9))
(confirm that (push 99 lst)  returns '(99 2 3 4 5 6 7 8 9))
(setq lst '("asdw" "erer" "rerw" 1 nil (lambda (x) x) zoop z (1 2 . 3) 8))
(setq split (split-list string? lst))
(confirm that (cadr split) returns '(1 nil (lambda (x) x) zoop z (1 2 . 3) 8))

(ignore
 "This test cannot work until handling of strings containing escaped double quotes is fixed,"
 "and so for now we will ignore it."
 (confirm that (apply concat (intercalate " " (car split))) returns "\"asdw erer rerw\""))

(confirm that (bin-list-to-int  '(1 0 1 0 1 0 1))  returns 85)
(confirm that (bin-list-to-int*   1 0 1 0 1 0 1)   returns 85)

(confirm that (reduce  + '()          )            returns nil)
(confirm that (reduce  + '(1)         )            returns 1)
(confirm that (reduce  + '(1 2)       )            returns 3)
(confirm that (reduce  + '(1 2 3)     )            returns 6)
(confirm that (reduce  + '(1 2 3 4)   )            returns 10)
(confirm that (reduce  + '()         0)            returns 0)
(confirm that (reduce  + '(1)        0)            returns 1)
(confirm that (reduce  + '(1 2)      0)            returns 3)
(confirm that (reduce  + '(1 2 3)    0)            returns 6)
(confirm that (reduce  + '(1 2 3 4)  0)            returns 10)
(confirm that (rreduce + '()          )            returns nil)
(confirm that (rreduce + '(1)         )            returns 1)
(confirm that (rreduce + '(1 2)       )            returns 3)
(confirm that (rreduce + '(1 2 3)     )            returns 6)
(confirm that (rreduce + '(1 2 3 4)   )            returns 10)
(confirm that (rreduce + '()         0)            returns 0)
(confirm that (rreduce + '(1)        0)            returns 1)
(confirm that (rreduce + '(1 2)      0)            returns 3)
(confirm that (rreduce + '(1 2 3)    0)            returns 6)
(confirm that (rreduce + '(1 2 3 4)  0)            returns 10)

(confirm that (zip  '(a b c) '(1 2 3))             returns '((a 1)   (b 2)   (c 3)))
(confirm that (zip  '(a b c) '(1 2 3) '(x y z))    returns '((a 1 x) (b 2 y) (c 3 z)))
;; (confirm that (zip* '(1 2 3 4) '(a b c) '(x y z p q)) returns '((1 a x) (2 b y) (3 c z)))

(setq   lst                                               '(7 5 4 6 3 9 1 6 2 8 6))
(confirm that (sort!! lst <)                       returns '(1 2 3 4 5 6 6 6 7 8 9))

(confirm that ((reduced   +  ) $(1 2 3 4))         returns 10)
(confirm that ((reduced   + 4) $(1 2 3  ))         returns 10)
(confirm that ((rreduced  +  ) $(1 2 3 4))         returns 10)
(confirm that ((rreduced  + 4) $(1 2 3  ))         returns 10)
(confirm that ((reduced   +  ) '(1 2 3 4))         returns 10)
(confirm that ((reduced   + 4) '(1 2 3  ))         returns 10)
(confirm that ((rreduced  +  ) '(1 2 3 4))         returns 10)
(confirm that ((rreduced  + 4) '(1 2 3  ))         returns 10)
(confirm that ((reduced*  +  )   1 2 3 4 )         returns 10)
(confirm that ((reduced*  + 4)   1 2 3   )         returns 10)
(confirm that ((rreduced* +  )   1 2 3 4 )         returns 10)
(confirm that ((rreduced* + 4)   1 2 3   )         returns 10)

(setq lst '(1 nil 2 3 nil 4 nil))
(confirm that (delq! nil lst)                      returns '(1 2 3 4))
(setq lst '(1 2 3 2 4 2 5))
(confirm that (delq! 2 lst)                        returns '(1 2 3 2 4 2 5))
(setq lst '(1 2 3 2 4 2 5))
(confirm that (delql! 2 lst)                       returns '(1 3 4 5))
(setq lst '(1 2 3 2 4 2 5))
(confirm that (delql! 1 lst)                       returns '(2 3 2 4 2 5))
(setq lst '((1 2) (3 4) (5 6)))
(defun filter-evens (lst) (delq! nil (mapcar (lambda (x) (when (even? x) x)) lst)))
;(log-core t)
(confirm that (mapcan filter-evens lst) returns '(2 4 6))
(confirm that (apply concat (intercalate " " '("These"  "are" "words")))  returns "These are words")
(confirm that (mapconcat string '(These  are  words) " ")                 returns "These are words")
(confirm that (mapconcat string '(These "are" words) " ")                 returns "These are words")
(confirm that (append      '(1 2) '(3 4)    '(5 6))                       returns '(1 2 3 4 5 6))
(confirm that (append      $(1 2) '(3 4)    $(5 6))                       returns '(1 2 3 4 5 6))
(confirm that (mapcan id  '((1 2)  (3 4)     (5 6)))                      returns '(1 2 3 4 5 6))
(confirm that (mapcan id  '((1 2)  (3 4) nil (5 6)))                      returns '(1 2 3 4 5 6))
(confirm that (mapcan cdr '((1 2 3) (4 5 6) (7 8 9)))                     returns '(2 3 5 6 8 9))
(confirm that (prime? 7)                                                  returns t)
(confirm that (prime? 8)                                                  returns nil)
(confirm that (primes 8)                                                  returns '(2 3 5 7 11 13 17 19))
(setq lst '(1 2 3 4))
(confirm that (mapcar! double lst)                                        returns '(2 4 6 8))
(confirm that lst                                                         returns '(2 4 6 8))
(confirm that (subst '(1 2 3 4 5 6 7 (8 5 9 5 10)) 5 'five)               returns '(1 2 3 4 five 6 7 (8 five 9 five 10)))
(confirm that (transform-tree even? double '(1 2 3 4 5 6 7 (8 5 9 5 10))) returns '(1 4 3 8 5 12 7 (16 5 9 5 20)))
(setq   lst  '(a 2 3 (b 4 x (y 5)) (6 nil 7)))
(confirm that (transform-tree  integer? double lst)                       returns '(a 4 6 (b 8 x (y 10)) (12 nil 14)))
(confirm that lst                                                         returns '(a 2 3 (b 4 x (y  5)) (6  nil  7)))
(confirm that (transform-tree! integer? double lst)                       returns '(a 4 6 (b 8 x (y 10)) (12 nil 14)))
(confirm that lst                                                         returns '(a 4 6 (b 8 x (y 10)) (12 nil 14)))
(confirm that (make-string      10 "x")                                   returns "xxxxxxxxxx")
(confirm that (pad-string-right 10 "x" "hello")                           returns "helloxxxxx")
(confirm that (pad-string-left  10 "x" "hello")                           returns "xxxxxhello")
(confirm that (mapconcat id '("hello" "to" "the" "world") " ")            returns "hello to the world")
(confirm that (mapconcat id '("hello" "to" "the" "world"))                returns "hellototheworld")
(confirm that (sort!! '(1 2 3 4 5 6) (lambda (x y) (and (even? x) (not (even? y))))) returns '(6 4 2 5 3 1))
(setq   lst  (sort!! '(1 2 3 4 5 6) (lambda (x y) (and (even? x) (not (even? y))))))
(confirm that (filter even? lst) returns '(6 4 2))
(confirm that (funcall + 1 2 3 4 5 6) returns 21)
(confirm that (apply concat (mapcar* (curry1 ljust 20) "hello" "to" "the" "world"))
 returns "hello               to                  the                 world               ")
(confirm that (prog1 1 2 3) returns 1)
(confirm that (prog2 1 2 3) returns 2)

(confirm that (setq pl '(a 10 b 20 c 30 d 40 e 50))   returns '(a 10 b 20 c 30 d 40  e 50))

(confirm that (plist-set  pl 'a 1)                     returns '(a 1  b 20 c 30 d 40  e 50))
(confirm that pl                                       returns '(a 10 b 20 c 30 d 40  e 50))
(confirm that (plist-set! pl 'a 1)                     returns '(a 1  b 20 c 30 d 40  e 50))

(confirm that (plist-set  pl 'b 2)                     returns '(a 1  b 2  c 30 d 40  e 50))
(confirm that pl                                       returns '(a 1  b 20 c 30 d 40  e 50))
(confirm that (plist-set! pl 'b 2)                     returns '(a 1  b 2  c 30 d 40  e 50))

(confirm that (plist-set  pl 'c 3)                     returns '(a 1  b 2  c 3  d 40  e 50))
(confirm that pl                                       returns '(a 1  b 2  c 30 d 40  e 50))
(confirm that (plist-set! pl 'c 3)                     returns '(a 1  b 2  c 3  d 40  e 50))

(confirm that (plist-set  pl 'd nil)                   returns '(a 1  b 2  c 3  d nil e 50))
(confirm that pl                                       returns '(a 1  b 2  c 3  d 40  e 50))
(confirm that (plist-set! pl 'd nil)                   returns '(a 1  b 2  c 3  d nil e 50))

(confirm that (plist-set  pl 'e 5)                     returns '(a 1  b 2  c 3  d nil e 5 ))
(confirm that pl                                       returns '(a 1  b 2  c 3  d nil e 50))
(confirm that (plist-set! pl 'e 5)                     returns '(a 1  b 2  c 3  d nil e 5 ))

(confirm that pl                                       returns '(a 1  b 2  c 3  d nil e 5))

(confirm that (plist-get  pl 'a)                       returns '1)
(confirm that (plist-get  pl 'b)                       returns '2)
(confirm that (plist-get  pl 'c)                       returns '3)
(confirm that (plist-get  pl 'd)                       returns 'nil)
(confirm that (plist-get  pl 'e)                       returns '5)
(confirm that (plist-get  pl 'f)                       returns 'nil)
                            
(confirm that (plist-has? pl 'a)                       returns 't)
(confirm that (plist-has? pl 'b)                       returns 't)
(confirm that (plist-has? pl 'c)                       returns 't)
(confirm that (plist-has? pl 'd)                       returns 't)
(confirm that (plist-has? pl 'e)                       returns 't)
(confirm that (plist-has? pl 'f)                       returns 'nil)

(confirm that (plist-keys pl)                          returns '(a b c d e))
(confirm that (plist-vals pl)                          returns '(1 2 3 nil 5))

(confirm that (plist-remove  pl 'a)                    returns '(b 2 c 3 d nil e 5))
(confirm that pl                                       returns '(a 1 b 2 c 3 d nil e 5))
(confirm that (plist-remove! pl 'a)                    returns '(b 2 c 3 d nil e 5))
                               
(confirm that (plist-remove  pl 'd)                    returns '(b 2 c 3 e 5))
(confirm that (plist-remove! pl 'd)                    returns '(b 2 c 3 e 5))
                               
(confirm that (plist-remove  pl 'e)                    returns '(b 2 c 3))
(confirm that (plist-remove! pl 'e)                    returns '(b 2 c 3))
                               
(confirm that (plist-remove  pl 'c)                    returns '(b 2))
(confirm that (plist-remove! pl 'c)                    returns '(b 2))
                               
(confirm that (plist-remove  pl 'b)                    returns nil)
(confirm that (plist-remove! pl 'b)                    returns '(nil nil))

(confirm that (make-plist '(a b c) '(1 2 3))           returns '(a 1 b 2 c 3))

(confirm that (put!       :quux :foo 'a)               returns 'a)
(confirm that (put!       :quux :bar 'b)               returns 'b)
(confirm that (put!       :quux :baz 'c)               returns 'c)
(confirm that (props      :quux)                       returns '(:baz c :bar b :foo a))

(confirm that (set-props  :quux '(:corge d))           returns '(:corge d))
(confirm that (put!       :quux :foo 'a)               returns 'a)
(confirm that (put!       :quux :bar 'b)               returns 'b)
(confirm that (put!       :quux :baz 'c)               returns 'c)

(confirm that (remove!    :quux :foo)                  returns 'a)
(confirm that (remove!    :quux :bar)                  returns 'b)
(confirm that (remove!    :quux :baz)                  returns 'c)
(confirm that (remove!    :quux :corge)                returns 'd)

(confirm that (props      :quux)                       returns nil)

(confirm that (nconc! '(1 2 3) '(4 5 6) '(7 8 9))      returns '(1 2 3 4 5 6 7 8 9))
(confirm that (append '(1 2 3) '(4 5 6) '(7 8 9))      returns '(1 2 3 4 5 6 7 8 9))

(confirm that (memql?   3 '(1 2 3 4 5 6))              returns '(3 4 5 6))
(confirm that (memql?   9 '(1 2 3 4 5 6))              returns nil)
(confirm that (indexql  3 '(1 2 3 4 5 6))              returns 2)
(confirm that (indexql  9 '(1 2 3 4 5 6))              returns nil)
(confirm that (removeql 3 '(1 2 3 4 5 6))              returns '(1 2 4 5 6))
(confirm that (removeql 9 '(1 2 3 4 5 6))              returns '(1 2 3 4 5 6))

(confirm that (equal? '(1 . (2 . (3 . nil))) '(1 2 3)) returns t)
(confirm that (depth  '(1 2 (3 4 (5)) (6 7)))          returns 3)

(confirm that (min 2 3 1 4 9 8 1 7)                    returns 1)
(confirm that (max 2 3 1 4 9 8 1 7)                    returns 9)

(confirm that (equal? '(:a (:nested list) ("containing" 6 :items)) '(:a (:nested list) ("containing" 6 :items)))
 returns t)
(confirm that (equal? '(:a (:nested list) ("containing" 6 :items)) '(:a (:nested list) ("containing" 6  items)))
 returns nil)

(confirm that (flatten  '("A" (deeply (:nested list)) (of (:many "elements"))))
 returns '("A" deeply  :nested list  of  :many "elements"))
(confirm that (flatten1 '("A" (deeply (:nested list)) (of (:many "elements"))))
 returns '("A" deeply (:nested list) of (:many "elements")))

(defun case-test-fun (n)
 (case n
  (1     (concat "O" "ne"))
  ((2 3) "Two")
  ((2)   "Three")
  (nil   "Nil 1")
  ((nil) "Nil 2")
  (else  "Other")))

(confirm that (case-test-fun 1)                  returns "One")
(confirm that (case-test-fun (+ 1 1))            returns "Two")
(confirm that (case-test-fun 4)                  returns "Other")
(confirm that (case-test-fun 'a)                 returns "Other")
(confirm that (case-test-fun nil)                returns "Nil 1")
(confirm that (round-up-to-nearest-multiple 7 8) returns  8)
(confirm that (round-up-to-nearest-multiple 9 8) returns 16)

(if *use-soft-rationals*
 (progn
  (confirm that (add-rational     3                 (rational 2 3)) returns '(11 . 3))
  (confirm that (add-rational     212               (rational 3 4)) returns '(851 . 4))
  (confirm that (add-rational     217               (rational 3 4)) returns '(871 . 4))
  (confirm that (add-rational     (rational 21 2)   (rational 3 4)) returns '(45  . 4))
  (confirm that (add-rational     (rational 21 7)   (rational 3 4)) returns '(15 . 4))
  (confirm that (sub-rational     3                 (rational 2 3)) returns '(7 . 3))
  (confirm that (sub-rational     212               (rational 3 4)) returns '(845 . 4))
  (confirm that (sub-rational     217               (rational 3 4)) returns '(865 . 4))
  (confirm that (sub-rational     (rational 21 2)   (rational 3 4)) returns '(39  . 4))
  (confirm that (sub-rational     (rational 21 7)   (rational 3 4)) returns '(9 . 4))
  (confirm that (mul-rational     3                 (rational 2 3)) returns 2)
  (confirm that (mul-rational     212               (rational 3 4)) returns 159)
  (confirm that (mul-rational     217               (rational 3 4)) returns '(651 . 4))
  (confirm that (mul-rational     (rational 21 2)   (rational 3 4)) returns '(63  . 8))
  (confirm that (mul-rational     (rational 21 7)   (rational 3 4)) returns '(9   . 4))
  (confirm that (div-rational     3                 (rational 2 3)) returns '(9  . 2))
  (confirm that (div-rational     212               (rational 3 4)) returns '(848 . 3))
  (confirm that (div-rational     217               (rational 3 4)) returns '(868 . 3))
  (confirm that (div-rational     (rational 21 2)   (rational 3 4)) returns 14)
  (confirm that (div-rational     (rational 21 7)   (rational 3 4)) returns 4))
 (progn
  (confirm that (add-rational     3                 2/3)            returns 11/3)
  (confirm that (add-rational     212               3/4)            returns 851/4)
  (confirm that (add-rational     217               3/4)            returns 871/4)
  (confirm that (add-rational     21/2              3/4)            returns 45/4)
  (confirm that (add-rational     21/7              3/4)            returns 15/4)
  (confirm that (sub-rational     3                 2/3)            returns 7/3)
  (confirm that (sub-rational     212               3/4)            returns 845/4)
  (confirm that (sub-rational     217               3/4)            returns 865/4)
  (confirm that (sub-rational     21/2              3/4)            returns 39/4)
  (confirm that (sub-rational     21/7              3/4)            returns 9/4)
  (confirm that (mul-rational     3                 2/3)            returns 2)
  (confirm that (mul-rational     212               3/4)            returns 159)
  (confirm that (mul-rational     217               3/4)            returns 651/4)
  (confirm that (mul-rational     21/2              3/4)            returns 63/8)
  (confirm that (mul-rational     21/7              3/4)            returns 9/4)
  (confirm that (div-rational     3                 2/3)            returns 9/2)
  (confirm that (div-rational     212               3/4)            returns 848/3)
  (confirm that (div-rational     217               3/4)            returns 868/3)
  (confirm that (div-rational     21/2              3/4)            returns 14)
  (confirm that (div-rational     21/7              3/4)            returns 4)))
(confirm that (round (approx-sqrt 14))                              returns 4)
(confirm that (round (approx-sqrt 16))                              returns 4)
(confirm that (round (approx-sqrt 25))                              returns 5)
(confirm that (round (approx-sqrt 36))                              returns 6)
(confirm that (round (approx-sqrt 49))                              returns 7)
(confirm that (round (approx-sqrt 60))                              returns 8)
(confirm that (round (approx-sqrt 64))                              returns 8)
(confirm that (round (rational  7 8))                               returns 1)
(confirm that (round (rational 14 8))                               returns 2)
(confirm that (is-square? 35)                                       returns nil)
(confirm that (is-square? 36)                                       returns t)
(confirm that (round-up-to-square 35)                               returns 36)
(confirm that (round-up-to-square 36)                               returns 36)
(confirm that (copy-list '(1 2 3 4 5 6 7 8 9))                             returns '(1 2 3 4 5 6 7 8 9))
(confirm that (plist-to-alist '(a 1 b 2 c 3))                              returns '((a . 1) (b . 2) (c . 3)))
(confirm that (alist-to-plist '((a . 1) (b . 2) (c . 3)))                  returns '(a 1 b 2 c 3))
(confirm that (plist-to-alist (alist-to-plist '((a . 1) (b . 2) (c . 3)))) returns '((a . 1) (b . 2) (c . 3)))
(confirm that (concat* :this " sentence " 'contains " " 5 " words.")       returns ":this sentence contains 5 words.")
(confirm that (concat* :this " sentence " 'contains " a list: " '(1 2 3))  returns ":this sentence contains a list: (1 2 3)")
(setq l1 '(1 2 3 4 5 6 7 8 9 10))
(setq l2 '(a b c d e f g h i j))
(setq l3 '(q r s t u v w x y z))
(confirm that (heads (list l1 l2 l3)) returns '(1 a q))
(confirm that (tails (list l1 l2 l3)) returns '((2 3 4 5 6 7 8 9 10) (b c d e f g h i j) (r s t u v w x y z)))
(confirm that (copy-tree '(1 2 (3 (4 5)) (6 7) 8 9)) returns '(1 2 (3 (4 5)) (6 7) 8 9))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matrix tests:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq matrix (make-matrix 6 6 0))

(matrix-set! matrix 0 3 3)
(matrix-set! matrix 0 5 5)
(matrix-set! matrix 1 1 11)
(matrix-set! matrix 1 5 15)
(matrix-set! matrix 2 0 20)
(matrix-set! matrix 2 4 24)
(matrix-set! matrix 4 4 44)
(matrix-set! matrix 4 1 41)
(matrix-set! matrix 5 2 52)

(confirm that matrix returns
 '((0 0 0 3 0 5)
   (0 11 0 0 0 15)
   (20 0 0 0 24 0)
   (0 0 0 0 0 0)
   (0 41 0 0 44 0)
   (0 0 52 0 0 0)))

(confirm that
 (matrix-rotate-right
  '((1 2 3)
    (4 5 6)
    (7 8 9)))
 returns
 '((7 4 1)
   (8 5 2)
   (9 6 3)))

(setq matrix (make-matrix 6 6 0))
(setq ctr 5)

(until (zero? ctr)
 (matrix-set! matrix ctr ctr (* 100 ctr))
 (decr ctr))

(confirm that matrix returns
 '((0 0 0 0 0 0)
   (0 100 0 0 0 0)
   (0 0 200 0 0 0)
   (0 0 0 300 0 0)
   (0 0 0 0 400 0)
   (0 0 0 0 0 500)))

(confirm that (transform-matrix matrix (lambda (row col val) (+ val (* 10 row) col))) returns
 '((0 1 2 3 4 5)
   (10 111 12 13 14 15)
   (20 21 222 23 24 25)
   (30 31 32 333 34 35)
   (40 41 42 43 444 45)
   (50 51 52 53 54 555)))

(confirm that matrix returns
 '((0 0 0 0 0 0)
   (0 100 0 0 0 0)
   (0 0 200 0 0 0)
   (0 0 0 300 0 0)
   (0 0 0 0 400 0)
   (0 0 0 0 0 500)))

(confirm that (transform-matrix! matrix (lambda (row col val) (+ val (* 10 row) col))) returns
 '((0 1 2 3 4 5)
   (10 111 12 13 14 15)
   (20 21 222 23 24 25)
   (30 31 32 333 34 35)
   (40 41 42 43 444 45)
   (50 51 52 53 54 555)))

(confirm that matrix returns
 '((0 1 2 3 4 5)
   (10 111 12 13 14 15)
   (20 21 222 23 24 25)
   (30 31 32 333 34 35)
   (40 41 42 43 444 45)
   (50 51 52 53 54 555)))
;;(log-eval t)
(write-matrix matrix 3) (nl)

(setq l '(a 1 b 2 c 3 d 4 e 5))
(confirm that (removeql! 'c l) returns '(a 1 b 2 3 d 4 e 5))
(confirm that (removeql! 'e l) returns '(a 1 b 2 3 d 4 5))
(confirm that (removeql! 'a l) returns '(1 b 2 3 d 4 5))
(confirm that l                returns '(1 b 2 3 d 4 5))

(setq l '(a 1 b 2 c 3 d 4 e 5))
(confirm that (removeq!  'c l) returns '(a 1 b 2 3 d 4 e 5))
(confirm that (removeq!  'e l) returns '(a 1 b 2 3 d 4 5))
(confirm that (removeq!  'a l) returns '(1 b 2 3 d 4 5))
(confirm that l                returns '(1 b 2 3 d 4 5))

(confirm that (max-delta '(1 3 2 8 4 5)) returns 7)
(confirm that (max-delta '(3 2 8 4 1 5)) returns 7)
(confirm that (sum '(1 2 3 4))           returns 10)

(confirm that (compact '(nil nil 1 nil 2 3 nil nil 4 nil)) returns '(1 2 3 4))

(confirm that (all even? '(2 4 6 8))    returns t)
(confirm that (all even? '(1 2 4 6 8))  returns nil)
(confirm that (all even? '(2 4 6 1 8))  returns nil)
(confirm that (all even? '(2 4 6 8 1))  returns nil)
(confirm that (any odd?  '(2 4 6 8))    returns nil)
(confirm that (any odd?  '(2 4 6 8 1))  returns t)
(confirm that (any odd?  '(1 2 4 6 8))  returns t)
(confirm that (any odd?  '(2 4 1 6 8))  returns t)
(confirm that (mapcar+ + '(1 2 3) '(4 5 6) '(7 8 9)) returns '(12 15 18))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ignore
 "Make some syntax kind of like this work:"
 (confirm that (random 10) is (and (>= 0 val) (< 10 val))))

#|

Write some tests for structs.
Write some tests for random.
Write some tests for bitwise operators.

|#

(confirm that (<  1 2 3 4 5)  returns t)
(confirm that (<  1 2 3 0 5)  returns nil)
(confirm that (<  1 2 3 4 4)  returns nil)
(confirm that (<  1)          returns t)

(confirm that (<= 1 2 3 4 5)  returns t)
(confirm that (<= 1 2 3 0 5)  returns nil)
(confirm that (<= 1 2 3 4 4)  returns t)
(confirm that (<= 1)          returns t)

(confirm that (>  5 4 3 2 1)  returns t)
(confirm that (>  5 0 3 2 1)  returns nil)
(confirm that (>  4 4 3 2 1)  returns nil)
(confirm that (>  1)          returns t)

(confirm that (>= 5 4 3 2 1)  returns t)
(confirm that (>= 5 0 3 2 1)  returns nil)
(confirm that (>= 4 4 3 2 1)  returns t)
(confirm that (>= 1)          returns t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(princ "All tests passed.")
(provide 'tests)
(nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

