(nl)

(confirm that (feature? 'std) returns t)

(nl)

(setq!   lst  (unionql '(1 2 3) '(4 5 6)))

(confirm that lst                              returns '(6 5 4 3 2 1))
(confirm that (memql?   3 lst)                 returns t)
(confirm that (removeql 4 lst)                 returns '(6 5 3 2 1))
(confirm that (indexql  5 lst)                 returns 1)
(confirm that (mapcar! double lst)             returns '(12 10 8 6 4 2))
(confirm that (butlast '(1 2 3 4 5))           returns '(1 2 3 4))
(confirm that (reverse '(1 2 3 4 5))           returns '(5 4 3 2 1))
(confirm that (reverse (butlast '(1 2 3 4 5))) returns '(4 3 2 1))
(confirm that (unionql '(1 2 3) '(4 5 6))      returns '(6 5 4 3 2 1))

(nl)

(setq!   lst (make-list 6 200))

(confirm that lst                   returns '(200 200 200 200 200 200))
(confirm that (list-set! lst 0 100) returns 100)
(confirm that (list-ref  lst 0)     returns 100)

(confirm that lst                   returns '(100 200 200 200 200 200))
(confirm that (list-set! lst 1 101) returns 101)
(confirm that (list-ref lst 1)      returns 101)

(confirm that lst                   returns '(100 101 200 200 200 200))
(confirm that (list-set! lst 2 102) returns 102)
(confirm that (list-ref lst 2)      returns 102)

(confirm that lst                   returns '(100 101 102 200 200 200))
(confirm that (list-set! lst 3 103) returns 103)
(confirm that (list-ref lst 3)      returns 103)

(confirm that lst                   returns '(100 101 102 103 200 200))
(confirm that (list-set! lst 4 104) returns 104)
(confirm that (list-ref lst 4)      returns 104)

(confirm that lst                   returns '(100 101 102 103 104 200))
(confirm that (list-set! lst 5 105) returns 105)
(confirm that (list-ref lst 5)      returns 105)

(confirm that lst                   returns '(100 101 102 103 104 105))

(defun lshift4 (n) (<< n 4))
(add-logging-to lshift4)

(confirm that 
 (body lshift4)
 returns '(progn
           (princ "Applying " 'lshift4 " to parameters " (syms (env)) " with arguments " (vals (env)) ".") 
           (nl)
           (let ((result (progn (<< n 4))))
            (princ "Result of applying " 'lshift4 " was " result ".") (nl)
            result)))

(confirm that (lshift4 4) returns 64)

(nl)

(setq! lst $(2 4 1 5 3 7 9 6 8))

(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 9)) lst) lst) returns '(9 2 4 1 5 3 7 6 8))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 8)) lst) lst) returns '(8 9 2 4 1 5 3 7 6))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 7)) lst) lst) returns '(7 8 9 2 4 1 5 3 6))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 6)) lst) lst) returns '(6 7 8 9 2 4 1 5 3))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 5)) lst) lst) returns '(5 6 7 8 9 2 4 1 3))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 4)) lst) lst) returns '(4 5 6 7 8 9 2 1 3))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 3)) lst) lst) returns '(3 4 5 6 7 8 9 2 1))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 2)) lst) lst) returns '(2 3 4 5 6 7 8 9 1))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 1)) lst) lst) returns '(1 2 3 4 5 6 7 8 9))

(nl)

(confirm that (nthcdr 0 lst) returns '(1 2 3 4 5 6 7 8 9))
(confirm that (nthcdr 1 lst) returns '(2 3 4 5 6 7 8 9))
(confirm that (nthcdr 2 lst) returns '(3 4 5 6 7 8 9))

(nl)

(setq! c (curry1 nthcdr 3))
(confirm that (c lst) returns '(4 5 6 7 8 9))

(nl)

(confirm that (pop! lst)     returns 1)
(confirm that lst            returns '(2 3 4 5 6 7 8 9))
(confirm that (push! 99 lst) returns '(99 2 3 4 5 6 7 8 9))

(ignore
 "This would result (deliberately) in an invalid call, so we ignore it for now."
 "It remains here only to serve as an example."
 ;; a deliberate invalid call:
 (nl) (princ "AN ERROR WILL FOLLOW: ") (nl)
 ;; (log-eval t)
 (s x 2 3))

(nl)

(setq! lst '("asdw" "erer" "rerw" 1 nil (lambda (x) x) zoop z (1 2 . 3) 8))
(setq! split (split-list string? lst))

(ignore
 "This test cannot work until handling of strings containing escaped double quotes is fixed,"
 "and so for now we will ignore it."
 (confirm that (apply concat (intercalate " " (car split))) returns "\"asdw erer rerw\""))

(confirm that (cadr split) returns '(1 nil (lambda (x) x) zoop z (1 2 . 3) 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore
 "No tests written for this one yet."
 (defmacro defun (name params . docs-and-body)
  (let* ((split (split-list string? docs-and-body))
         (docs  (apply concat (intercalate " " (car split))))
         (body  (cadr split)))
   $('progn 
     $('setq! name $('lambda params . body))
     $('put! docs ':doc name))))
 (defun somefun (x y)
  "Multiply two"
  "numbers."
  (* x y))
 (write (get :doc somefun)) (nl)
 (write (doc somefun))      (nl)
 (write (doc write))        (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nl)

(confirm that (bin-list-to-int  '(1 0 1 0 1 0 1)) returns 85)
(confirm that (bin-list-to-int*   1 0 1 0 1 0 1)  returns 85)

(nl)

(confirm that (reduce  + '()          ) returns nil)
(confirm that (reduce  + '(1)         ) returns 1)
(confirm that (reduce  + '(1 2)       ) returns 3)
(confirm that (reduce  + '(1 2 3)     ) returns 6)
(confirm that (reduce  + '(1 2 3 4)   ) returns 10)

(nl)

(confirm that (reduce  + '()         0) returns 0)
(confirm that (reduce  + '(1)        0) returns 1)
(confirm that (reduce  + '(1 2)      0) returns 3)
(confirm that (reduce  + '(1 2 3)    0) returns 6)
(confirm that (reduce  + '(1 2 3 4)  0) returns 10)

(nl)

(confirm that (rreduce + '()          ) returns nil)
(confirm that (rreduce + '(1)         ) returns 1)
(confirm that (rreduce + '(1 2)       ) returns 3)
(confirm that (rreduce + '(1 2 3)     ) returns 6)
(confirm that (rreduce + '(1 2 3 4)   ) returns 10)

(nl)

(confirm that (rreduce + '()         0) returns 0)
(confirm that (rreduce + '(1)        0) returns 1)
(confirm that (rreduce + '(1 2)      0) returns 3)
(confirm that (rreduce + '(1 2 3)    0) returns 6)
(confirm that (rreduce + '(1 2 3 4)  0) returns 10)

(nl)

(confirm that (zip '(a b c) '(1 2 3))          returns '((a 1) (b 2) (c 3)))
(confirm that (zip '(a b c) '(1 2 3) '(x y z)) returns '((a 1 x) (b 2 y) (c 3 z)))

(nl)

(setq!   lst                         '(7 5 4 6 3 9 1 6 2 8 6))
(confirm that (sort!! lst <) returns '(1 2 3 4 5 6 6 6 7 8 9))

(nl)

(confirm that ((reduced   +  ) $(1 2 3 4)) returns 10)
(confirm that ((reduced   + 4) $(1 2 3  )) returns 10)
(confirm that ((rreduced  +  ) $(1 2 3 4)) returns 10)
(confirm that ((rreduced  + 4) $(1 2 3  )) returns 10)
(confirm that ((reduced   +  ) '(1 2 3 4)) returns 10)
(confirm that ((reduced   + 4) '(1 2 3  )) returns 10)
(confirm that ((rreduced  +  ) '(1 2 3 4)) returns 10)
(confirm that ((rreduced  + 4) '(1 2 3  )) returns 10)
(confirm that ((reduced*  +  )   1 2 3 4 ) returns 10)
(confirm that ((reduced*  + 4)   1 2 3   ) returns 10)
(confirm that ((rreduced* +  )   1 2 3 4 ) returns 10)
(confirm that ((rreduced* + 4)   1 2 3   ) returns 10)

(nl)

(setq lst '(1 nil 2 3 nil 4 nil))
(confirm that (delq! nil lst) returns '(1 2 3 4))

(setq lst '(1 2 3 2 4 2 5))
(confirm that (delq! 2 lst)   returns '(1 2 3 2 4 2 5))

(setq lst '(1 2 3 2 4 2 5))
(confirm that (delql! 2 lst)  returns '(1 3 4 5))

(setq lst '(1 2 3 2 4 2 5))
(confirm that (delql! 1 lst)  returns '(2 3 2 4 2 5))

(nl)

(setq lst '((1 2) (3 4) (5 6)))
(defun filter-evens (lst)
 (delq! nil (mapcar (lambda (x) (when (even? x) x)) lst)))
(confirm that (mapcan filter-evens lst) returns '(2 4 6))

(nl)

(confirm that (apply concat (intercalate " " '(These  are words)))  returns "These are words")
(confirm that (apply concat (intercalate " " '(These "are" words))) returns "These are words")
(confirm that (mapconcat string '(These  are  words) " ")           returns "These are words")
(confirm that (mapconcat string '(These "are" words) " ")           returns "These are words")

(nl)

(confirm that (append      '(1 2) '(3 4)    '(5 6))   returns '(1 2 3 4 5 6))
(confirm that (append      $(1 2) '(3 4)    $(5 6))   returns '(1 2 3 4 5 6))
(confirm that (mapcan id  '((1 2)  (3 4)     (5 6)))  returns '(1 2 3 4 5 6))
(confirm that (mapcan id  '((1 2)  (3 4) nil (5 6)))  returns '(1 2 3 4 5 6))
(confirm that (mapcan cdr '((1 2 3) (4 5 6) (7 8 9))) returns '(2 3 5 6 8 9))

(nl)

(confirm that (prime? 7) returns t)
(confirm that (prime? 8) returns nil)
(confirm that (primes 8) returns '(2 3 5 7 11 13 17 19))

(nl)

(setq! lst '(1 2 3 4))
(confirm that (mapcar! double lst) returns '(2 4 6 8))
(confirm that lst                  returns '(2 4 6 8))

(setq!   lst  '(a 2 3 (b 4 x (y 5)) (6 nil 7)))
(confirm that (transform  integer? double lst) returns '(a 4 6 (b 8 x (y 10)) (12 nil 14)))
(confirm that lst                              returns '(a 2 3 (b 4 x (y  5)) (6  nil  7)))
(confirm that (transform! integer? double lst) returns '(a 4 6 (b 8 x (y 10)) (12 nil 14)))
(confirm that lst                              returns '(a 4 6 (b 8 x (y 10)) (12 nil 14)))

(nl)

(confirm that (sort!! '(1 2 3 4 5 6) (lambda (x y) (and (even? x) (not (even? y))))) returns '(6 4 2 5 3 1))
(setq!   lst  (sort!! '(1 2 3 4 5 6) (lambda (x y) (and (even? x) (not (even? y))))))
(confirm that (filter even? lst) returns '(6 4 2))

(nl)

(confirm that (funcall + 1 2 3 4 5 6) returns 21)

(nl)

(confirm that (make-string      10 "x")         returns "xxxxxxxxxx")
(confirm that (pad-string-right 10 "x" "hello") returns "helloxxxxx")
(confirm that (pad-string-left  10 "x" "hello") returns "xxxxxhello")

(nl)

(confirm that (plist-keys   '(a 1 b 2 c 3)) returns '(a b c))
(confirm that (plist-values '(a 1 b 2 c 3)) returns '(1 2 3))

(nl)

(confirm that (apply concat (mapcar* (curry1 ljust 20) "hello" "to" "the" "world"))
 returns "hello               to                  the                 world               ")

(confirm that (mapconcat id '("hello" "to" "the" "world") " ") returns "hello to the world")
(confirm that (mapconcat id '("hello" "to" "the" "world"))     returns "hellototheworld")

(nl)

(confirm that (prog1 1 2 3) returns 1)
(confirm that (prog2 1 2 3) returns 2)

(nl)

(confirm that (put 'a :foo :quux)                   returns 'a)
(confirm that (put 'b :bar :quux)                   returns 'b)
(confirm that (put 'c :baz :quux)                   returns 'c)
(confirm that (props :quux)                         returns '(:baz c :bar b :foo a))
(confirm that (plist-remove eq? :foo (props :quux)) returns '(:baz c :bar b))
(confirm that (plist-remove eq? :bar (props :quux)) returns '(:baz c :foo a))
(confirm that (plist-remove eq? :baz (props :quux)) returns '(:bar b :foo a))
(confirm that (plist-removeq    :foo (props :quux)) returns '(:baz c :bar b))
(confirm that (plist-removeq    :bar (props :quux)) returns '(:baz c :foo a))
(confirm that (plist-removeq    :baz (props :quux)) returns '(:bar b :foo a))
(confirm that (plist-removeql   :foo (props :quux)) returns '(:baz c :bar b))
(confirm that (plist-removeql   :bar (props :quux)) returns '(:baz c :foo a))
(confirm that (plist-removeql   :baz (props :quux)) returns '(:bar b :foo a))
(confirm that (props!  :quux  '(:corge d))          returns '(:corge d))
(confirm that (put 'a  :foo     :quux)              returns 'a)
(confirm that (put 'b  :bar     :quux)              returns 'b)
(confirm that (put 'c  :baz     :quux)              returns 'c)
(confirm that (remove! :foo     :quux)              returns 'a)
(confirm that (remove! :bar     :quux)              returns 'b)
(confirm that (remove! :baz     :quux)              returns 'c)
(confirm that (remove! :corge   :quux)              returns 'd)
(confirm that (props   :quux)                       returns nil)

(nl)

(confirm that (nconc! '(1 2 3) '(4 5 6) '(7 8 9)) returns '(1 2 3 4 5 6 7 8 9))
(confirm that (append '(1 2 3) '(4 5 6) '(7 8 9)) returns '(1 2 3 4 5 6 7 8 9))

(nl)

(confirm that (memql?   3 '(1 2 3 4 5 6)) returns t)
(confirm that (memql?   9 '(1 2 3 4 5 6)) returns nil)
(confirm that (indexql  3 '(1 2 3 4 5 6)) returns 2)
(confirm that (indexql  9 '(1 2 3 4 5 6)) returns nil)
(confirm that (removeql 3 '(1 2 3 4 5 6)) returns '(1 2 4 5 6))
(confirm that (removeql 9 '(1 2 3 4 5 6)) returns '(1 2 3 4 5 6)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(princ "All tests passed.")
(nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



