(nl)

(setq!   lst  (unionql '(1 2 3) '(4 5 6)))

(confirm that lst                              returns (6 5 4 3 2 1))
(confirm that (memql?   3 lst)                 returns t)
(confirm that (removeql 4 lst)                 returns (6 5 3 2 1))
(confirm that (indexql  5 lst)                 returns 1)
(confirm that (mapcar! double lst)             returns (12 10 8 6 4 2))
(confirm that (butlast '(1 2 3 4 5))           returns (1 2 3 4))
(confirm that (reverse '(1 2 3 4 5))           returns (5 4 3 2 1))
(confirm that (reverse (butlast '(1 2 3 4 5))) returns (4 3 2 1))
(confirm that (unionql '(1 2 3) '(4 5 6))      returns (6 5 4 3 2 1))

(nl)

(setq!   lst  (make-list 6 200))

(confirm that lst                              returns (200 200 200 200 200 200))
(confirm that (list-set! lst 0 100)            returns 100)
(confirm that lst                              returns (100 200 200 200 200 200))
(confirm that (list-ref lst 0)                 returns 100)
(confirm that (list-set! lst 1 101)            returns 101)
(confirm that lst                              returns (100 101 200 200 200 200))
(confirm that (list-ref lst 1)                 returns 101)
(confirm that (list-set! lst 2 102)            returns 102)
(confirm that lst                              returns (100 101 102 200 200 200))
(confirm that (list-ref lst 2)                 returns 102)
(confirm that (list-set! lst 3 103)            returns 103)
(confirm that lst                              returns (100 101 102 103 200 200))
(confirm that (list-ref lst 3)                 returns 103)
(confirm that (list-set! lst 4 104)            returns 104)
(confirm that lst                              returns (100 101 102 103 104 200))
(confirm that (list-ref lst 4)                 returns 104)
(confirm that (list-set! lst 5 105)            returns 105)
(confirm that lst                              returns (100 101 102 103 104 105))
(confirm that (list-ref lst 5)                 returns 105)

(nl) 

(defun lshift4 (n) (<< n 4))
(add-logging-to lshift4)

(confirm that 
 (body lshift4)
 returns (progn
  (princ "Applying " 'lshift4 " to parameters " (syms (env)) " with arguments " (vals (env)) ".") 
  (nl)
  (let ((result (progn (<< n 4))))
   (princ "Result of applying " 'lshift4 " was " result ".") (nl)
   result)))

(confirm that (lshift4 4) returns 64)

(nl)

(setq! lst $(2 4 1 5 3 7 9 6 8))

(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 9)) lst) lst) returns (9 2 4 1 5 3 7 6 8))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 8)) lst) lst) returns (8 9 2 4 1 5 3 7 6))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 7)) lst) lst) returns (7 8 9 2 4 1 5 3 6))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 6)) lst) lst) returns (6 7 8 9 2 4 1 5 3))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 5)) lst) lst) returns (5 6 7 8 9 2 4 1 3))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 4)) lst) lst) returns (4 5 6 7 8 9 2 1 3))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 3)) lst) lst) returns (3 4 5 6 7 8 9 2 1))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 2)) lst) lst) returns (2 3 4 5 6 7 8 9 1))
(confirm that (progn (select-and-move-to-front! (lambda (o) (eql? o 1)) lst) lst) returns (1 2 3 4 5 6 7 8 9))

(nl)

(confirm that (nthcdr 0 lst) returns (1 2 3 4 5 6 7 8 9))
(confirm that (nthcdr 1 lst) returns (2 3 4 5 6 7 8 9))
(confirm that (nthcdr 2 lst) returns (3 4 5 6 7 8 9))

(nl)

(setq! c (curry1 nthcdr 3))
(confirm that (c lst) returns (4 5 6 7 8 9))

(nl)

(confirm that (pop! lst)     returns 1)
(confirm that (push! 99 lst) returns (99 2 3 4 5 6 7 8 9))

(ignore
 "This would result (deliberately) in an invalid call, so we ignore it for now."
 "It remains here only to serve as an example."
 ;; a deliberate invalid call:
 (nl) (princ "AN ERROR WILL FOLLOW: ") (nl)
 ;; (log-eval t)
 (s x 2 3))

(ignore
 "This one triggers an indentation bug, investigate:"
 (defun split-list (pred? lst)
  (let ((front nil)
        (current lst))
   (while (and current (funcall pred? (car current)))
    (setq front (cons (car current) front))
    (setq current (cdr current)))
   $((fake-reverse front) current))))

(nl)

(setq! lst '("asdw" "erer" "rerw" 1 nil (lambda (x) x) zoop z (1 2 . 3) 8))
(setq! split (split-list string?              lst))

(ignore
 "This test cannot work until handling of strings containing escaped double quotes is fixed,"
 "and so for now we will ignore it."
 (confirm that (apply concat (intercalate " " (car split))) returns "\"asdw erer rerw\""))

(confirm that (cadr split) returns (1 nil (lambda (x) x) zoop z (1 2 . 3) 8))

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

(defun binlist-to-dec-1 (lst)
 (letrec
  ((chase
    (lambda (acc lst)
     (if (nil? lst)
      acc
      (chase (+ (<< acc 1) (car lst)) (cdr lst))))))
  (chase 0 lst)))

(defun binlist-to-dec-2 (lst) (reduce   (lambda (x y) (+ (<< x 1) y)) lst 0))
(setq! binlist-to-dec-3       (reduced  (lambda (x y) (+ (<< x 1) y))))
(setq! bins-to-dec            (reduced* (lambda (x y) (+ (<< x 1) y))))

(confirm that (binlist-to-dec-1 '(1 0 1 0 1 0 1)) returns 85)
(confirm that (binlist-to-dec-2 '(1 0 1 0 1 0 1)) returns 85)
(confirm that (binlist-to-dec-3 '(1 0 1 0 1 0 1)) returns 85)
(confirm that (bins-to-dec        1 0 1 0 1 0 1)  returns 85)

(nl)

(confirm that (reduce  + '()          )        returns nil)
(confirm that (reduce  + '(1)         )        returns 1)
(confirm that (reduce  + '(1 2)       )        returns 3)
(confirm that (reduce  + '(1 2 3)     )        returns 6)
(confirm that (reduce  + '(1 2 3 4)   )        returns 10)

(nl)

(confirm that (reduce  + '()         0)        returns 0)
(confirm that (reduce  + '(1)        0)        returns 1)
(confirm that (reduce  + '(1 2)      0)        returns 3)
(confirm that (reduce  + '(1 2 3)    0)        returns 6)
(confirm that (reduce  + '(1 2 3 4)  0)        returns 10)

(nl)

(confirm that (rreduce + '()          )        returns nil)
(confirm that (rreduce + '(1)         )        returns 1)
(confirm that (rreduce + '(1 2)       )        returns 3)
(confirm that (rreduce + '(1 2 3)     )        returns 6)
(confirm that (rreduce + '(1 2 3 4)   )        returns 10)

(nl)

(confirm that (rreduce + '()         0)        returns 0)
(confirm that (rreduce + '(1)        0)        returns 1)
(confirm that (rreduce + '(1 2)      0)        returns 3)
(confirm that (rreduce + '(1 2 3)    0)        returns 6)
(confirm that (rreduce + '(1 2 3 4)  0)        returns 10)

(nl)

(confirm that (zip '(a b c) '(1 2 3))          returns ((a 1) (b 2) (c 3)))
(confirm that (zip '(a b c) '(1 2 3) '(x y z)) returns ((a 1 x) (b 2 y) (c 3 z)))

(nl)

(setq lst '(7 5 4 6 3 9 1 6 2 8 6))
(confirm that (sort!! lst <) returns (1 2 3 4 5 6 6 6 7 8 9))

(nl)

(confirm that ((reduced   +  )  '(1 2 3 4))    returns 10)
(confirm that ((reduced   + 4)  '(1 2 3  ))    returns 10)
(confirm that ((rreduced  +  )  '(1 2 3 4))    returns 10)
(confirm that ((rreduced  + 4)  '(1 2 3)  )    returns 10)
(confirm that ((reduced*  +  )    1 2 3 4 )    returns 10)
(confirm that ((reduced*  + 4)    1 2 3   )    returns 10)
(confirm that ((rreduced* +  )    1 2 3 4 )    returns 10)
(confirm that ((rreduced* + 4)    1 2 3   )    returns 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(princ "All tests passed.")
(nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
