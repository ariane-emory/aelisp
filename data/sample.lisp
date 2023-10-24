;;(log-eval t)

(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq! lst '(1 2 3 4 5 6))
(setq! lst (unionql '(1 2 3) '(4 5 6)))
(princ "initial lst:     ") (write lst)                                (nl)
(princ "memql? 2:        ") (write (memql?   3 lst))                   (nl)
(princ "removeql 4:      ") (write (removeql 4 lst))                   (nl)
(princ "indexql 5:       ") (write (indexql  5 lst))                   (nl)
(princ "mapcar!:         ") (write (mapcar! double lst))               (nl)
(princ "butlast:         ") (write (butlast '(1 2 3 4 5)))             (nl)
(princ "reverse:         ") (write (reverse '(1 2 3 4 5)))             (nl)
(princ "reverse butlast: ") (write (reverse (butlast '(1 2 3 4 5))))   (nl)
(princ "union:           ") (write (unionql '(1 2 3) '(4 5 6)))        (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! lst (make-list 6 200))
(nl) (princ "make-list:       ") (write lst) (nl)

(list-set! lst 0 100)
(princ "list-set! 0 100: ") (write lst) (nl)
(princ "list-ref  0:     ") (write (list-ref lst 0)) (nl)

(list-set! lst 1 101)
(princ "list-set! 1 101: ") (write lst) (nl)
(princ "list-ref  1:     ") (write (list-ref lst 1)) (nl)

(list-set! lst 2 102)
(princ "list-set! 2 102: ") (write lst) (nl)
(princ "list-ref  2:     ") (write (list-ref lst 2)) (nl)

(list-set! lst 3 103)
(princ "list-set! 3 103: ") (write lst) (nl)
(princ "list-ref  3:     ") (write (list-ref lst 3)) (nl)

(list-set! lst 4 104)
(princ "list-set! 4 104: ") (write lst) (nl)
(princ "list-ref  4:     ") (write (list-ref lst 4)) (nl)

(list-set! lst 5 105)
(princ "list-set! 5 105: ") (write lst) (nl)
(princ "list-ref  5:     ") (write (list-ref lst 5)) (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lshift4 (n) (<< n 4))

(add-logging-to lshift4)
(princ 'lshift4 "'s body is now " (body lshift4)) (nl)
(princ "Call returned " (lshift4 4) ".") (nl) (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! lst $(2 4 1 5 3 7 9 6 8))

(princ (select-and-move-to-front! (lambda (o) (eql? o 9)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 8)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 7)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 6)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 5)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 4)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 3)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 2)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 1)) lst)) (spc) (write lst) (nl)

(select-and-move-to-front! (lambda (o) (eql? o 2)) (list 2))

(princ (nthcdr 0 lst)) (nl)
(princ (nthcdr 1 lst)) (nl)
(princ (nthcdr 2 lst)) (nl)
(setq! c (curry1 nthcdr 3))
(princ (c lst)) (nl)

(princ (pop! lst)) (nl)
(princ lst) (nl)
(princ (push! 99 lst)) (nl)
(princ lst) (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore
 ;; a deliberate invalid call:
 (nl) (princ "AN ERROR WILL FOLLOW: ") (nl)
 ;; (log-eval t)
 (s x 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This one triggers an indentation bug, investigate:
;; (defun split-list (pred? lst)
;;  (let ((front nil)
;;        (current lst))
;;   (while (and current (funcall pred? (car current)))
;;    (setq front (cons (car current) front))
;;    (setq current (cdr current)))
;;   $((fake-reverse front) current)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s lst  '("asdw" "erer" "rerw" 1 nil (lambda (x) x) zoop z (1 2 . 3) 8))
(s split (split-list string? lst))
(nl)
(princ "front") (write (apply concat (intercalate " " (car split))))  (nl)

(test "back" '(1 nil (lambda (x) x) zoop z (1 2 . 3) 8) (cadr split))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore
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
 ;;(log-eval t)

 (write (doc somefun)) (nl)
 (write (doc write)) (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun binlist-to-dec-1 (lst)
 (letrec
  ((chase
    (lambda (acc lst)
     (if (nil? lst)
      acc
      (chase (+ (<< acc 1) (car lst)) (cdr lst))))))
  (chase 0 lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun binlist-to-dec-2 (lst) (reduce   (lambda (x y) (+ (<< x 1) y)) lst 0))
(setq! binlist-to-dec-3       (reduced  (lambda (x y) (+ (<< x 1) y))))
(setq! bins-to-dec            (reduced* (lambda (x y) (+ (<< x 1) y))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(test "binlist-to-dec-1" 85 (binlist-to-dec-1 '(1 0 1 0 1 0 1))) ;; ⇒ 85
(test "binlist-to-dec-2" 85 (binlist-to-dec-2 '(1 0 1 0 1 0 1)));; ⇒ 85
(test "binlist-to-dec-3" 85 (binlist-to-dec-3 '(1 0 1 0 1 0 1)));; ⇒ 85
(test "bins-to-dec"      85 (bins-to-dec        1 0 1 0 1 0 1));; ⇒ 85
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(test "lr   #1" nil (reduce  + '()))
(test "lr   #2" 1   (reduce  + '(1)))
(test "lr   #3" 3   (reduce  + '(1 2)))
(test "lr   #4" 6   (reduce  + '(1 2 3)))
(test "lr   #5" 10  (reduce  + '(1 2 3 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(test "lr 0 #1" 0   (reduce  + '()         0))
(test "lr 0 #2" 1   (reduce  + '(1)        0))
(test "lr 0 #3" 3   (reduce  + '(1 2)      0))
(test "lr 0 #4" 6   (reduce  + '(1 2 3)    0))
(test "lr 0 #5" 10  (reduce  + '(1 2 3 4)  0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(test "rr   #1" nil (rreduce + '()))
(test "rr   #2" 1   (rreduce + '(1)))
(test "rr   #3" 3   (rreduce + '(1 2)))
(test "rr   #4" 6   (rreduce + '(1 2 3)))
(test "rr   #5" 10  (rreduce + '(1 2 3 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(test "rr 0 #1" 0   (rreduce + '()         0))
(test "rr 0 #2" 1   (rreduce + '(1)        0))
(test "rr 0 #3" 3   (rreduce + '(1 2)      0))
(test "rr 0 #4" 6   (rreduce + '(1 2 3)    0))
(test "rr 0 #5" 10  (rreduce + '(1 2 3 4)  0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(test "zipped 2" '((a 1) (b 2) (c 3))       (zip '(a b c) '(1 2 3)))
(test "zipped 3" '((a 1 x) (b 2 y) (c 3 z)) (zip '(a b c) '(1 2 3) '(x y z)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mylist '(7 5 4 6 3 9 1 6 2 8 6))
(defun less-than (a b) (< a b))
(test "sorted" '(1 2 3 4 5 6 6 6 7 8 9) (sort!! mylist less-than))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mylist '(7 5 4 6 3 9 1 6 2 8 6))
(test "sorted" '(1 2 3 4 5 6 6 6 7 8 9) (sort!! mylist <))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! sum (reduced* +))
(test "#1 l this*   == 10" 10 (sum  1 2 3 4))
(setq! sum (reduced* + 4))
(test "#2 l this* 4 == 10" 10 (sum  1 2 3))
(setq! sum (rreduced* +))
(test "#3 r this*   == 10" 10 (sum  1 2 3 4))
(setq! sum (rreduced* + 4))
(test "#4 r this* 4 == 10" 10 (sum  1 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! sum (reduced  +))
(test "#5 l this    == 10" 10 (sum '(1 2 3 4)))
(setq! sum (reduced  + 4))
(test "#6 l this  4 == 10" 10 (sum '(1 2 3)))
(setq! sum (rreduced  +))
(test "#7 r this    == 10" 10 (sum '(1 2 3 4)))
(setq! sum (rreduced  + 4))
(test "#8 r this  4 == 10" 10 (sum '(1 2 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl) (princ "Done.") (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
