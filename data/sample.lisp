
(defun 2* (x) (* 2 x)) (nl)

(princ "this should print 6: " ) (write (2* 3)) (nl) ;; successfuly prints 6.
(princ "this should print 6: " ) (write (2* 3)) (nl) ;; does not re-expand.

(write (append '(1 2) '(3 4) '(5 6))) (nl)

;; (log-all t)
(write (nconc! '(1 2) '(3 4) '(5 6))) (nl)


;; (defun nconc! lists
;;  (reduce nconc2! (car lists) (cdr lists)))

(write (nconc! '(1 2) '(3 4) '(5 6))) (nl)

(setq! nconc! (reduced nconc2!))
(write (nconc! '(1 2) '(3 4) '(5 6))) (nl)


(defun add (x y) (+ x y))

(setq! sum (reduced add))

(write (sum 1 2 3 4 5 6)) (nl)

(princni ", " 1 2 3)

(princ "zip2: ") (write (zip2 '(1 2 3) '(a b c))) (nl)

(princ "zip3: ") (write (zip3 '(1 2 3) '(a b c) '(10 20 30))) (nl)

(setq! zip (reduced (lambda (x y) (zip2 x y)) arg))
(princ "zip: ") (write (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)

(defun flatten-fully (lst)
  (cond
    ((atom? lst) (list lst))
    (t (append (flatten-fully (car lst)) (flatten-fully (cdr lst))))))
(defun zip-many lists
  (if (nil? (car lists))
      nil
      (append
       (list (flatten-fully (mapcar car lists)))
       (zip-many (mapcar cdr lists)))))

(princ "zip-many: ") (write (zip-many '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)

 
;; (setq! zip (reduced (lambda (x y) (flatten-left (zip2 x y))) arg))

;; (write (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)

;; For some reason, the result printed is:
;; (1 x (10 y))
;; instead of:
;; ((1 a 10 x) (2 b 20 y) (3 c 30 z))
