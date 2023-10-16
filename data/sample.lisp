
(defun 2* (x) (* 2 x)) (nl)

(princ "this should print 6: " ) (write (2* 3)) (nl) ;; successfuly prints 6.
(princ "this should print 6: " ) (write (2* 3)) (nl) ;; does not re-expand.

(write (append '(1 2) '(3 4) '(5 6))) (nl)

;; (log-all t)
(write (nconc! '(1 2) '(3 4) '(5 6))) (nl)


(defun nconc! lists
 (reduce nconc2! (car lists) (cdr lists)))

(write (nconc! '(1 2) '(3 4) '(5 6))) (nl)

(setq! nconc! (reduced nconc2!))

(write (nconc! '(1 2) '(3 4) '(5 6))) (nl)


(defun add (x y) (+ x y))

(setq! sum (reduced add))

(write (sum 1 2 3 4 5 6)) (nl)
