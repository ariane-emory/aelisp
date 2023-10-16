
(defun 2* (x) (* 2 x)) (nl)

(princ "this should print 6: " ) (write (2* 3)) (nl) ;; successfuly prints 6.
(princ "this should print 6: " ) (write (2* 3)) (nl) ;; does not re-expand.

(defun append-many lists
 (reduce append lists))

(log-all t)
(write (append-many '(1 2) '(3 4) '(5 6))) (nl)
