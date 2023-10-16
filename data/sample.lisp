
(defun 2* (x) (* 2 x)) (nl)

(princ "this should print 6: " ) (write (2* 3)) (nl) ;; successfuly prints 6.
(princ "this should print 6: " ) (write (2* 3)) (nl) ;; does not re-expand.
