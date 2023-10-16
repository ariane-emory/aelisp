
(defun 2* (x) (* 2 x)) (nl)

(princ "this should print 6: " ) (write (2* 3)) (nl) ;; successfuly prints 6.
(princ "this should print 6: " ) (write (2* 3)) (nl) ;; does not re-expand.





;; What you just gave me seems to recurse forever without returning.


 

(defun flatten (lst)
  (when (not? (nil? lst))
   (if (cons? (car lst))
    (append (flatten (car lst)) (flatten (cdr lst)))
    (cons (car lst) (flatten (cdr lst))))))


(setq! left-nested-zip (reduced (lambda (x y) (zip2 x y)) arg))

;; (defun zip lsts
;;  "Zip a list of lists."
;;  (mapcar flatten (left-nested-zip lsts)))

(princ "flat: ") (write (mapcar flatten (left-nested-zip '(1 2 3) '(a b c) '(10 20 30) '(x y z)))) (nl)

