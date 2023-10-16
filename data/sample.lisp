
(defun flatten (lst)
  (when (not? (nil? lst))
   (if (cons? (car lst))
    (append (flatten (car lst)) (flatten (cdr lst)))
    (cons (car lst) (flatten (cdr lst))))))


(setq! left-nested-zip (reduced (lambda (x y) (zip2 x y)) arg))

(defun zip lists
 "Zip a list of lists."
 (mapcar flatten (left-nested-zip lists)))


(princ "flat: ") (write (mapcar flatten (left-nested-zip '(1 2 3) '(a b c) '(10 20 30) '(x y z)))) (nl)
(princ "flat: ") (write (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)
