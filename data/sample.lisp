
(defun flatten (lst)
  (when (not? (nil? lst))
   (if (cons? (car lst))
    (append (flatten (car lst)) (flatten (cdr lst)))
    (cons (car lst) (flatten (cdr lst))))))

(setq! left-nested-zip (reduced zip2))

(defun zip lists
 "Zip a list of lists."
 (mapcar flatten (apply left-nested-zip lists)))

(defun zipm lists
 "Zip a list of lists."
 (mapcar flatten (apply left-nested-zip lists)))

(princ "flat 1: ")
;; (log-all t)
(write (mapcar flatten (left-nested-zip '(1 2 3) '(a b c) '(10 20 30) '(x y z)))) (nl)

(princ "flat m: ")
;; (log-all t)
(write (zipm '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)

(princ "flat 2: ")
;; (log-all t)
(write (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)
