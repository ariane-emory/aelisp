
(defun flatten (lst)
  (when (not? (nil? lst))
   (if (cons? (car lst))
    (append (flatten (car lst)) (flatten (cdr lst)))
    (cons (car lst) (flatten (cdr lst))))))


(setq! left-nested-zip (reduced zip2))

(defun zip lists
 "Zip a list of lists."
 (mapcar flatten (apply left-nested-zip lists)))

;; (log-all t)
(princ "flat 1: ")
(write (mapcar flatten (left-nested-zip '(1 2 3) '(a b c) '(10 20 30) '(x y z)))) (nl)

;; (log-all t)
(princ "flat 2: ")
(write (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)
