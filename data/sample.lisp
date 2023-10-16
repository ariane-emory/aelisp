
(setq! left-nested-zip (reduced zip2))

(princ "flat 1: ")
;; (log-all t)
(write (mapcar flatten (left-nested-zip '(1 2 3) '(a b c) '(10 20 30) '(x y z)))) (nl)

;; (defun zip lists
;;  "Zip a list of lists."
;;  (mapcar flatten (apply left-nested-zip lists)))

;; (princ "flat 2: ")
;; ;; (log-all t)
;; (write (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)


(defmacro zip lists
 "Zip a list of lists."
 (if (not? (cdr lists))
  (car lists)
  (list 'mapcar 'flatten (cons 'left-nested-zip lists))))

;; (log-macro t)
;; (log-core  t)
;; (log-eval  t)

(write (zip '(1 2 3) '(a b c) '(7 8 9) '(x y z) '(p q r))) (nl)
(write (zip '(1 2 3) '(a b c) '(7 8 9) '(x y z))) (nl)
(write (zip '(1 2 3) '(a b c) '(7 8 9))) (nl)
(write (zip '(1 2 3) '(a b c))) (nl)
(write (zip '(1 2 3))) (nl)
