
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
 (list 'mapcar 'flatten (cons 'left-nested-zip lists)))
;; (mapcar flatten (apply left-nested-zip lists)))

(princ "flat m: ")
;; (log-all t)
(log-macro t)

(zip '(1 2 3) '(a b c))
(zip '(1 2 3) '(a b c) '(7 8 9))
(zip '(1 2 3) '(a b c) '(7 8 9) '(x y z))
(zip '(1 2 3) '(a b c) '(7 8 9) '(x y z) '(p q r))
