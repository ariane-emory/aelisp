
;; (setq! left-nested-zip (reduced zip2))

;; (princ "flat 1: ")
;; (log-all t)
;; (write (mapcar flatten (left-nested-zip '(1 2 3) '(a b c) '(10 20 30) '(x y z)))) (nl)

(log-macro t)
;; (log-core  t)
;; (log-eval  t)

(princ " ") (write (zip '(1 2 3) '(a b c) '(7 8 9) '(x y z) '(p q r))) (nl)
(princ " ") (write (zip '(1 2 3) '(a b c) '(7 8 9) '(x y z))) (nl)
(princ " ") (write (zip '(1 2 3) '(a b c) '(7 8 9))) (nl)
(princ " ") (write (zip '(1 2 3) '(a b c))) (nl)
(princ " ") (write (zip '(1 2 3))) (nl)

(setq! x 66)
(write (list 22 44 x))
(write #(22 44 x))
