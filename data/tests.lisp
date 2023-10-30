(setq! *random-seed* 1)

(defun random-int-range rest
 (let ((val  (setq! *random-seed* 1)))
  *random-seed*))

(princ (random-int-range)) (nl)
(princ (random-int-range 10)) (nl)
(princ (random-int-range -10 10)) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

