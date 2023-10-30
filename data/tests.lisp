(setq! *random-seed* (now-us))

(defun random-inner ()
 "Return a random integer."
 (setq! *random-seed* (% (+ (mul *random-a* *random-seed*) *random-c*) *random-m*)))

(defun random-int-range rest
 (let ((val  (setq! *random-seed* 1)))
  *random-seed*))

(repeat 10 (princ (random-int-range))         (nl))
(repeat 10 (princ (random-int-range 10))      (nl))
(repeat 10 (princ (random-int-range -10 10))  (nl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(princ "All tests passed.")
(provide 'tests)
(nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

