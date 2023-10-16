;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; construction zone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! test
 (lambda args
  (princ "car-ed head " (car args)) (nl)
  (princ "cdr-ed tail " (cdr args)) (nl)))

(test 1 2 3)

(setq! test
 (lambda (head . tail)
  tail))

(setq! test
 (lambda (head head2 . tail)
  (princ "head "  head) (nl)
  (princ "head2 " head2) (nl)
  (princ "tail "  tail) (nl)))

(test 1 2 3 4 5)

(princn "got here")
(princn ''(1 . 2))

(princn ''(1 2))

(setq! test
 (lambda args
  (cdr args)))

(princn '(test 1 2 3 4 5))

(setq! test
 (macro (foo bar)
  (list quote (list foo bar))))

(princ "test: ") (write (test 12 34)) (nl)

(princ "Reached the end.") (nl)






