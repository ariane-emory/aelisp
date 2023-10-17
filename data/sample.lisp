(setq! lst-base '(1 2 3 4 5 6 ((7 8 (unquote 9) 10 11 12 13))))
(setq! lst lst-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xform (obj pred? fun-if fun-else)
 (cond
  ((pred? obj) (fun-if obj))
  ((atom? obj) obj)
  (t (cons
      (xform (car obj) pred? fun-if fun-else)
      (xform (cdr obj) pred? fun-if fun-else)))))

(write (xform lst integer? 2* id))
(nl)

(princ "Done.")
(nl)
