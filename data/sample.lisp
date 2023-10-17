(setq! lst-base '(1 2 3 4 5 6 ((7 8 (unquote 9) 10 11 12 13))))
(setq! lst lst-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xform (obj pred? fun)
  (if (pred? obj)
   (fun obj)
   (if (atom? obj)
    obj
    (cons
     (xform (car obj) pred? fun)
     (xform (cdr obj) pred? fun)))))


(write (xform lst integer? 2*))
(nl)

(princ "Done.")
(nl)
