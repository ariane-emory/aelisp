(setq! lst-base '(1 a 2 b 3 4 5 6 ((7 8 (unquote 9) 10 11 12 13))))
(setq! lst-base '(1 a 2 b 3 ((4 (unquote 5) 6))))
(setq! lst lst-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xform (obj pred? if-fun else-fun)
 (cond
  ((nil?  obj)  nil)
  ((pred? obj) (if-fun   obj))
  ((atom? obj) (else-fun obj))
  (t (cons
      (xform (car obj) pred? if-fun else-fun)
      (xform (cdr obj) pred? if-fun else-fun)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (write (xform lst integer? 2* id))
;; (nl)

(defmacro quotify (x) $('quote x))

;;(log-eval t)

(log-macro t)

(nl) (princ "Before.")
(nl) (quotify yyy)
(nl) (princ "After.")

(nl) (princ "Before.")
(nl) (xform lst integer? 2* quotify)
(nl) (princ "After.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nl)
(princ "Done.")
(exit)
