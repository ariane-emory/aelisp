(setq! lst-base '(1 a 2 b 3 4 5 6 ((7 8 (unquote 9) 10 11 12 13))))
(setq! lst-base '(1 a 2 b 3 ((4 (unquote 5) 6))))
(setq! lst-base '(1 a 2))
(setq! lst-base '(1 a (unquote 2)))
(setq! lst lst-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro quotify (x) $('quote x))

(defmacro xform (obj)
 (cond
  ((and (cons? obj) (cdr obj)) (list 'cons (list 'quote (car obj)) (list 'xform (cdr obj))))
  ((cons? obj)                 (list 'cons (list 'quote (car obj)) nil))
  (t                           obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(log-macro t)

(nl) (princ "Before.")
(xform (list 1 2 (list 3 4)))
(nl) (princ "After.")
(nl)

(nl) (princ "Before.")
(xform 1)
(nl) (princ "After.")
(nl)

(nl) (princ "Before.")
(xform nil)
(nl) (princ "After.")
(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nl)
(princ "Done.")
(exit)shove
