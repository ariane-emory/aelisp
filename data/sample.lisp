(setq! lst-base '(1 a 2 b 3 4 5 6 ((7 8 (unquote 9) 10 11 12 13))))
(setq! lst-base '(1 a 2 b 3 ((4 (unquote 5) 6))))
(setq! lst-base '(1 a 2))
(setq! lst-base '(1 a (unquote 2)))
(setq! lst lst-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro xform-old (obj pred? if-fun else-fun)
 (cond
  ((nil?  obj)  nil)
  ((pred? obj) $(if-fun   obj))
  ((atom? obj) $(else-fun obj))
  (t (cons
      $(xform-old (car obj) pred? if-fun else-fun)
      $(xform-old (cdr obj) pred? if-fun else-fun)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro quotify (x) $('quote x))

(defmacro xform-inner (obj)
 (when obj
   $(cons (car obj) $('xform-inner (cdr obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(log-eval t)
(log-macro t)

(nl) (princ "Before.")
(xform-inner (1 2 3 4))
(nl) (princ "After.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nl)
(princ "Done.")
(exit)
