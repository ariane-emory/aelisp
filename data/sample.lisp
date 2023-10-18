;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro quotify (x) $('quote x))

(log-macro t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (nl) (princ "Before.")
;; `(1 2 ,(- 5 2) (4 5))
;; (nl) (princ "After.")
;; (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lists (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;; `(cons left-nested-zip ,lists) 
`(mapcar flatten (cons left-nested-zip ,lists))
`(mapcar flatten (cons left-nested-zip ,@lists))


(write (eval `(mapcar writen $,lists))) (nl)


(exit)

(nl)
(old-zip '(a b c) '(1 2 3))

(nl)
(zip '(a b c) '(1 2 3))

(nl)
(princ "Done.")









