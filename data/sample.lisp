;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro quotify (x) $('quote x))

(log-macro t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (nl) (princ "Before.")
;; `(1 2 ,(- 5 2) (4 5))
;; (nl) (princ "After.")
;; (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

`left-nested-zip
(nl)
`(cons left-nested-zip ,lists) 

(exit)

;; (append2 (list (quote cons)) lists)

;; (exit)

(zip '(a b c) '(1 2 3))

(nl)
(princ "Done.")
(exit)shove





