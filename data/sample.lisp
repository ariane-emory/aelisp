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

`(cons left-nested-zip ,lists) 

(exit)

;; (append2 (list (quote cons)) lists)

;; (exit)

(zip '(a b c) '(1 2 3))

(nl)
(princ "Done.")
(exit)shove





