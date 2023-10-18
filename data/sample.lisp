;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lists (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;; (log-macro t)
;; (log-macro nil)

;; (nl)
;; (write `,1)

;; (nl)
;; (eval `(mapcar princ ',lists))

;; (nl)
;; (write (zip '(a b c) '(1 2 3)))

;; ;; (nl)
;; ;; (old-zip '(a b c) '(1 2 3))

;; (nl)
;; (princ "Done.")

(setq! lst '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

;; (when memql? 5 lst
;;  (log-eval t)
;;  (log-eval nil)
;;  (print (removeql 5 lst)))

;; (log-core t)
;; (log-core nil)
;; (log-eval t)
;; (log-eval nil)

;; (princ "Found 5 in list at index ")
;; (write (indexql 5 lst))
;; (nl)
;; (princ "Found 88 in list at index ")
;; (write (indexql 88 lst))
;; (nl)
;; (princ "Found 5 in list at index ")
;; (write (indexq 5 lst))
;; (nl)
;; (princ "Found 88 in list at index ")
;; (write (indexq 88 lst))
;; (nl)

(if (memql? 5 lst)
 (princ "5 is in list.")
 (princ "5 is not in list."))
(nl)
(if (memql? 88 lst)
 (princ "88 is in list.")
 (princ "88 is not in list."))
(nl)
;; (if (memq? 5 lst)
;;  (princ "5 is in list.")
;;  (princ "5 is not in list."))
;; (nl)
;; (if (memq? 88 lst)
;;  (princ "88 is in list.")
;;  (princ "88 is not in list."))
;; (nl)

;; (princ "Removing 5 from list:  ") (write (removeq  5  lst)) (nl)
;; (princ "Removing 88 from list: ") (write (removeq  88 lst)) (nl)
;; (princ "Removing 5 from list:  ") (write (removeql 5  lst)) (nl)
;; (princ "Removing 88 from list: ") (write (removeql 88 lst)) (nl)

;; (log-eval t)
;; (memql? 5 lst)
;; (memql? 88 lst)
;; (log-eval nil)

(log-macro t)
(mame-mem? memql? eql?)
