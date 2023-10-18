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

(defun indexql (x lst)
 (cond
  ((nil? lst) nil)
  ((eql? x (car lst)) 0)
  (t (let ((tail-result (indexql x (cdr lst))))
      (when tail-result
       (+ 1 tail-result))))))

(log-core t)
(log-core nil)
(log-eval nil)
(log-eval t)

;; (when memql? 5 lst
;;  (log-eval t)
;;  (log-eval nil)
;;  (print (removeql 5 lst)))

(princ "Found 5 in list at index")  (indexql 5 lst)  (nl)
