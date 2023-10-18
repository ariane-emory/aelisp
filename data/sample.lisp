;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lst '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defmacro defun-mem-fun (name pred?)
 `(defun ,name (x lst)
   (cond
	  ((,pred? (car lst) x) t)
	  (lst (,name x (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; (defmacro defun-mem-fun (name pred?)
;;  $('defun name $('x 'lst)
;;    $('cond
;; 	   $($(pred? $('car lst) 'x) t)
;; 	   $(lst $(name 'x $('cdr 'lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

(log-macro t)
;;(log-eval  t)
(defun-mem-fun memql? eql?)
(log-eval  nil)
(log-macro nil)

(nl) (nl) (nl)

(log-macro t)
;;(log-eval  t)
(defun-mem-fun memq?  eq?)
(log-eval  nil)
(log-macro nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(exit)

(write memql?)
(nl)
(write memq?)
(nl)

(log-macro nil)

(if (memql? 5 lst)
 (princ "5 is in list.")
 (princ "5 is not in list."))
(nl)
(if (memql? 88 lst)
 (princ "88 is in list.")
 (princ "88 is not in list."))
(nl)

(if (memq? 5 lst)
 (princ "5 is in list.")
 (princ "5 is not in list."))
(nl)
(if (memq? 88 lst)
 (princ "88 is in list.")
 (princ "88 is not in list."))
(nl)

