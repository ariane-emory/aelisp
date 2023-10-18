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

