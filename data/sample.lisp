;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (log-eval  t)
;; (log-core  t)
;; (log-macro t)
(setq! lst '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defmacro defun-mem-fun (name pred?)
 `(defun ,name (x lst)
   (cond
	  ((,pred? (car lst) x) t)
	  (lst (,name x (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun-mem-fun memql? eql?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun-mem-fun memq?  eq?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defmacro defun-remove-fun (name pred?)
  `(defun ,name (x lst)
     (cond
      ((,pred? (car lst) x) (cdr lst))
      (lst (cons (car lst) (,name x (cdr lst)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-remove-fun removeq eq?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-remove-fun removeql eql?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun-index-fun (name pred?)
  `(defun ,name (x lst)
     (cond
      ((,pred? x (car lst)) 0)
      (lst
       (let ((tail-result (,name x (cdr lst))))
         (when tail-result
           (+ 1 tail-result)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-index-fun indexq eq?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-index-fun indexql eql?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(write (memq? 5 lst)) (nl)
(write (memql? 5 lst)) (nl)
(write (indexq 5 lst)) (nl)
(write (indexql 5 lst)) (nl)
(write (removeq 5 lst)) (nl)
(write (removeql 5 lst)) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(exit)
