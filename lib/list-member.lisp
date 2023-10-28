;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list mem funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun memq? (elem lst)
 "Return non-nil if ELEM is an element of LST. Comparison done with 'eq?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((found nil))
  (while (and lst (not found))
   (if (eq? elem (car lst))
    (setq! found t)
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun memql? (elem lst)
 "Return non-nil if ELEM is an element of LST. Comparison done with 'eql?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((found nil))
  (while (and lst (not found))
   (if (eql? elem (car lst))
    (setq! found t)
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-member)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
