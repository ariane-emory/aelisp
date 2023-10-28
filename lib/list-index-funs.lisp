;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list index funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indexq (elem lst)
 "Return the zero-based index of the first occurrence of ELEM in LST, or nil if"
 "ELEM does not appear in the list. Comparison done with 'eq?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((idx 0)
       (found nil))
  (while (and lst (not found))
   (if (eq? elem (car lst))
    (setq! found idx)
    (setq! idx (+ 1 idx))
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indexql (elem lst)
 "Return the zero-based index of the first occurrence of ELEM in LST, or nil if"
 "ELEM does not appear in the list. Comparison done with 'eql?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((idx 0)
       (found nil))
  (while (and lst (not found))
   (if (eql? elem (car lst))
    (setq! found idx)
    (setq! idx (+ 1 idx))
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-index-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
