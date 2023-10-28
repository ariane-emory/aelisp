;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delq:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delq! (item lst)
 "Destructively remove all items eq? to ITEM from LST."
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (while (and lst (eq? (car lst) item))
   (setq! lst (cdr lst)))
  (let ((ptr lst))
   (while (and ptr (cdr ptr))
    (if (eq? (cadr ptr) item)
     (rplacd! ptr (cddr ptr))
     (setq! ptr (cdr ptr)))))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delql! (item lst)
 "Destructively remove all items eql? to ITEM from LST."
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (while (and lst (eql? (car lst) item))
   (setq! lst (cdr lst)))
  (let ((ptr lst))
   (while (and ptr (cdr ptr))
    (if (eql? (cadr ptr) item)
     (rplacd! ptr (cddr ptr))
     (setq! ptr (cdr ptr)))))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'delq)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
