;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; selection sort parts;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-first! (pred? lst)
 "Destructively remove the first item matching PRED? from LST."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (if (pred? (car lst))
  (if (cdr lst)
   (progn 
    (rplaca! lst (cadr lst))
    (rplacd! lst (cddr lst)))
   (error "can't remove last item"))
  (let ((prev lst) (current (cdr lst)))
   (while (and current (not (pred? (car current))))
    (setq! prev current)
    (setq! current (cdr current)))
   (if current
    (progn
     (rplacd! prev (cdr current))
     (car current))
    (error "obj was not in lst")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun select-and-move-to-front! (pred? lst)
 "Move the first item in LST matching PRED? to its head."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((head (car lst)))
  (if (pred? head)
   head
   (let ((obj      (remove-first! pred? lst))
         (new-tail (cons (car lst) (cdr lst))))
    (rplaca! lst obj)
    (rplacd! lst new-tail)
    obj))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'selection-sort)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
