;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (append/nconc variants):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun append2 (lst1 lst2)
 "Append LST1 and LST2."
 (unless (list? lst1) (error "LST1 must be a list"))
 (unless (list? lst2) (error "LST2 must be a list"))
 (let ((result nil)          ; Initialize the result list as empty
       (last-cell nil)       ; Track the last cell in the result
       (current lst1))
  ;; Iterate over lst1 and copy elements to result
  (while current
   (let ((new-cell (cons (car current) nil)))
    (if (nil? result)
     (setq! result new-cell)    ; Initialize result if it's the first element
     (rplacd! last-cell new-cell)) ; Attach new-cell to the end of result
    (setq! last-cell new-cell)
    (setq! current (cdr current))))
  ;; Attach lst2 to the end of result
  (if (nil? result)
   lst2
   (progn
    (rplacd! last-cell lst2)
    result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun append lists
 "Append any number of LISTS."
 (let ((result nil)
       (last-cell nil)
       (current-lists lists))
  (while current-lists
   (let ((current-list (car current-lists)))
    (unless (list? current-list) (error "Every argument must be a list"))
    (while current-list
     (let ((new-cell (cons (car current-list) nil)))
      (if (nil? result)
       (setq! result new-cell)
       (rplacd! last-cell new-cell))
      (setq! last-cell new-cell)
      (setq! current-list (cdr current-list))))
    (setq! current-lists (cdr current-lists))))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nconc2! (lst1 lst2)
 "Destructively join LST1 an LST2."
 (unless (list? lst1) (error "LST1 must be a list"))
 (unless (list? lst2) (error "LST2 must be a list"))
 (cond
  ((nil? lst1) lst2)
  (t           (rplacd! (last lst1) lst2) lst1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nconc! lists
 "Destructively concatenate multiple lists."
 (let ((result (car lists))
       (rest-lists (cdr lists)))
  (while rest-lists
   (setq! result (nconc2! result (car rest-lists)))
   (setq! rest-lists (cdr rest-lists)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'append-and-nconc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
