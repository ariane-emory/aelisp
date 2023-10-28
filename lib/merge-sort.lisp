;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (sorting):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(letrec
 ((half
   (lambda (lst)
    "Splits LST into two approximately equal parts."
    (let ((slow lst)
          (fast (cdr lst)))
     (while (and fast (cdr fast))
      (setq! slow (cdr slow))
      (setq! fast (cddr fast)))
     (let ((right (cdr slow)))
      (rplacd! slow nil)
      (cons lst right)))))
  (merge
   (lambda (left right pred?)
    "Merge two sorted lists, LST1 and LST2, into a single sorted list according"
    "to the binary predicate PRED?."
    (cond
     ((nil? left)  right)
     ((nil? right) left)
     ((pred? (car left) (car right))
      (cons (car left) (merge (cdr left) right pred?)))
     (t
      (cons (car right) (merge left (cdr right) pred?)))))))
 (defun sort!!  (lst pred?)
  "Just a basic merge sort of LST by PRED?, destroying LST in the process and"
  "returning a new list."
  (unless (list? lst)  (error "LST must be a list"))
  (unless (fun? pred?) (error "PRED? must be a function"))
  (if (or (nil? lst) (nil? (cdr lst)))
   lst
   (let* ((splits (half lst))
          (left   (car splits))
          (right  (cdr splits)))
    (merge (sort!!  left pred?)
     (sort!!  right pred?) pred?)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'merge-sort)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
