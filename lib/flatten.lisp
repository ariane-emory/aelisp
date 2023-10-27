(require 'std-minimal)
(require 'measure-time)
(require 'nconc)

(report-time-us "def flatten funs               "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (flattening):                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun flatten1 (lst)
  (unless (list? lst) (error "LST must be a list"))
  (cond
   ((nil? lst) nil)
   ((list? (car lst))
    (append (car lst) (flatten1 (cdr lst))))
   (t (cons (car lst) (flatten1 (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun flatten-left (lst)
  "Flatten a left-nested list structure LST."
  (unless (list? lst) (error "LST must be a list"))
  (if (cons? (car lst))i)
  (append (flatten-left (car lst)) $(cadr lst))
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun flatten (lst)
  (unless (list? lst) (error "LST must be a list"))
  (when lst
   (if (cons? (car lst))
    (append (flatten (car lst)) (flatten (cdr lst)))
    (cons (car lst) (flatten (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'flatten)
