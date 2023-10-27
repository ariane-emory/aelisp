(require 'compound-cars-and-cdrs)

(report-time-us "get numbered access            "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (retrieving by position):                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun first    (lst)                    (car lst))
 (defun second   (lst)                   (cadr lst))
 (defun third    (lst)                  (caddr lst))
 (defun fourth   (lst)                 (cadddr lst))
 (defun fifth    (lst)            (car (cddddr lst)))
 (defun sixth    (lst)           (cadr (cddddr lst)))
 (defun seventh  (lst)          (caddr (cddddr lst)))
 (defun eighth   (lst)         (cadddr (cddddr lst)))
 (defun ninth    (lst)    (car (cddddr (cddddr lst))))
 (defun tenth    (lst)   (cadr (cddddr (cddddr lst))))
 (defun eleventh (lst)  (caddr (cddddr (cddddr lst))))
 (defun twelfth  (lst) (cadddr (cddddr (cddddr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun nth (index lst)
  "Get the nth item in LST."
  (unless (integer? index) (error "INDEX must be an integer"))
  (unless (list? lst)      (error "LST must be a list"))
  (cond
   ((= 0 index) (car lst))
   (lst          (nth (- index 1) (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun nthcdr (n lst)
  "Get the nth cdr of LST."
  (unless (integer? n) (error "N must be an integer"))
  (unless (list? lst)  (error "LST must be a list"))
  (if (= 0 n)
   lst
   (nthcdr (1- n) (cdr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'numbered-access)
