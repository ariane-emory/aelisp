;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (retrieving by position):
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
 (let ((current lst))
   (while (and (> index 0) current)
     (setq current (cdr current))
     (setq index (1- index)))
   (if current
       (car current)
     nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nthcdr (n lst)
 "Get the nth cdr of LST."
 (unless (integer? n) (error "N must be an integer"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((current lst))
   (while (and (> n 0) current)
     (setq current (cdr current))
     (setq n (1- n)))
   current))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun nth (index lst)
;;  "Get the nth item in LST."
;;  (unless (integer? index) (error "INDEX must be an integer"))
;;  (unless (list? lst)      (error "LST must be a list"))
;;  (cond
;;   ((zero? index) (car lst))
;;   (lst          (nth (- index 1) (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun nthcdr (n lst)
;;  "Get the nth cdr of LST."
;;  (unless (integer? n) (error "N must be an integer"))
;;  (unless (list? lst)  (error "LST must be a list"))
;;  (if (zero? n)
;;   lst
;;   (nthcdr (1- n) (cdr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun last (lst)
 "Get last item in a LST."
 (unless (list? lst) (error "LST must be a list"))
 (cond
  ((nil? (cdr lst)) lst)
  (lst              (last (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-access-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
