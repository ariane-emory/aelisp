;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list remove funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeq (elem lst)
 "Non-destructively remove ELEM from LST. Comparison done with 'eq?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (unless (eq? elem (car lst))
    (setq! result (cons (car lst) result)))
   (setq! lst (cdr lst)))
  (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeql (elem lst)
 "Non-destructively remove ELEM from LST. Comparison done with 'eql?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (unless (eql? elem (car lst))
    (setq! result (cons (car lst) result)))
   (setq! lst (cdr lst)))
  (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeq! (obj lst)
 "Remove the first item eq? to OBJ from LST."
 (unless (list? lst) (error "LST must be a list"))
 (let ((head (car lst))
       (tail (cdr lst)))
  (if (eq? obj head)
   (if (nil? tail)
    (error "can't remove last item from LST")
    (rplaca! lst (second lst))
    (rplacd! lst (cddr   lst)))
   (let ((prev     lst)
         (current  (cdr lst)))
    (letrec
     ((chase
       (lambda (lst)
        (let ((head (car lst))
              (next (cdr lst)))
         (cond
          ((eq? obj head) (progn (rplacd! prev next) obj))
          (next           (progn (setq! prev lst) (chase next)))
          (t              (error "OBJ was not found in LST")))))))
     (chase current))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeql! (obj lst)
 "Remove the first item eql? to OBJ from LST."
 (unless (list? lst) (error "LST must be a list"))
 (let ((head (car lst))
       (tail (cdr lst)))
  (if (eql? obj head)
   (if (nil? tail)
    (error "can't remove last item from LST")
    (rplaca! lst (second lst))
    (rplacd! lst (cddr   lst)))
   (let ((prev     lst)
         (current  (cdr lst)))
    (letrec
     ((chase
       (lambda (lst)
        (let ((head (car lst))
              (next (cdr lst)))
         (cond
          ((eql? obj head) (progn (rplacd! prev next) obj))
          (next            (progn (setq! prev lst) (chase next)))
          (t               (error "OBJ was not found in LST")))))))
     (chase current))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-remove)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
