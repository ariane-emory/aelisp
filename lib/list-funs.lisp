(require 'std-minimal)
(require 'measure-time)

(report-time-us "def misc list funs             "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (unsorted):                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun last (lst)
  "Get last item in a LST."
  (unless (list? lst) (error "LST must be a list"))
  (cond
   ((nil? (cdr lst)) lst)
   (lst              (last (cdr lst)))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun list* args
  (let*
   ((chase
	   (lambda (args)
		  (cond
       ((nil? args)       nil)
		   ((nil? (cdr args)) (car args))
		   (t                 (cons (car args) (chase (cdr args))))))))
   (chase args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun depth (lst)
  "Get the depth of a nested list structure. This one is untested."
  (unless (list? lst) (error "LST must be a list"))
  (if (atom? lst)
   0
   (max 1 (+ (depth (car lst)) (depth (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun filter (pred? lst)
  "Return a list containing those members of lst satisfying pred?."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (cond
   ((nil? lst) nil)
   ((pred? (car lst))
    (cons (car lst) (filter pred? (cdr lst))))
   (t (filter pred? (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun intercalate (intercalated lst)
  "Intercalate intercalated between items."
  (unless (list? lst) (error "LST must be a list"))
  (if (or (nil? lst) (nil? (cdr lst)))
   lst
   (cons (car lst)
    (cons intercalated
     (intercalate intercalated (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun butlast (lst)
  "Returns a new list that contains all the elements of the input list except"
  "last one."
  (unless (list? lst) (error "LST must be a list"))
  (if (or (nil? lst) (nil? (cdr lst)))
   nil
   (cons (car lst) (butlast (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun reverse (lst)
  "Returns a new list that is the reverse of the input list."
  (unless (list? lst) (error "LST must be a list"))
  (letrec
   ((reverse-internal
     (lambda (lst acc)
      (if (nil? lst)
       acc
       (reverse-internal (cdr lst) (cons (car lst) acc))))))
   (reverse-internal lst nil)))
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
           (next            (progn (setq! prev lst) (chase next)))
           (t               (error "OBJ was not found in LST"))
           )))))
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
           (t               (error "OBJ was not found in LST"))
           )))))
      (chase current))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun copy-list (lst)
  "Take shallow copy of LST."
  (unless (list? lst) (error "LST must be a list"))
  (when lst (cons (car lst) (copy-list (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'list-funs)
