(require 'std-minimal)
(require 'measure-time)

(report-time-us "def vector-lists               "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (vector-style list API):                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! list-set!
  (make-chase-fun (lst index)
   ((zero? index) (rplaca! position (car rest)))
   (position (chase (1- index) (car rest)))
   (t (error "INDEX out of range"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! list-ref
  (make-chase-fun (lst index)
   ((zero? index) head)
   (position (chase (1- index)))
   (t (error "INDEX out of range"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! list-length length)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun make-list (size init-val)
  "Make a new list of length SIZE with it's cars set to INIT-VAL."
  (unless (integer? size) (error "SIZE must be an integer"))
  (cond
   ((zero? size)  nil)
   (t            (cons init-val (make-list (1- size) init-val)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'vector-lists)
