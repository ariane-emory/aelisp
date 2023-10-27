(require 'std-minimal)
(require 'measure-time)

(report-time-us "def vector-lists               "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (vector-style list API):                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! list-set!
  (make-chase-fun (lst index)
   ((= 0 index) (rplaca! position (car rest)))
   (position (chase (- index 1) (car rest)))
   (t (error "INDEX out of range"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! list-ref
  (make-chase-fun (lst index)
   ((= 0 index) head)
   (position (chase (- index 1)))
   (t (error "INDEX out of range"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! list-length length)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun make-list (size init-val)
  "Make a new list of length SIZE with it's cars set to INIT-VAL."
  (unless (integer? size) (error "SIZE must be an integer"))
  (cond
   ((= 0 size)  nil)
   (t            (cons init-val (make-list (- size 1) init-val)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'vector-lists)
