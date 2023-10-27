(require 'std-minimal)
(require 'measure-time)

(report-time-us "def transform                  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (transform):                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun transform! (pred? fun obj)
  "Destructively transform the cons tree OBJ by replacing members matching
  PRED? with the result of applying FUN to them."
  (when (not (lambda? fun)) (error "FUN must be a function"))
  (when (atom? obj)         (error "OBJ must be a list"))
  (cond
   ((pred? obj) (set! obj (fun obj)))
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
     (cond
      ((pred? head) (rplaca! obj (fun head)))
      ((cons? head) (transform! pred? fun head)))
     (cond
      ((pred? tail) (rplacd! obj (fun tail)))
      ((cons? tail) (rplacd! obj (transform! pred? fun tail))))))
   (t obj))
  obj)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun transform (pred? fun obj)
  "Transform OBJ by replacing members matching PRED? with the result of
  applying FUN to them or, if obj is not a cons tree, by applying FUN to
  OBJ."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (fun? fun?)  (error "FUN must be a function"))
  (cond
   ((and (atom? obj) (pred? obj)) (fun obj))
   ((atom? obj) obj)
   (t
    (cons
     (transform pred? fun (car obj))
     (transform pred? fun (cdr obj))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun prefetch (expr)
  "Try to optimize EXPR by replacing it's symbol? members with the result of
  looking them up. This is, mysteriously, not a very effective optimization."
  (transform!
   (lambda (x) (and (symbol? x) (bound? x)))
   (lambda (x) (eval x))
   expr))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'transform)
