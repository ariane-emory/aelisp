(require 'std-minimal)
(require 'measure-time)

(report-time-us "def unsorted                   "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; random unsorted stuff:                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun apply* (fun . args)
  "Try to remember how this one works and document it."
  (unless (fun? fun) (error "FUN must be a function"))
  (apply fun (apply list* args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun curry1 (fun arg1)
  "Curry the first argument of FUN as ARG1. This would be better if it were a macro."
  (unless (fun? fun) (error "FUN must be a function"))
  (lambda args
   (apply fun arg1 args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun spc    ()    (princ " "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun max(a b)
  "Return the larger of A and B."
  (unless (integer? a) (error "A must be an integer"))
  (unless (integer? b) (error "B must be an integer"))
  (if (> a b) a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun min(a b)
  "Return the smaller of A and B."
  (unless (integer? a) (error "A must be an integer"))
  (unless (integer? b) (error "B must be an integer"))
  (if (< a b) a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun 1+(n)
  "Return N + 1."
  (unless (integer? n) (error "N must be an integer"))
  (+ 1 n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun 1-(n)
  "Return N - 1."
  (unless (integer? n) (error "N must be an integer"))
  (- n 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun double(n)
  "Return N * 2."
  (unless (integer? n) (error "N must be an integer"))
  (<< n 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! 2*     double)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro ignore args
  "Ignores ARGS and do nothing."
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun add-logging-to (fun)
  "Add logging to a function FUN."
  (unless (fun? fun) (error "FUN must be a function"))
  (if (has? :added-logging fun)
   (error "logging was already added to this fun")
   (let* ((fun-name      (get :last-bound-to fun))
          (old-fun-body  (body fun))
          (old-body-tail (cdr old-fun-body))
          (new-body
           `((princ
              "Applying " ',fun-name
              " to parameters "  (syms (env))
              " with arguments " (vals (env)) ".")
             (nl)
             (let ((result (progn ,@old-body-tail)))
              (princ "Result of applying " ',fun-name " was " result ".")
              (nl)
              result))))
    (rplacd! (body fun) new-body))
   (put! t :added-logging fun) (nl)
   fun))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! bin-list-to-int  (reduced  (lambda (acc bin) (+ (<< acc 1) bin))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! bin-list-to-int* (reduced* (lambda (acc bin) (+ (<< acc 1) bin))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'std-misc)
