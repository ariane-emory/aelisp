(require 'std-minimal)

(report-time-us "def reduction functions        "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (reduction):                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (letrec
  ((reduce-inner
    (lambda (fun lst)
     (cond
      ((nil? lst)        nil)
      ((nil? (cdr lst))  (car lst))
      (t                 (reduce-inner fun (cons (fun (car lst) (cadr lst)) (cddr lst))))))))
  (defun reduce (fun lst . init-val)
   "Left-reduce ('foldl' in Haskell) LST by applying FUN to successive pairs."
   (cond
    ((not (fun? fun))  (error "FUN must be a function"))
    ((not (list? lst)) (error "LST must be a list"))
    ((cdr init-val)    (error "INIT-VAL must be a single object"))
    ((nil? init-val)   (reduce-inner fun lst))
    (t                 (reduce-inner fun (cons (car init-val) lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (letrec
  ((rreduce-inner
    (lambda (fun lst)
     (cond
      ((nil? lst)        nil)
      ((nil? (cdr lst))  (car lst))
      (t                 (fun (car lst) (rreduce-inner fun (cdr lst))))))))
  (defun rreduce (fun lst . init-val)
   "Right-reduce ('foldr' in Haskell) LST by applying FUN to successive pairs."
   (cond
    ((not (fun? fun))  (error "FUN must be a function"))
    ((not (list? lst)) (error "LST must be a list"))
    ((cdr init-val)    (error "INIT-VAL must be a single object"))
    ((nil? init-val)   (rreduce-inner fun lst))
    (t                 (rreduce-inner fun (append2 lst (list (car init-val))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun reduced (fun . init-val)
  "Return a function that is a left reduction of the binary function FUN."
  (unless (fun? fun)   (error "FUN must be a function"))
  (when (cdr init-val) (error "INIT-VAL must be a single object"))
  (lambda (lst) (reduce fun lst . init-val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun reduced* (fun . init-val)
  "Return a function that is a left reduction of the binary function FUN."
  "which takes loose args."
  (unless (fun? fun)   (error "FUN must be a function"))
  (when (cdr init-val) (error "INIT-VAL must be a single object"))
  (lambda args ((reduced fun . init-val) args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun rreduced (fun . init-val)
  "Return a function that is a right reduction of the binary function FUN."
  (unless (fun? fun)   (error "FUN must be a function"))
  (when (cdr init-val) (error "INIT-VAL must be a single object"))
  (lambda (lst) (rreduce fun lst . init-val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun rreduced* (fun . init-val)
  "Return a function that is a right reduction of the binary function FUN."
  "which takes loose args."
  (unless (fun? fun)   (error "FUN must be a function"))
  (when (cdr init-val) (error "INIT-VAL must be a single object"))
  (lambda args ((rreduced fun . init-val) args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'reduction)
