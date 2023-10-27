;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple std load time measuerement:                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! before (now-us))
(setq! print-load-time-us
 (lambda ()
  (princ "Loaded in ")
  (princ (elapsed-us before))
  (princ " us.")
  (nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'std-minimal)
(require 'measure-time)
(require 'core-aliases)
(require 'compound-cars-and-cdrs)
(require 'quasiquote)
(require 'numbered-access)
(require 'reduce)
(require 'map)
(require 'append)
(require 'nconc)
(require 'push-funs)
(require 'flatten)
(require 'zip)
(require 'tail-chaser-macros)
(require 'transform)
(require 'sort)
(require 'list-funs)
(require 'union)
(require 'predicates)
(require 'log-toggle)
(require 'vector-lists)
(require 'split-list)
(require 'print-macros)
(require 'std-misc)

(report-time-us "def tail chasers               "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro make-member-pred (pred?)
  `(make-chase-fun (obj lst)
    ((,pred? obj head) t)
    (position (chase obj))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro make-remove-fun (pred?)
  `(make-chase-fun (obj lst)
    ((,pred? obj head) tail)
    (position (cons head (chase obj)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro make-index-fun (pred?)
  `(make-chase-fun (obj lst)
    ((,pred? obj head) (car rest))
    (position (chase obj (if rest (1+ (car rest)) 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! indexq   (make-index-fun   eq?))
 (setq! memq?    (make-member-pred eq?))
 (setq! removeq  (make-remove-fun  eq?))
 (setq! indexql  (make-index-fun   eql?))
 (setq! memql?   (make-member-pred eql?))
 (setq! removeql (make-remove-fun  eql?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )




(report-time-us "def selection sort parts       "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun remove-first! (pred? lst)
  "Destructively remove the first item matching PRED? from LST."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (if (pred? (car lst))
   (if (cdr lst)
    (progn 
     (rplaca! lst (cadr lst))
     (rplacd! lst (cddr lst)))
    (error "can't remove last item"))
   (let ((prev lst) (current (cdr lst)))
    (while (and current (not (pred? (car current))))
     (setq! prev current)
     (setq! current (cdr current)))
    (if current
     (progn
      (rplacd! prev (cdr current))
      (car current))
     (error "obj was not in lst")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun select-and-move-to-front! (pred? lst)
  "Move the first item in LST matching PRED? to its head."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (let ((head (car lst)))
   (if (pred? head)
    head
    (let ((obj      (remove-first! pred? lst))
          (new-tail (cons (car lst) (cdr lst))))
     (rplaca! lst obj)
     (rplacd! lst new-tail)
     obj))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )


(mapc* require
 'delq
 'doc-strings
 'primes
 'confirm
 'strings
 'benchmark
 'prop-removeb
 'prog-macros
 'plist-funs
 'prop-removeb
 'scheme-compat
 'elisp-compat
 'std-aliases)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print-load-time-us)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'std)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
