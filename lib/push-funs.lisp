(require 'std-minimal)
(require 'measure-time)
(require 'nconc)

(report-time-us "def push functions             "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (push/push-back):                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun push-back (lst elem) 
  "Non-destructively push ELEM onto the tail of LST."
  (unless (list? lst) (error "LST must be a list"))
  (append lst (cons elem nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun push (elem lst)
  "Non-destructively pop ELEM from the head of LST."
  "The only difference from car is tha an empty LST may not be popped."
  (unless (cons? lst) (error "LST must be a list"))
  (cons elem lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun push (elem lst)
  "Non-destructively push ELEM onto the head of LST, aka cons."
  (unless (list? lst) (error "LST must be a list"))
  (cons elem lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;  (defun push-back! (lst elem)
 ;;   "Destructively push ELEM onto the tail of LST."
 ;;   (unless (list? lst) (error "LST must be a list"))
 ;;   (rplacd! (last lst) (cons elem nil))
 ;;   lst)
 ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;  (defun push! (elem lst)
 ;;   "Destructively push ELEM onto the head of LST."
 ;;   (unless (list? lst) (error "LST must be a list"))
 ;;   (let ((old-car (car lst)))
 ;;    (rplaca! lst elem)
 ;;    (rplacd! lst (cons old-car (cdr lst)))
 ;;    lst))
 ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;  (defmacro push! (val list-sym)
 ;;   "Destructively push an item onto the list bound to LIST-SYM."
 ;;   (unless (symbol? list-sym) (error "LIST-SYM must be a symbol"))
 ;;   $('if $('not $('symbol? $('quote list-sym)))
 ;;     $('error "LIST-SYM must be a symbol")
 ;;     $('setq! list-sym $('cons val list-sym))))
 ;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; (defmacro pop! (list-sym)
 ;;  "Destructively pop an item from the list bound to LIST-SYM."
 ;;  (unless (symbol? list-sym) (error "LIST-SYM must be a symbol"))
 ;;  $('if $('not $('symbol? $('quote list-sym)))
 ;;    $('error "LIST-SYM must be a symbol")
 ;;    $('let $($('head $('car list-sym)))
 ;;      $('setq! list-sym $('cdr list-sym))
 ;;      'head)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'push-funs)
