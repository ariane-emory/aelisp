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

(report-time-us "def fancy prints               "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; fancy output macros:                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro princn  exprs           $('progn (cons 'princ exprs) $('nl)))
 (defmacro printn  exprs           $('progn (cons 'print exprs) $('nl)))
 (defmacro putn    exprs           $('progn (cons 'put  exprs)  $('nl)))
 (defmacro writen  exprs           $('progn (cons 'write exprs) $('nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro princni (delim . exprs) $('apply 'princn $('intercalate delim $('quote exprs))))
 (defmacro printi  (delim . exprs) $('apply 'print  $('intercalate delim $('quote exprs))))
 (defmacro putni   (delim . exprs) $('apply 'putn   $('intercalate delim $('quote exprs))))
 (defmacro writeni (delim . exprs) $('apply 'writen $('intercalate delim $('quote exprs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro princns exprs           $('princni " " . exprs))
 (defmacro prints  exprs           $('princti " " . exprs))
 (defmacro putns   exprs           $('putni   " " . exprs))
 (defmacro writens exprs           $('writeni " " . exprs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )


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
 (defun id     (o)   o)
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
 (defmacro funcall (fun . args)
  "Apply FUN to ARGS. This only exists to make porting code from other Lisps easier."
  (unless (fun? (eval fun)) (error "FUN must be a function"))
  (cons fun args))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun root-env ()
  "Get the root environment."
  (setq! pos (env))
  (while (env pos)
   (write pos) (nl)
   (setq! pos (env pos)))
  pos) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro defconstant (sym value)
  "Set SYM to VALUE and mark it as constant."
  (unless (symbol? sym) (error "SYM must be a symbol"))
  $('progn
    $('setq! sym value)
    $('put! 't ':constant $('quote sym))
    value))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 (setq! bin-list-to-int  (reduced  (lambda (acc bin) (+ (<< acc 1) bin))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! bin-list-to-int* (reduced* (lambda (acc bin) (+ (<< acc 1) bin))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )


(report-time-us "def list predicates            "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (more unsorted):                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro defun-list-pred-fun (name combiner base-case)
  `(defun ,name (pred? lst)
    (if lst
     (,combiner
      (pred? (car lst))
      (,name pred? (cdr lst)))
     ,base-case)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun-list-pred-fun any? or  nil)
 (defun-list-pred-fun all? and t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro defun-list-transform-fun (name transformer)
  `(defun ,name (lsts)
    (when lsts
     (cons (,transformer (car lsts)) (,name (cdr lsts))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun-list-transform-fun heads caar)
 (defun-list-transform-fun tails cdar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )


(require 'vector-lists)


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


(report-time-us "def list split funs            "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun split-list-alternate! (pred? lst)
  "Destructively split the LST into two sublists:"
  "1. The longest initial sublist of elements satisfying PRED?"
  "2. The rest of the elements."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (list? lst) (error "LST must be a list"))
  (let ((front nil)
        (back lst))
   (while (and back (pred? (car back)))
    (push! (pop! back) front))
   $((reverse front) back)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun split-list (pred? lst)
  "Destructivly split LST into two sublists:"
  "1. The longest initial sublist of elements satisfying PRED?"
  "2. The rest of the elements."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (list? lst) (error "LST must be a list"))
  (let ((prev nil)
        (current lst))
   (while (and current (pred? (car current)))
    (setq! prev current)
    (setq! current (cdr current)))
   (if prev
    (progn
     (rplacd! prev nil)
     $(lst current))
    $(nil lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun split-list (pred? lst)
  "Split LST into two sublists:"
  "1. The longest initial sublist of elements satisfying PRED?"
  "2. The rest of the elements."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (list? lst) (error "LST must be a list"))
  (let ((front nil)
        (current lst))
   (while (and current (pred? (car current)))
    (setq front (cons (car current) front))
    (setq current (cdr current)))
   $((reverse front) current)))
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
