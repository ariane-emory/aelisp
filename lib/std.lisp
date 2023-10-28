;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; std config:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! *use-safe-provide*      nil)
(setq! *microbench*            t)
(setq! *microbench-defmacros*  t)
(setq! *microbench-defuns*     t)
(setq! *microbench-provides*   t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simpler-version of std load time measuerement:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! *before-std* (now-us))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crucial macros, without which nothing else in std will even work:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! defmacro
 (macro (name params . body)
  (unless (eq? :SYMBOL (type name))
   (error "NAME must be a symbol"))
  (unless (or (eq? :CONS (type params)) (eq? :SYMBOL (type params)))
   (error "PARAMS must be a list or symbol"))
  (unless (eq? :CONS (type body))
   (error "BODY must be a cons"))
  $('setq! name $('macro params . body))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun (name params . body)
 (unless (eq? :SYMBOL (type name))
  (error "NAME must be a symbol"))
 (unless (or (eq? :CONS (type params)) (eq? :SYMBOL (type params)))
  (error "PARAMS must be a list or symbol"))
 (unless (eq? :CONS (type body))
  (error "BODY must be a cons"))
 $('setq! name $('lambda params . body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; feature? and provide:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let
 ((feature-memq?
   (if (bound? 'memq?)
    memq?
    (lambda (elem lst)
     (let ((found nil))
      (while (and lst (not found))
       (if (eq? elem (car lst))
        (setq! found t)
        (setq! lst (cdr lst))))
      found)))))
 (defun feature? (feature)
  "t if FEATURE is present in *features*."
  (unless (and (eq? :SYMBOL (type feature)) (not (keyword? feature)))
   (error "FEATURE must be a non-keyword symbol"))
  (feature-memq? feature *features*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun safe-provide (feature)
 "Add FEATURE to *features* if it is not already present."
 (unless (and (eq? :SYMBOL (type feature)) (not (keyword? feature)))
  (error "FEATURE must be a non-keyword symbol"))
 (unless (feature? feature) (push! feature *features*))
 feature)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun provide! (feature)
 "Add FEATURE to *features*."
 (push! feature *features*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! provide (if *use-safe-provide* safe-provide provide!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; microbenchmark:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and *microbench* *microbench-defmacros*)
 (setq! defmacro-base defmacro)
 (setq! defmacro
  (macro (name params . body)
   $('progn
     $('setq! '*microbench-before* $('now-us))
     $('let $($('result $('defmacro-base name params . body)))
       $('nl)
       $('princ "defmacrod  " $('quote name) " in "
         $('elapsed-us '*microbench-before*) " us.")
       'result)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and *microbench* *microbench-defuns*)
 (setq! defun-base defun)
 (defmacro defun (name params . body)
  $('progn
    $('setq! '*microbench-before* $('now-us))
    $('let $($('microbenched-fun's-result $('defun-base name params . body)))
      $('nl)
      $('princ "defunned   " $('quote name) " in "
        $('elapsed-us '*microbench-before*) " us.")
      'microbenched-fun's-result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and *microbench* *microbench-provides*)
 (setq! provide-base provide)
 (defun provide (feature)
  (setq! *microbench-before* (now-us))
  (let ((result (provide-base feature)))
   (nl)
   (princ "provide    '" feature " in " (elapsed-us *microbench-before*) " us."))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; already defined above:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'features)
(provide 'microbench)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type predicates:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun type?     (typ o) (eq? typ       (type           o)))
(defun atom?     (o)     (not (type?    :CONS           o)))
(defun char?     (o)          (type?    :CHAR           o))
(defun cons?     (o)          (type?    :CONS           o))
(defun core?     (o)          (type?    :CORE           o))
(defun env?      (o)          (type?    :ENV            o))
(defun error?    (o)          (type?    :ERROR          o))
(defun float?    (o)          (type?    :FLOAT          o))
(defun integer?  (o)          (type?    :INTEGER        o))
(defun lambda?   (o)          (type?    :LAMBDA         o))
(defun λ?        (o)          (type?    :LAMBDA         o))
(defun macro?    (o)          (type?    :MACRO          o))
(defun rational? (o)          (type?    :RATIONAL       o))
(defun string?   (o)          (type?    :STRING         o))
(defun symbol?   (o)          (type?    :SYMBOL         o))
(defun improper? (o)     (and (list? o) (not (proper?   o))))
(defun fun?      (o)     (or  (core? o) (lambda? o) (macro? o)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! list? tail?)
(setq! pair? cons?) ;; scheme compatability
(setq! null? nil?)  ;; scheme compatability
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some basic funs/macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ignore args
 "Ignores ARGS and do nothing."
 nil)
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
(defun id (o) o)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'basic-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! s       setq!)
(setq! setcar! rplaca!)
(setq! setcdr! rplacd!)
(setq! ¬       not)          
(setq! ∨       or )           
(setq! ∧       and)
(setq! list?   tail?)
(setq! setcdr! rplacd!)
(setq! setcar! rplaca!)
(setq! λ       lambda)
(setq! lte     <=)
(setq! gte     >=)
(setq! lt      <)
(setq! gt      >)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-aliases)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound car/cdrs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caar     (lst)               (car (car lst)))
(defun cadr     (lst)               (car (cdr lst)))
(defun cdar     (lst)               (cdr (car lst)))
(defun cddr     (lst)               (cdr (cdr lst)))
(defun caaar    (lst)          (car (car (car lst))))
(defun caadr    (lst)          (car (car (cdr lst))))
(defun cadar    (lst)          (car (cdr (car lst))))
(defun caddr    (lst)          (car (cdr (cdr lst))))
(defun cdaar    (lst)          (cdr (car (car lst))))
(defun cdadr    (lst)          (cdr (car (cdr lst))))
(defun cddar    (lst)          (cdr (cdr (car lst))))
(defun cdddr    (lst)          (cdr (cdr (cdr lst))))
(defun caaaar   (lst)     (car (car (car (car lst)))))
(defun caaadr   (lst)     (car (car (car (cdr lst)))))
(defun caadar   (lst)     (car (car (cdr (car lst)))))
(defun caaddr   (lst)     (car (car (cdr (cdr lst)))))
(defun cadaar   (lst)     (car (cdr (car (car lst)))))
(defun cadadr   (lst)     (car (cdr (car (cdr lst)))))
(defun caddar   (lst)     (car (cdr (cdr (car lst)))))
(defun cadddr   (lst)     (car (cdr (cdr (cdr lst)))))
(defun cdaaar   (lst)     (cdr (car (car (car lst)))))
(defun cdaadr   (lst)     (cdr (car (car (cdr lst)))))
(defun cdadar   (lst)     (cdr (car (cdr (car lst)))))
(defun cdaddr   (lst)     (cdr (car (cdr (cdr lst)))))
(defun cddaar   (lst)     (cdr (cdr (car (car lst)))))
(defun cddadr   (lst)     (cdr (cdr (car (cdr lst)))))
(defun cdddar   (lst)     (cdr (cdr (cdr (car lst)))))
(defun cddddr   (lst)     (cdr (cdr (cdr (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'compound-cars-and-cdrs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove! property macro:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro remove! (prop obj)
 "Remove a property PROP from OBJ."
 $('prog1
   $('quote $('get prop obj))
   $('props! obj $('plist-removeql prop $('props obj)))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'remove-prop-macro)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (append/nconc variants):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun append2 (lst1 lst2)
 "Append LST1 and LST2."
 (unless (list? lst1) (error "LST1 must be a list"))
 (unless (list? lst2) (error "LST2 must be a list"))
 (let ((result nil)          ; Initialize the result list as empty
       (last-cell nil)       ; Track the last cell in the result
       (current lst1))
  ;; Iterate over lst1 and copy elements to result
  (while current
   (let ((new-cell (cons (car current) nil)))
    (if (nil? result)
     (setq! result new-cell)    ; Initialize result if it's the first element
     (rplacd! last-cell new-cell)) ; Attach new-cell to the end of result
    (setq! last-cell new-cell)
    (setq! current (cdr current))))
  ;; Attach lst2 to the end of result
  (if (nil? result)
   lst2
   (progn
    (rplacd! last-cell lst2)
    result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun append lists
 "Append any number of LISTS."
 (let ((result nil)
       (last-cell nil)
       (current-lists lists))
  (while current-lists
   (let ((current-list (car current-lists)))
    (unless (list? current-list) (error "Every argument must be a list"))
    (while current-list
     (let ((new-cell (cons (car current-list) nil)))
      (if (nil? result)
       (setq! result new-cell)
       (rplacd! last-cell new-cell))
      (setq! last-cell new-cell)
      (setq! current-list (cdr current-list))))
    (setq! current-lists (cdr current-lists))))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nconc2! (lst1 lst2)
 "Destructively join LST1 an LST2."
 (unless (list? lst1) (error "LST1 must be a list"))
 (unless (list? lst2) (error "LST2 must be a list"))
 (cond
  ((nil? lst1) lst2)
  (t           (rplacd! (last lst1) lst2) lst1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nconc! lists
 "Destructively concatenate multiple lists."
 (let ((result (car lists))
       (rest-lists (cdr lists)))
  (while rest-lists
   (setq! result (nconc2! result (car rest-lists)))
   (setq! rest-lists (cdr rest-lists)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'append-and-nconc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quasiquotation:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'append-and-nconc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro expand-quasiquoted (expr)
 "Expand a quasiquoted expression and resolve unquotes and"
 "unquote-splicings within."
 (cond
  ;; If it's not a cons then it's an atom that we should quote.
  ((atom? expr)
   $('quote expr))
  ;; Directly replace (unquote x) with x.
  ((eq? (car expr) 'unquote)
   (car (cdr expr)))
  ;; If the second element of the list is an unquote-splicing, we want to use
  ;; append2.
  ((and
    (cons? (cdr expr))
    (cons? (cadr expr))
    (eq?   (caadr expr) 'unquote-splicing))
   $('append2
     $('list $('expand-quasiquoted (car expr)))
     (cadadr expr)))
  ;; If the second element of the list is an unquote, use cons but without
  ;; splicing.
  ((and
    (cons? (cdr expr))
    (eq?   (cadr expr) 'unquote))
   $('cons
     $('expand-quasiquoted (car expr))
     (caddr expr)))
  ;; Error out for splicing outside of list context
  ((eq? (car expr) 'unquote-splicing)
   (error "unquote-splicing can't occur at top level"))
  ;; If the list is regular, we just recurse on both its parts
  (t $('cons
       $('expand-quasiquoted (car expr))
       $('expand-quasiquoted (cdr expr))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! quasiquote expand-quasiquoted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'quasiquote)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(setq! *std-modules*
 $(
   'list-access-funs
   'reduce-funs
   'map-funs
   'push-pop-funs
   'flatten-funs
   'transform
   'merge-sort
   'union
   'misc-list-funs
   'misc-predicate-funs
   'log-toggle-funs
   'benchmark
   'unsorted-funs
   'vector-list-funs
   'list-member-funs
   'list-index-funs
   'list-remove-funs
   'split-list
   'delq
   'prime-funs
   'selection-sort-funs
   'test-macros
   'string-funs
   'prog-macros
   'plist-funs
   'std-aliases
   'scheme-compat-aliases
   'elisp-compat-aliases))

(let ((module *std-modules*))
 (while module
  (require (car module))
  (setq! module (cdr module))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'std)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(princ "Loaded in   ")
(princ (elapsed-us *before-std*))
(princ " us.")
(nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
