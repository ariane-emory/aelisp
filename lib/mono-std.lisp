;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *log-loading-std-enabled*
 (princ "Loading monolithic std...")
 (nl))

(require 'std-fundamental)

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
(setq! has-key? khas?)
(setq! get-key  kget)
(setq! set-key  kset)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (retrieving by position):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun first    (lst)                    (car lst))
(defun second   (lst)                   (cadr lst))
(defun third    (lst)                  (caddr lst))
(defun fourth   (lst)                 (cadddr lst))
(defun fifth    (lst)            (car (cddddr lst)))
(defun sixth    (lst)           (cadr (cddddr lst)))
(defun seventh  (lst)          (caddr (cddddr lst)))
(defun eighth   (lst)         (cadddr (cddddr lst)))
(defun ninth    (lst)    (car (cddddr (cddddr lst))))
(defun tenth    (lst)   (cadr (cddddr (cddddr lst))))
(defun eleventh (lst)  (caddr (cddddr (cddddr lst))))
(defun twelfth  (lst) (cadddr (cddddr (cddddr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nth (index lst)
 "Get the nth item in LST."
 (unless (integer? index) (error "INDEX must be an integer"))
 (unless (list? lst)      (error "LST must be a list"))
 (cond
  ((zero? index) (car lst))
  (lst          (nth (- index 1) (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nthcdr (n lst)
 "Get the nth cdr of LST."
 (unless (integer? n) (error "N must be an integer"))
 (unless (list? lst)  (error "LST must be a list"))
 (if (zero? n)
  lst
  (nthcdr (1- n) (cdr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun last (lst)
 "Get last item in a LST."
 (unless (list? lst) (error "LST must be a list"))
 (cond
  ((nil? (cdr lst)) lst)
  (lst              (last (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-access-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (tail chaser macros):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-chase-fun (params . cond-clauses)
 "Creates a function for recursive traversal and processing of lists."
  
 "This macro is a generalized version that allows customization of"
 "the parameter order through the PARAMS parameter."
  
 "PARAMS:       A list of length 2 that specifies the parameter order."
 "              One parameter must be the symbol 'lst."
 "COND-CLAUSES: The conditions to process the list."
 (unless (cons? params)        (error "PARAMS must be a list"))
 (unless (cons? cond-clauses)  (error "COND-clauses must be a list"))
 (unless (= 2 (length params)) (error "PARAMS needs length 2"))
 (let* ((lst-is-first? (eq? 'lst (first  params)))
        (user-param    (if lst-is-first? (second params) (first params)))
        (lambda-params (cons (first params) (cons (second params) 'rest))))
  (unless (or lst-is-first? (eq? 'lst (second params)))
   (error "one of the PARAMS must be the symbol 'lst"))
  `(lambda ,lambda-params
    (let ((position lst))
     (letrec
      ((chase-internal
        (lambda (,user-param lst . rest)
         (setq! position lst)
         (let ((head (car position))
               (tail (cdr position)))
          (cond ,@cond-clauses))))
       (chase
        (lambda (,user-param . rest)
         (chase-internal ,user-param (cdr position) . rest))))
      (chase-internal ,user-param position . rest))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'tail-chaser-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (reduction):
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
(provide 'reduce)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (map variants):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar (fun lst)
 "Map fun over LST, returning the resulting list."
 (unless (fun? fun)   (error "FUN must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((acc nil))
  (while lst
   (setq! acc (cons (fun (car lst)) acc))
   (setq! lst    (cdr lst)))
  (reverse acc)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar* (fun . args) (apply mapcar fun (list args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar! (fun lst)
 "Map fun over LST, altering the list."
 (unless (fun? fun)  (error "FUN must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (let ((current lst))
  (while current
   (setcar! current (fun (car current)))
   (setq! current (cdr current)))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapconcat (fun lst . rest)
 "Map fun over LST, returning the result of concatenating the resulting strings."
 (unless (fun? fun)     (error "FUN must be a function"))
 (unless (list? lst)    (error "LST must be a list"))
 (unless (or (nil? rest) (single? rest))
  (error "MAPCONCAT takes exactly only one optional arguments after LST"))
 (let ((delimiter (car rest))
       (acc (if lst (fun (car lst)) ""))
       (current (cdr lst)))
  (unless (or (nil? delimiter) (string? delimiter))
   (error "DELIMITER must be a string or nil"))
  (while current
   (setq acc (concat acc (or delimiter "") (fun (car current))))
   (setq current (cdr current)))
  acc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcan (fun lst)
 "Map fun over LST and concatenate the results by altering them."
 (unless (fun? fun)   (error "FUN must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((results nil)
       (tail nil)
       (current lst))
  (while current
   (let ((result (fun (car current))))
    (when result
     (if results
      (progn
       (setq! tail (nconc! tail result))
       (setq! tail (last tail)))
      (setq! results result)
      (setq! tail results))))
   (setq! current (cdr current)))
  results))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapc (fun lst)
 "Apply FUN to each element of LST for side effects only and return LST."
 (unless (fun? fun)   (error "FUN must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((current lst))
  (while current
   (fun (car current))
   (setq! current (cdr current)))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapc* (fun . args) (apply mapc fun (list args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (flattening):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten1 (lst)
 (unless (list? lst) (error "LST must be a list"))
 (cond
  ((nil? lst) nil)
  ((list? (car lst))
   (append (car lst) (flatten1 (cdr lst))))
  (t (cons (car lst) (flatten1 (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten-left (lst)
 "Flatten a left-nested list structure LST."
 (unless (list? lst) (error "LST must be a list"))
 (if (cons? (car lst))i)
 (append (flatten-left (car lst)) $(cadr lst))
 lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (lst)
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (if (cons? (car lst))
   (append (flatten (car lst)) (flatten (cdr lst)))
   (cons (car lst) (flatten (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'flatten)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (zipping):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zip2 (lst1 lst2)
 "Zip LST1 and LST2."
 (unless (list? lst1) (error "LST1 must be a list"))
 (unless (list? lst2) (error "LST2 must be a list"))
 (cond
  ((∨ (nil? lst1) (nil? lst2)) nil)
  (t  (cons  $((car lst1) (car lst2))
       (zip2   (cdr lst1) (cdr lst2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zip3 (l1 l2 l3)
 "Zip the three lists LST1, LST2 and LST3."
 (unless (list? l1) (error "LST1 must be a list"))
 (unless (list? l2) (error "LST2 must be a list"))
 (unless (list? l3) (error "LST3 must be a list"))
 (mapcar flatten1 (reduce zip2 $(l2 l3) l1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! left-nested-zip (reduced* zip2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro zip lists
 "Zip many lists. This might not flatten properly if the zipped elements are"
 "themselves lists."
 (unless (list? lists) (error "LISTS must be a list"))
 (if (cdr lists)
  $('mapcar 'flatten (cons 'left-nested-zip lists))
  $('mapcar 'list  (car lists))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'zip)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (push/push-back):
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
(provide 'push-pop-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (transform):
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
(provide 'transform)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (sorting):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(letrec
 ((half
   (lambda (lst)
    "Splits LST into two approximately equal parts."
    (let ((slow lst)
          (fast (cdr lst)))
     (while (and fast (cdr fast))
      (setq! slow (cdr slow))
      (setq! fast (cddr fast)))
     (let ((right (cdr slow)))
      (rplacd! slow nil)
      (cons lst right)))))
  (merge
   (lambda (left right pred?)
    "Merge two sorted lists, LST1 and LST2, into a single sorted list according"
    "to the binary predicate PRED?."
    (cond
     ((nil? left)  right)
     ((nil? right) left)
     ((pred? (car left) (car right))
      (cons (car left) (merge (cdr left) right pred?)))
     (t
      (cons (car right) (merge left (cdr right) pred?)))))))
 (defun sort!!  (lst pred?)
  "Just a basic merge sort of LST by PRED?, destroying LST in the process and"
  "returning a new list."
  (unless (list? lst)  (error "LST must be a list"))
  (unless (fun? pred?) (error "PRED? must be a function"))
  (if (or (nil? lst) (nil? (cdr lst)))
   lst
   (let* ((splits (half lst))
          (left   (car splits))
          (right  (cdr splits)))
    (merge (sort!!  left pred?)
     (sort!!  right pred?) pred?)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'merge-sort)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list mem funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun memq? (elem lst)
 "Return non-nil if ELEM is an element of LST. Comparison done with 'eq?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((found nil))
  (while (and lst (not found))
   (if (eq? elem (car lst))
    (setq! found t)
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun memql? (elem lst)
 "Return non-nil if ELEM is an element of LST. Comparison done with 'eql?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((found nil))
  (while (and lst (not found))
   (if (eql? elem (car lst))
    (setq! found t)
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-member)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list index funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indexq (elem lst)
 "Return the zero-based index of the first occurrence of ELEM in LST, or nil if"
 "ELEM does not appear in the list. Comparison done with 'eq?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((idx 0)
       (found nil))
  (while (and lst (not found))
   (if (eq? elem (car lst))
    (setq! found idx)
    (setq! idx (+ 1 idx))
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indexql (elem lst)
 "Return the zero-based index of the first occurrence of ELEM in LST, or nil if"
 "ELEM does not appear in the list. Comparison done with 'eql?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((idx 0)
       (found nil))
  (while (and lst (not found))
   (if (eql? elem (car lst))
    (setq! found idx)
    (setq! idx (+ 1 idx))
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-index)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list remove funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeq (elem lst)
 "Non-destructively remove ELEM from LST. Comparison done with 'eq?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (unless (eq? elem (car lst))
    (setq! result (cons (car lst) result)))
   (setq! lst (cdr lst)))
  (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeql (elem lst)
 "Non-destructively remove ELEM from LST. Comparison done with 'eql?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (unless (eql? elem (car lst))
    (setq! result (cons (car lst) result)))
   (setq! lst (cdr lst)))
  (reverse result)))
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
          (next           (progn (setq! prev lst) (chase next)))
          (t              (error "OBJ was not found in LST")))))))
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
          (t               (error "OBJ was not found in LST")))))))
     (chase current))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-remove)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (unions):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun union2q (lst1 lst2)
 "Return the union of LST1 and LST2, using eq? to test for duplicates."
 (unless (list? lst1)      (error "LST1 must be a list"))
 (unless (list? lst2)      (error "LST2 must be a list"))
 (let* ((combine  (lambda (acc x) (if (memq? x acc) acc (cons x acc))))
        (union1   (reduce combine lst1 '())))
  (reduce combine lst2 union1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun union2ql (lst1 lst2)
 "Return the union of LST1 and LST2, using eql? to test for duplicates."
 (unless (list? lst1)      (error "LST1 must be a list"))
 (unless (list? lst2)      (error "LST2 must be a list"))
 (let* ((combine  (lambda (acc x) (if (memql? x acc) acc (cons x acc))))
        (union1   (reduce combine lst1 '())))
  (reduce combine lst2 union1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! unionq
 (reduced* union2q))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! unionql
 (reduced* union2ql))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'union)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equal? predicate:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun equal? (o1 o2)
 "True when O1 and O2 are eql? or cons trees whose atomic mebers are equal?."
 (cond
  ((and (atom? o1) (atom? o2)) (eql? o1 o2))
  ((and (cons? o1) (cons? o2))
   (and (equal? (car o1) (car o2))
    (equal? (cdr o1) (cdr o2))))
  (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; even?/odd? predicates:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun even?(n)
 "t if N is even."
 (unless (integer? n) (error "N must be an integer"))
 (zero? (% n 2 )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun odd?(n)
 "t if N is odd."
 (unless (integer? n) (error "N must be an integer"))
 (= 1 (% n 2 )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; always?/never? predicates:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun always?(x) "always true." t)
(defun never? (x) "never true."  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun single? (lst)
 "true if LST is a list with single item."
 (unless (cons? lst) (error "LST must be a cons"))
 (not (cdr lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun double? (lst)
 "true if LST is list with two items."
 (unless (cons? lst) (error "LST must be a cons"))
 (not (cddr lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun single? (lst)
 "true if LST is a list with three items."
 (unless (cons? lst) (error "LST must be a cons"))
 (not (cdddr lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manipulate predicates:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compose-pred1s preds
 "Does what it says on the tin and composes unary predicatess PREDS."
 (unless (all? fun? preds) (error "PREDS must be functions"))
 (lambda (val)
  (lets
   ((fun
     (lambda (preds)
      (cond
       ((nil? (car preds))       t)
       ((not  ((car preds) val)) nil)
       (t     (fun (cdr preds)))))))
   (fun preds))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; invert a predicate:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun invert-pred1 pred?
 "Does what it says on the tin and inverts a unary predicate PRED?."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (lambda (val)
  (not (pred? val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-predicates)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (unsorted):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-to-list (lst elem)
 "Add ELEM to the head of LST."
 (let ((new-tail (cons (car lst) (cdr lst))))
  (rplaca! lst elem)
  (rplacd! lst new-tail))
 lst)
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
 "Get the depth of a nested list structure."
 (unless (list? lst) (error "LST must be a list"))
 (let ((stack (list (cons lst 1))) ; Stack with initial list and depth of 1
       (max-depth 0))
  (while stack
   (let* ((current (pop! stack))
          (current-list (car current))
          (current-depth (cdr current)))
    (if (> current-depth max-depth)
     (setq! max-depth current-depth))
    (mapc (lambda (item)
           (when (list? item)
            (push! (cons item (1+ current-depth)) stack)))
     current-list)))
  max-depth))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter (pred? lst)
 "Return a list containing those members of lst satisfying pred?."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (if (pred? (car lst))
    (setq! result (cons (car lst) result)))
   (setq! lst (cdr lst)))
  (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intercalate (intercalated lst)
 "Intercalate INTERCALATED between items in LST."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while (cdr lst)
   (setq! result (cons (car lst) result))
   (setq! result (cons intercalated result))
   (setq! lst (cdr lst)))
  (if lst (setq! result (cons (car lst) result)))
  (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reverse (lst)
 "Return a new list that is the reverse of the input list LST."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (setq! result (cons (car lst) result))
   (setq! lst (cdr lst)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun butlast (lst)
 "Returns a new list that contains all the elements of the input list except the last one."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil)
       (prev nil))
  (while (cdr lst)
   (setq! prev (cons (car lst) prev))
   (setq! lst (cdr lst)))
  (while prev
   (setq! result (cons (car prev) result))
   (setq! prev (cdr prev)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-list (lst)
 "Take a shallow copy of LST."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (setq! result (cons (car lst) result))
   (setq! lst (cdr lst)))
  (reverse result)))
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
(provide 'misc-list-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; log toggle helpers, these should be replaced with macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun with-toggled-fun1 (toggled-fun)
 "Get a function that enables TOGGLED-FUN, evaluates FUN-OR-EXPR and sets"
 "TOGGLED-FUN back to it's prior state."
 (unless (fun? toggled-fun) (error "TOGGLED-FUN must be a function"))
 (lambda (fun-or-expr)
  (if (lambda? fun-or-expr)
   (let* ((old    (toggled-fun t))
          (result (fun-or-expr))
          (new    (toggled-fun old)))
    result)
   (let* ((old    (toggled-fun t))
          (result (eval fun-or-expr))
          (new    (toggled-fun old)))
    result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun with-toggled-fun (toggled-fun)
 "Get a function that enables TOGGLED-FUN, evaluates FUN-OR-EXPRS and sets"
 "TOGGLED-FUN back to it's prior state."
 (unless (fun? toggled-fun) (error "TOGGLED-FUN must be a function"))
 (lambda funs-or-exprs
  (last (mapcar (with-toggled-fun1 toggled-fun) funs-or-exprs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! with-log-eval  (with-toggled-fun log-eval))
(setq! with-log-core  (with-toggled-fun log-core))
(setq! with-log-macro (with-toggled-fun log-macro))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'log-toggle-helpers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy output macros:
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
(provide 'print-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun benchmark (repetitions print-interval qexpr)
 "Benchmark QEXPR by running it REPETITIONS times and returning the"
 "total/average time in ms, printing updates ever PRINT-INTERVAL iterations."

 "THIS PROBABLY NEEDS AN UPDATE!"
 (unless (integer? repetitions)   (error "REPETITIONS must be an integer"))
 (unless (integer? print-interval)(error "PRINT-INTERVAL must be an integer"))
 (nl)
 (let ((ctr   0)
       (total 0))
  (repeat repetitions
   (setq! ctr (1+ ctr))
   (let ((before (time)))
    (eval qexpr)
    (setq! total (+ total (elapsed before))))
   (when (zero? (% ctr print-interval))
    (nl)
    (princ "Iteration #")
    (princ ctr)
    (princ ", ")
    (princ (÷ total 1000))
    (princ " ms so far.")))
  (nl)
  (princ "total ums: ")
  (princ total) (nl)
  (princ "total ms: ")
  (princ (÷ total 1000)) (nl)
  (princ "total s: ")
  (princ (÷ total 1000000)) (nl)
  (let ((each-ms (÷ total repetitions 1000)))
   (princ "each ms: ")
   (princ (÷ total repetitions 1000))
   (nl)
   each-ms)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'benchmark)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random unsorted stuff:
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
(defun max2 (a b)
 "Return the larger of A and B."
 (unless (integer? a) (error "A must be an integer"))
 (unless (integer? b) (error "B must be an integer"))
 (if (> a b) a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun min2 (a b)
 "Return the smaller of A and B."
 (unless (integer? a) (error "A must be an integer"))
 (unless (integer? b) (error "B must be an integer"))
 (if (< a b) a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun min lst
 "Get the least element in LST."
 (let ((current-min (first lst)))
  (mapc
   (lambda (x) 
    (when (< x current-min) 
     (setq! current-min x)))
   lst)
  current-min))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max lst
 "Get the greatest element in LST."
 (let ((current-max (first lst)))
  (mapc
   (lambda (x) 
    (when (> x current-max) 
     (setq! current-max x)))
   lst)
  current-max))
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
(provide 'unsorted-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (vector-style list API):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-set! (lst index obj)
 "Destructively set the element at INDEX of LST to OBJ."
 (unless (list? lst) (error "LST must be a list"))
 (unless (and (integer? index) (>= index 0)) (error "INDEX must be a non-negative integer"))
 (let ((current-index 0)
       (done nil))
  (while (and lst (not done))
   (if (= current-index index) ; <- changed eq? to =
    (progn
     (rplaca! lst obj)
     (setq! done t))
    (setq! lst (cdr lst))
    (setq! current-index (+ 1 current-index))))
  (unless done (error "INDEX out of bounds")))
 obj) ; return the set object for convenience, similar to setq!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-ref (lst index)
 "Return the element at INDEX of LST."
 (unless (list? lst) (error "LST must be a list"))
 (unless (and (integer? index) (>= index 0)) (error "INDEX must be a non-negative integer"))
 (let ((current-index 0))
  (while (and lst (not (= current-index index)))
   (setq! lst (cdr lst))
   (setq! current-index (+ 1 current-index)))
  (if lst
   (car lst)
   (error "INDEX out of bounds"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! list-length length)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-list (size init-val)
 "Make a new list of length SIZE with its cars set to INIT-VAL."
 (unless (integer? size) (error "SIZE must be an integer"))
 (let ((result nil)
       (current-index 0))
  (while (< current-index size)
   (setq! result (cons init-val result))
   (setq! current-index (+ 1 current-index)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'vector-lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; split lists:
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
(provide 'split-list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delq:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delq! (item lst)
 "Destructively remove all items eq? to ITEM from LST."
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (while (and lst (eq? (car lst) item))
   (setq! lst (cdr lst)))
  (let ((ptr lst))
   (while (and ptr (cdr ptr))
    (if (eq? (cadr ptr) item)
     (rplacd! ptr (cddr ptr))
     (setq! ptr (cdr ptr)))))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delql! (item lst)
 "Destructively remove all items eql? to ITEM from LST."
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (while (and lst (eql? (car lst) item))
   (setq! lst (cdr lst)))
  (let ((ptr lst))
   (while (and ptr (cdr ptr))
    (if (eql? (cadr ptr) item)
     (rplacd! ptr (cddr ptr))
     (setq! ptr (cdr ptr)))))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'delq)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prime? (num)
 "Check if a number is prime."
 (unless (integer? num) (error "NUM must be an integer"))
 (if (or (= num 0) (= num 1))
  nil
  (let ((limit (/ num 2))
        (divisor 2)
        (is-prime t))
   (while (and (<= divisor limit) is-prime)
    (if (= (% num divisor) 0)
     (setq! is-prime nil)
     (setq! divisor (+ divisor 1))))
   is-prime)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun primes (n)
 "Return the first n prime numbers."
 (unless (integer? n) (error "N must be an integer"))
 (let ((count 0)
       (num 2)
       (primes '()))
  (while (< count n)
   (when (prime? num)
    (setq! count (+ count 1))
    (setq! primes (append2 primes (list num))))
   (setq! num (+ num 1)))
  primes))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'prime-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; selection sort parts;
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
(provide 'selection-sort-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; documentation funss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun doc (obj)
 "Get an object OBJ's documentation."
 (let* ((doc       (or (get :doc obj) "This object has no documentation."))
        (binding   (get :last-bound-to obj))
        (is-fun    (or (lambda? obj) (macro? obj)))
        (name      (or binding (string obj)))
        (params    (when is-fun
                    (string (cons name (params obj)))))
        (docstring (if is-fun
                    (concat params ": " doc)
                    (concat name   ": " doc))))
  docstring))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'doc-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test funs/macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore
 "An old version of require-equal that we are no longer using."

 "Signal an error unless VAL is equal? to TEST-VAL, otherwise return the result"
 "of evaluating EXPR."
 (defun require-equal (test-val val)
  (if (equal? test-val val)
   val
   (error "require-equal failed: " test-val " ≠ " val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro require-equal (test-val expr)
 "Signal an error unless EXPR evaluates to TEST-VAL, otherwise return the"
 "result of evaluating EXPR."
 $('let $($('val expr))
   $('if $('equal? test-val 'val)
     'val
     $('error
       $('concat
         '"require-equal failed: "
         $('string test-val)
         '" ≠ "
         $('string 'val))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! *confirm's-2nd-column* 72)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro confirm (that expr returns expected)
 "Test whether EXPR evaluates to EXPECTED."
 (unless (eq? 'that   that)     (error "expected 'that as 2nd argument"))
 (unless (eq? 'returns returns) (error "expected 'returns as 4th argument"))
 $('progn  ;; *confirm's-2nd-column*
   $('let* $(
             $('printed $('princ $('string $('quote expr))))
             $('column   'printed)
             )
     $('while $('< 'column *confirm's-2nd-column*)
       $('princ '" ")
       $('setq! 'column $('+ 1 'column)))
     $('princ '" ⇒ ")
     (when (cons? expected)
      $('princ "'"))
     $('write $('require-equal
                expected
                expr))
     $('nl))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'test-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string funss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-string (size init-val)
 "Make a new string of length SIZE with it's chars set to INIT-VAL."
 (unless (integer? size)         (error "SIZE must be an integer."))
 (unless (string? init-val)      (error "INIT-VAL must be a string."))
 (unless (= 1 (length init-val)) (error "INIT-VAL must be a string of length 1."))
 (apply concat (make-list size init-val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pad-string-right (size init-val str)
 "Pad STR to SIZE with INIT-VAL."
 (unless (integer? size)         (error "SIZE must be an integer."))
 (unless (string? init-val)      (error "INIT-VAL must be a string."))
 (unless (= 1 (length init-val)) (error "INIT-VAL must be a string of length 1."))
 (let ((pad (make-string (- size (length str)) init-val)))
  (concat str pad)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pad-string-left (size init-val str)
 "Pad STR to SIZE with INIT-VAL."
 (unless (integer? size)         (error "SIZE must be an integer."))
 (unless (string? init-val)      (error "INIT-VAL must be a string."))
 (unless (= 1 (length init-val)) (error "INIT-VAL must be a string of length 1."))
 (let ((pad (make-string (- size (length str)) init-val)))
  (concat pad str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun left-justify  (size str) (pad-string-right size " " str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun right-justify (size str) (pad-string-left size " " str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'string-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prog macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prog1 (expr1 . exprs)
 "Evaluate EXPR1, then evaluate EXPRS in order, and return the value of EXPR1."
 $('let $($('result $('eval expr1)))
   $('progn . exprs)
   'result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prog2 (expr1 expr2 . exprs)
 "Evaluate EXPR1, then evaluate EXPR2, then evaluate EXPRS in order, and return the value of"
 "EXPR2."
 $('progn
   expr1
   $('let $($('result2 $('eval expr2)))
     $('progn . exprs)
     'result2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'prog-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plist funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-keys (plist)
 "Extracts the keys from a plist PLIST."
 (unless (list? plist)          (error "PLIST must be a list"))
 (when plist (cons (car plist) (plist-keys (cddr plist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-values (plist)
 "Extracts the values from a plist PLIST."
 (when plist (plist-keys (cdr plist))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-remove (pred? prop plist)
 (unless (list? plist)          (error "PLIST must be a list"))
 (unless (even? (length plist)) (error "PLIST must have an even number of elements"))
 (when plist
  (if (pred? prop (car plist))
   (plist-remove pred? prop (cddr plist))
   (cons (car plist) (cons (cadr plist) (plist-remove pred? prop (cddr plist)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-removeq  (prop plist) (plist-remove eq?  prop plist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-removeql (prop plist) (plist-remove eql? prop plist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! keys      plist-keys)
(setq! vals-base vals)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vals args
 (when (cdr args) (error "VALS takes only one argument"))
 (let ((arg (car args)))
  (cond
   ((nil? arg)  (vals-base (env (env (env)))))
   ((list? arg) (plist-values arg))
   ((env? arg)  (vals-base arg))
   (t           (error "VALS takes a plist or an environment")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'plist-funs)
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
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rational math funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if *use-soft-rationals*
 (progn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun rational (n d)
   "Construct a rational number from numerator N and denominator D."
   (unless (integer? n) (error "N must be an integer."))
   (unless (integer? d) (error "D must be an integer."))
   (cons n d))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (defun integer-to-rational (n)
   "Construct a rational number from integer N."
   (unless (integer? n) (error "N must be an integer."))
   (cons n 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun numer (num)
   "Get NUM's numerator"
   (unless (number? num) (error "NUM must be a number"))
   (if (integer? num)
    num
    (car num)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun denom (num)
   "Get NUM's denominator"
   (unless (number? num) (error "NUM must be a number"))
   (if (integer? num)
    1
    (cdr num)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun rational? (obj)
   "t if OBJ is a rational number."
   (and (cons? obj) (integer? (car obj)) (integer? (cdr obj)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "When (not *use-soft-rationals*), integer-to-rational can just be id since the"
  "built-in numer and denom functions already handle integers correctly."
 (setq! integer-to-rational id))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun number? (obj)
 "t if OBJ is a number."
 (or (integer? obj) (rational? obj)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gcd (a b)
 "Get the greatest common divisor of A and B."
 (unless (and (integer? a) (integer? b))
  (error "gcd's arguments must be integers"))
 (if (zero? b) a (gcd b (mod a b))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simplify-number (number)
 "Simplify a rational number NUMBER."
 (unless (number? number) (error "NUMBER must be a number"))
 (if (integer? number)
  number
  (let* ((num (numer number))
         (den (denom number))
         (common-divisor (gcd num den)))
   (if (zero? den) (error "Denominator is 0, something has gone awry"))
   (let ((rat (rational (div num common-divisor) (div den common-divisor))))
    (if (one? (denom rat))
     (numer rat)
     rat)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-rational (a b)
 "Add the number B to the number A."
 (unless (number? a) (error "A must be a number"))
 (unless (number? b) (error "B must be a number")) 
 (if (integer? a) (setq! a (integer-to-rational a)))
 (if (integer? b) (setq! b (integer-to-rational b)))
 (let* ((num (add (mul (numer a) (denom b)) (mul (numer b) (denom a))))
        (den (mul (denom a) (denom b))))
  (simplify-number (rational num den))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sub-rational (a b)
 "Subtract the number B from the number A."
 (unless (number? a) (error "A must be a number"))
 (unless (number? b) (error "B must be a number")) 
 (if (integer? a) (setq! a (integer-to-rational a)))
 (if (integer? b) (setq! b (integer-to-rational b)))
 (let* ((num (sub (mul (numer a) (denom b)) (mul (numer b) (denom a))))
        (den (mul (denom a) (denom b))))
  (simplify-number (rational num den))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mul-rational (a b)
 "Multiply the number A by the number B."
 (unless (number? a) (error "A must be a number"))
 (unless (number? b) (error "B must be a number")) 
 (if (integer? a) (setq! a (integer-to-rational a)))
 (if (integer? b) (setq! b (integer-to-rational b)))
 (let* ((num (mul (numer a) (numer b)))
        (den (mul (denom a) (denom b))))
  (simplify-number (rational num den))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun div-rational (a b)
 "Divide the number A by the number B."
 (unless (number? a) (error "A must be a number"))
 (unless (number? b) (error "B must be a number")) 
 (if (integer? a) (setq! a (integer-to-rational a)))
 (if (integer? b) (setq! b (integer-to-rational b)))
 (let* ((num (mul (numer a) (denom b)))
        (den (mul (denom a) (numer b))))
  (unless (zero? den)
   (simplify-number (rational num den)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! addd add-rational)
(setq! subr sub-rational)
(setq! mulr mul-rational)
(setq! divr div-rational)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'rational-math)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos scheme compat aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! #f            nil)
(setq! #t            t)
(setq! ???           'unspecified-result)
(setq! assoc         ahas?) 
(setq! assq          aget) 
(setq! collect-if    filter)
(setq! define        setq!)
(setq! display       write)
(setq! every         all?)
(setq! getl          pget)
(setq! gsort         sort!!)
(setq! make-vector   make-list)
(setq! map           mapcar)
(setq! map-append    mapcan)
(setq! position-of   indexq)
(setq! remove        removeq)
(setq! set!          setq!) ;should should be a macro that avoids re-defining what-scheme-implementation
(setq! vector-length list-length)
(setq! vector-ref    list-ref)
(setq! vector-set!   list-set!)
(setq! null?         nil?)  
(setq! pair?         cons?) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'scheme-compat-aliases)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp compat aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! nreverse         reverse)
(setq! put              put!)
(setq! setq             setq!)
(setq! rplaca           rplaca!)
(setq! rplacd           rplacd!)
(setq! nconc            nconc!)
(setq! null             nil?)
(setq! identity         id)
(setq! expand-file-name expand-path)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'elisp-compat-aliases)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! premove   plist-remove)
(setq! premoveq  plist-removeq)
(setq! premoveql plist-removeql)
(setq! ljust     left-justify)
(setq! rjust     right-justify)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'std-aliases)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new content that hasn't been sorted of merged into split std's modules yet:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sys* args (sys (reduce concat (intercalate " " (mapcar string (flatten args))))))
(setq! stdout (curry1 kget :stdout))
(setq! stderr (curry1 kget :stderr))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun not-nil? (obj) (not (nil? obj)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun !nil? (obj) (not (nil? obj)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! compact (curry1 filter not-nil?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun expand-file-name (name . rest)
 "Return the absolute file name of NAME, optionally relative to DEFAULT-DIRECTORY."
 (unless (string? name)
  (error "NAME must be a string"))
 (unless (or (nil? rest) (nil? (cdr rest)))
  (error "If present, REST must contain a single element"))
 (if (nil? rest)
  (expand-path name)
  (let ((default-directory (car rest)))
   (unless (string? default-directory)
    (error "If present, the first element of REST must be a string"))
   (expand-path (concat default-directory "/" name)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun round-up-to-nearest-multiple (num multiple)
 "Round NUM up to the nearest multiple of MULTIPLE."
 (unless (integer? num) (error "NUM must be an integer"))
 (unless (integer? multiple) (error "MULTIPLE must be an integer"))
 (unless (> multiple 0) (error "MULTIPLE must be greater than zero"))
 (unless (> num 0) (error "NUM must be greater than zero"))
 (* multiple (+ 1 (/ num multiple))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mono-std)
(provide 'std) ;; this counts as an implementation of 'std.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *log-loading-std-enabled*
 (nl)
 (princ "Loaded in   ")
 (princ (elapsed-us *time-before-loading-std*))
 (princ " us.")
 (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
