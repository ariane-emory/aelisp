;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *log-loading-std-enabled*
 (princ "Loading monolithic std..."))

(require 'std-fundamental)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some basic funs/macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ignore args
 "Ignores ARGS and do nothing."
 nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro funcall (fun . args)
 "Apply FUN to ARGS. This only exists to make porting code from other Lisps easier."
 (unless (fun? (eval fun)) (error "FUN must be a function"))
 (cons fun args))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun root-env ()
 "Get the root environment."
 (let* ((current (env))
        (parent  (env current)))
  (while parent
   (setq! current parent)
   (setq! parent (env current)))
  current))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defconstant (sym value)
 "Set SYM to VALUE and mark it as constant."
 (unless (symbol? sym) (error "SYM must be a symbol"))
 $('progn
   $('setq! sym value)
   $('put $('quote sym) ':constant 't)
   value))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun id (o) o)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'basic-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! s          setq!)
(setq! setcar!    rplaca!)
(setq! setcdr!    rplacd!)
(setq! ¬          not)          
(setq! ∨          or )           
(setq! ∧          and)
(setq! setcdr!    rplacd!)
(setq! setcar!    rplaca!)
(setq! λ          lambda)
;; (setq! has-key?   khas?)
;; (setq! get-key    kget)
;; (setq! set-key    kset)
(setq! phas? plist-has?)
(setq! pset! plist-set!)
(setq! pget  plist-get)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-aliases)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound car/cdrs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'compound-cars-and-cdrs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (append/nconc variants):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun append2 (lst1 lst2)
  "Append LST1 and LST2 by copying LST1 and connecting it to LST2."
  (unless (list? lst1) (error "LST1 must be a list"))
  (unless (list? lst2) (error "LST2 must be a list"))
  (if (nil? lst1)
      lst2
      (let* ((result (list (car lst1)))
             (tail result))
        (setq! lst1 (cdr lst1))
        (while lst1
          (let ((new-cons (list (car lst1))))
            (rplacd! tail new-cons)
            (setq! tail new-cons))
          (setq! lst1 (cdr lst1)))
        (rplacd! tail lst2) 
        result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun append lists
 "Append any number of LISTS."
 (let (result tail)
  (while lists
   (let ((current-list (car lists)))
    (unless (list? current-list) (error "Every argument must be a list"))
    (while current-list
     (let ((new-cons (list (car current-list))))
      (if (nil? result)
       (setq! result new-cons)
       (rplacd! tail new-cons))
      (setq! tail new-cons)
      (setq! current-list (cdr current-list))))
    (setq! lists (cdr lists))))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nconc2! (lst1 lst2)
 "Destructively join LST1 an LST2."
 (unless (list? lst1) (error "LST1 must be a list"))
 (unless (list? lst2) (error "LST2 must be a list"))
 (if (nil? lst1)
  lst2
  (rplacd! (last lst1) lst2)
  lst1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nconc! lists
 "Destructively concatenate multiple lists."
 (let ((result     (car lists))
       (rest-lists (cdr lists)))
  (while rest-lists
   (setq! result     (nconc2! result (car rest-lists)))
   (setq! rest-lists (cdr rest-lists)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'append-and-nconc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quasiquotation:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'append-and-nconc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! quasiquote expand-quasiquoted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'quasiquote)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (retrieving by position):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun nthcdr (index lst)
;;  "Get the INDEXth cdr of LST."
;;  (unless (integer? index) (error "N must be an integer"))
;;  (unless (list? lst)      (error "LST must be a list"))
;;  (unless (>= index 0)     (error "INDEX must be non-negative"))
;;  (if (zero? index)
;;   lst
;;   (nthcdr (1- index) (cdr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nthcdr (index lst)
 "Get the INDEXth cdr of LST."
 (unless (and (integer? index) (positive? index)) (error "N must be a positive integer"))
 (unless (list? lst)                              (error "LST must be a list"))
 (unless (>= index 0)                             (error "INDEX must be non-negative"))
 (until (zero? index)
  (setq! lst   (cdr lst))
  (setq! index (- index 1)))
 lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun nth (index lst)
;;  "Get the nth item in LST."
;;  (unless (integer? index) (error "INDEX must be an integer"))
;;  (unless (list? lst)      (error "LST must be a list"))
;;  (unless (>= index 0)     (error "INDEX must be non-negative"))
;;  (let ((len (length lst)))
;;   (if (>= index len)
;;    (error "INDEX is out of range")
;;    (cond
;;     ((zero? index) (car lst))
;;     (lst           (nth (- index 1) (cdr lst)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nth (index lst)
 "Get the nth item in LST."
 (car (nthcdr lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun last (lst)
;;  "Get last item in a LST."
;;  (unless (list? lst) (error "LST must be a list"))
;;  (cond
;;   ((nil? (cdr lst)) lst)
;;   (lst              (last (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun last (lst)
 "Get last cons cell in a LST."
 (unless (list? lst) (error "LST must be a list"))
  (while (cdr lst)
   (setq! lst (cdr lst)))
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-access-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (tail chaser macros):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'tail-chaser-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (reduction):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reduced (fun . init-val)
 "Return a function that is a left reduction of the binary function FUN."
 (unless (fun? fun)   (error "FUN must be a function"))
 (when (cdr init-val) (error "INIT-VAL must be a single object"))
 (lambda (lst) (reduce fun lst . init-val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reduced* (fun . init-val)
 "Return a function that is a left reduction of the binary function FUN."
 "which takes loose args."
 (unless (fun? fun)   (error "FUN must be a function"))
 (when (cdr init-val) (error "INIT-VAL must be a single object"))
 (lambda args ((reduced fun . init-val) args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rreduced (fun . init-val)
 "Return a function that is a right reduction of the binary function FUN."
 (unless (fun? fun)   (error "FUN must be a function"))
 (when (cdr init-val) (error "INIT-VAL must be a single object"))
 (lambda (lst) (rreduce fun lst . init-val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rreduced* (fun . init-val)
 "Return a function that is a right reduction of the binary function FUN."
 "which takes loose args."
 (unless (fun? fun)   (error "FUN must be a function"))
 (when (cdr init-val) (error "INIT-VAL must be a single object"))
 (lambda args ((rreduced fun . init-val) args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'reduce)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (map variants):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun mapcar (fun lst)
;;  "Map fun over LST, returning the resulting list."
;;  (unless (fun? fun)  (error "FUN must be a function"))
;;  (unless (list? lst) (error "LST must be a list"))
;;  (let ((acc nil))
;;   (while lst
;;    (setq! acc (cons (fun (car lst)) acc))
;;    (setq! lst (cdr lst)))
;;   (reverse acc)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar (fun lst)
 "Map FUN over LST, returning the resulting list."
 (unless (fun?  fun) (error "FUN must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (let* ((result (list (fun (car lst))))
         (tail   result)
         (lst     (cdr lst)))
   (while lst
    (let ((new-cons (list (fun (car lst)))))
     (rplacd! tail new-cons)
     (setq! tail new-cons))
    (setq! lst (cdr lst)))
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar* (fun . args) (apply mapcar fun (list args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar! (fun lst)
 "Map fun over LST, altering the list."
 (unless (fun? fun)  (error "FUN must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (let ((current lst))
  (while current
   (rplaca! current (fun (car current)))
   (setq! current (cdr current)))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapconcat (fun lst . rest)
 "Map fun over LST, returning the result of concatenating the resulting strings."
 (unless (fun? fun)  (error "FUN must be a function"))
 (unless (list? lst) (error "LST must be a list"))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun mapcan (fun lst)
;;  "Map fun over LST and concatenate the results by altering them."
;;  (unless (fun? fun)  (error "FUN must be a function"))
;;  (unless (list? lst) (error "LST must be a list"))
;;  (let ((result nil)
;;        (tail nil)
;;        (current lst))
;;   (while current
;;    (let ((fun-result (fun (car current))))
;;     (when fun-result
;;      (if result
;;       (progn
;;        (setq! tail (nconc2! tail fun-result))
;;        (setq! tail (last tail)))
;;       (setq! result fun-result)
;;       (setq! tail result))))
;;    (setq! current (cdr current)))
;;   result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcan (fun lst)
 "Map fun over LST and concatenate the results by altering them."
 (unless (fun? fun)  (error "FUN must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (let (result
       tail
       (current lst))
  (while current
   (let ((fun-result (fun (car current))))
    (when fun-result
     (if tail
      (progn
       (rplacd! tail fun-result)
       (setq!   tail (last tail)))
      (setq! result fun-result)
      (setq! tail   (last result)))))
   (setq! current (cdr current)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapc (fun lst)
 "Apply FUN to each element of LST for side effects only and return LST."
 (unless (fun? fun)  (error "FUN must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (let ((current lst))
  (while current
   (fun (car current))
   (setq! current (cdr current)))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapc* (fun . args) (apply mapc fun (list args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (flattening):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten1 (lst)
 (unless (list? lst) (error "LST must be a list"))
 (cond
  ((nil? lst) nil)
  ((list? (car lst))
   (append (car lst) (flatten1 (cdr lst))))
  (t (cons (car lst) (flatten1 (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten-left (lst)
 "Flatten a left-nested list structure LST."
 (unless (list? lst) (error "LST must be a list"))
 (if (cons? (car lst))i)
 (append (flatten-left (car lst)) $(cadr lst))
 lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (lst)
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (if (cons? (car lst))
   (append (flatten (car lst)) (flatten (cdr lst)))
   (cons (car lst) (flatten (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'flatten)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (zipping):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zip2 (lst1 lst2)
 "Zip LST1 and LST2."
 (unless (list? lst1) (error "LST1 must be a list"))
 (unless (list? lst2) (error "LST2 must be a list"))
 (cond
  ((∨ (nil? lst1) (nil? lst2)) nil)
  (t  (cons  $((car lst1) (car lst2))
       (zip2 (cdr lst1) (cdr lst2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zip3 (l1 l2 l3)
 "Zip the three lists LST1, LST2 and LST3."
 (unless (list? l1) (error "LST1 must be a list"))
 (unless (list? l2) (error "LST2 must be a list"))
 (unless (list? l3) (error "LST3 must be a list"))
 (mapcar flatten1 (reduce zip2 $(l2 l3) l1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! left-nested-zip (reduced* zip2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro zip lists
 "Zip many lists. This might not flatten properly if the zipped elements are"
 "themselves lists."
 (unless (list? lists) (error "LISTS must be a list"))
 (if (cdr lists)
  $('mapcar 'flatten (cons 'left-nested-zip lists))
  $('mapcar 'list  (car lists))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'zip)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (push/push-back):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun push-back (lst elem) 
 "Non-destructively push ELEM onto the tail of LST."
 (unless (list? lst) (error "LST must be a list"))
 (append lst (cons elem nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pop (elem lst)
 "Non-destructively pop ELEM from the head of LST."
 "The only difference from car is that an empty LST may not be popped."
 (unless (cons? lst) (error "LST must be a list"))
 (car lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun push (elem lst)
 "Non-destructively push ELEM onto the head of LST, aka cons."
 (unless (list? lst) (error "LST must be a list"))
 (cons elem lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'push-pop-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (transform):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform! (pred? fun obj)
 "Destructively transform the cons tree OBJ by replacing members matching
  PRED? with the result of applying FUN to them."
 (when (not (fun? pred?)) (error "PRED? must be a lambda function"))
 (when (not (fun? fun))   (error "FUN must be a lambda function"))
 (when (atom? obj)        (error "OBJ must be a non-empty list"))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform (pred? fun obj)
 "Transform OBJ by replacing members matching PRED? with the result of
  applying FUN to them or, if obj is not a cons tree, by applying FUN to
  OBJ."
 (when (not (fun? pred?)) (error "PRED? must be a lambda function"))
 (when (not (fun? fun))   (error "FUN must be a lambda function"))
 (cond
  ;; ((and (atom? obj) (pred? obj)) (fun obj))
  ((pred? obj) (fun obj))
  ((atom? obj) obj)
  (t
   (cons
    (transform pred? fun (car obj))
    (transform pred? fun (cdr obj))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform (pred? fun tree)
 "Replace items matching PRED? in TREE with the result of applying FUN to them."
 (unless (list? tree) (error "TREE must be a list"))
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (fun? fun)   (error "FUN must be a function"))
 (when tree
  (let* (result tail)
   (while tree
    (let* ((head (car tree))
           (new-tail
            (list
             (cond
              ((cons? head) (transform pred? fun head))
              ((pred? head) (fun head))
              (else head)))))
     (if result
      (rplacd! tail new-tail)
      (setq! result new-tail))
     (setq! tail new-tail)
     (setq! tree (cdr tree))))
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefetch (expr)
 "Try to optimize EXPR by replacing it's symbol? members with the result of
  looking them up. This is, mysteriously, not a very effective optimization."
 (transform!
  (lambda (x) (and (symbol? x) (bound? x)))
  (lambda (x) (eval x))
  expr))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subst (tree this that . rest)
 "Substitute occurence of THIS for THAT in TREE."
 (unless (list? tree) (error "TREE must be a list"))
 (unless (or (fun? (car rest)) (nil? (car rest)))
  (error "If provided, EQP?? must be a function"))
 (when (cdr rest)
  (error "subst accepts only one optional argument"))
 (when tree
  (let* ((eqp?? (or (car rest) eql?))
         result
         tail)
   (while tree
    (let* ((head (car tree))
           (new-tail
            (list
             (cond
              ((cons? head) (subst head this that eqp??))
              ((eqp?? this head) that)
              (else head)))))
     (if result
      (rplacd! tail new-tail)
      (setq! result new-tail))
     (setq! tail new-tail)
     (setq! tree (cdr tree))))
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform (pred? fun tree)
 "Replace items matching PRED? in TREE with the result of applying FUN to them."
 (unless (list? tree) (error "TREE must be a list"))
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (fun? fun)   (error "FUN must be a function"))
 (when tree
  (let* (result tail)
   (while tree
    (let* ((head (car tree))
           (new-tail
            (list
             (cond
              ((cons? head) (transform pred? fun head))
              ((pred? head) (fun head))
              (else head)))))
     (if result
      (rplacd! tail new-tail)
      (setq! result new-tail))
     (setq! tail new-tail)
     (setq! tree (cdr tree))))
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'transform)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (sorting):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'merge-sort)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list mem funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun memq? (elem lst)
 "Return non-nil if ELEM is an element of LST. Comparison done with 'eq?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((found nil))
  (while (and lst (not found))
   (if (eq? elem (car lst))
    (setq! found lst)
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun memql? (elem lst)
 "Return non-nil if ELEM is an element of LST. Comparison done with 'eql?'."
 (unless (list? lst) (error "LST must be a list"))
 (let ((found nil))
  (while (and lst (not found))
   (if (eql? elem (car lst))
    (setq! found lst)
    (setq! lst (cdr lst))))
  found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-member)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list index funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-index)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list remove funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun removeq (elem lst)
;;  "Non-destructively remove ELEM from LST. Comparison done with 'eq?'."
;;  (unless (list? lst) (error "LST must be a list"))
;;  (let ((result nil))
;;   (while lst
;;    (unless (eq? elem (car lst))
;;     (setq! result (cons (car lst) result)))
;;    (setq! lst (cdr lst)))
;;   (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeq (elem lst)
 "Non-destructively remove ELEM from LST. Comparison done with 'eq?'."
 (unless (list? lst) (error "LST must be a list"))
 (let (result tail)
  (while lst
   (unless (eq? elem (car lst))
    (let ((new-cons (list (car lst))))
     (if tail
      (progn
       (rplacd! tail new-cons)
       (setq!   tail new-cons))
      (setq! result new-cons)
      (setq! tail   result))))
   (setq! lst (cdr lst)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun removeql (elem lst)
;;  "Non-destructively remove ELEM from LST. Comparison done with 'eql?'."
;;  (unless (list? lst) (error "LST must be a list"))
;;  (let ((result nil))
;;   (while lst
;;    (unless (eq? elem (car lst))
;;     (setq! result (cons (car lst) result)))
;;    (setq! lst (cdr lst)))
;;   (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeql (elem lst)
 "Non-destructively remove ELEM from LST. Comparison done with 'eql?'."
 (unless (list? lst) (error "LST must be a list"))
 (let (result tail)
  (while lst
   (unless (eql? elem (car lst))
    (let ((new-cons (list (car lst))))
     (if tail
      (progn
       (rplacd! tail new-cons)
       (setq!   tail new-cons))
      (setq! result new-cons)
      (setq! tail   result))))
   (setq! lst (cdr lst)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun removeq! (elem lst)
;;  "Destructively remove the first item eq? to ELEM from LST."
;;  (unless (list? lst) (error "LST must be a list"))
;;  (let ((head (car lst))
;;        (tail (cdr lst)))
;;   (if (eq? elem head)
;;    (if (nil? tail)
;;     (error "can't remove last item from LST")
;;     (rplaca! lst (second lst))
;;     (rplacd! lst (cddr   lst)))
;;    (let ((prev     lst)
;;          (current  (cdr lst)))
;;     (letrec
;;      ((chase
;;        (lambda (lst)
;;         (let ((head (car lst))
;;               (next (cdr lst)))
;;          (cond
;;           ((eq? elem head) (progn (rplacd! prev next) elem))
;;           (next           (progn (setq! prev lst) (chase next)))
;;           (t              (error "ELEM was not found in LST")))))))
;;      (chase current))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeq! (elem lst)
 "Destructively remove the first item eq? to ELEM from LST."
 (unless (list? lst) (error "LST must be a list"))
 (if (eq? elem (car lst))
  (if (nil? (cdr lst))
   (error "can't remove last item from LST")
   (rplaca! lst (cadr lst))
   (rplacd! lst (cddr lst)))
  (let ((prev lst)
        (current (cdr lst)))
   (while (and current (not (eq? elem (car current))))
    (setq! prev current)
    (setq! current (cdr current)))
   (if current
    (rplacd! prev (cdr current))
    (error "ELEM was not found in LST")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun removeql! (elem lst)
;;  "Destructively remove the first item eql? to ELEM from LST."
;;  (unless (list? lst) (error "LST must be a list"))
;;  (let ((head (car lst))
;;        (tail (cdr lst)))
;;   (if (eql? elem head)
;;    (if (nil? tail)
;;     (error "can't remove last item from LST")
;;     (rplaca! lst (cadr lst))
;;     (rplacd! lst (cddr lst)))
;;    (let ((prev     lst)
;;          (current  (cdr lst)))
;;     (letrec
;;      ((chase
;;        (lambda (lst)
;;         (let ((head (car lst))
;;               (next (cdr lst)))
;;          (cond
;;           ((eql? elem head) (progn (rplacd! prev next) elem))
;;           (next            (progn (setq! prev lst) (chase next)))
;;           (t               (error "ELEM was not found in LST")))))))
;;      (chase current))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun removeql! (elem lst)
 "Destructively remove the first item eql? to ELEM from LST."
 (unless (list? lst) (error "LST must be a list"))
 (if (eql? elem (car lst))
  (if (nil? (cdr lst))
   (error "can't remove last item from LST")
   (rplaca! lst (cadr lst))
   (rplacd! lst (cddr lst)))
  (let ((prev lst)
        (current (cdr lst)))
   (while (and current (not (eql? elem (car current))))
    (setq! prev current)
    (setq! current (cdr current)))
   (if current
    (rplacd! prev (cdr current))
    (error "ELEM was not found in LST")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'list-remove)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (unions):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun union2q (lst1 lst2)
 "Return the union of LST1 and LST2, using eq? to test for duplicates."
 (unless (list? lst1)      (error "LST1 must be a list"))
 (unless (list? lst2)      (error "LST2 must be a list"))
 (let* ((combine  (lambda (acc x) (if (memq? x acc) acc (cons x acc))))
        (union1   (reduce combine lst1 '())))
  (reduce combine lst2 union1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun union2ql (lst1 lst2)
 "Return the union of LST1 and LST2, using eql? to test for duplicates."
 (unless (list? lst1)      (error "LST1 must be a list"))
 (unless (list? lst2)      (error "LST2 must be a list"))
 (let* ((combine  (lambda (acc x) (if (memql? x acc) acc (cons x acc))))
        (union1   (reduce combine lst1 '())))
  (reduce combine lst2 union1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! unionq
 (reduced* union2q))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! unionql
 (reduced* union2ql))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'union)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc predicates:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun equal? (o1 o2)
 "True when O1 and O2 are eql? or cons trees whose atomic mebers are equal?."
 (cond
  ((and (atom? o1) (atom? o2)) (eql? o1 o2))
  ((and (cons? o1) (cons? o2))
   (and (equal? (car o1) (car o2))
    (equal? (cdr o1) (cdr o2))))
  (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; always?/never? predicates:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun always? (x) "always true." t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun never?  (x) "never true."  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun single? (lst)
 "true if LST is a list with single item."
 (unless (list? lst) (error "LST must be a cons"))
 (and (cons? lst) (not (cdr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun double? (lst)
 "true if LST is list with two items."
 (unless (list? lst) (error "LST must be a cons"))
 (and (cons? lst) (cdr lst) (not (cddr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun triple? (lst)
 "true if LST is a list with three items."
 (unless (list? lst) (error "LST must be a cons"))
 (and (cons? lst) (cddr lst) (not (cdddr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manipulate predicates:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compose-pred1s preds
 "Does what it says on the tin and composes unary predicatess PREDS."
 (unless (all? fun? preds) (error "PREDS must be functions"))
 (lambda (val)
  (letrec
   ((fun
     (lambda (preds)
      (cond
       ((nil? (car preds))       t)
       ((not  ((car preds) val)) nil)
       (t     (fun (cdr preds)))))))
   (fun preds))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; invert a predicate:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun invert-pred1 pred?
 "Does what it says on the tin and inverts a unary predicate PRED?."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (lambda (val)
  (not (pred? val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-predicates)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (unsorted):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-to-list (lst elem)
 "Add ELEM to the head of LST."
 (let ((new-tail (cons (car lst) (cdr lst))))
  (rplaca! lst elem)
  (rplacd! lst new-tail))
 lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list* args
 "Come up with a docstring for this!"
 (let*
  ((chase
	  (lambda (args)
		 (cond
      ((nil? args)       nil)
		  ((nil? (cdr args)) (car args))
		  (t                 (cons (car args) (chase (cdr args))))))))
  (chase args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun filter (pred? lst)
;;  "Return a list containing those members of lst satisfying pred?."
;;  (unless (fun? pred?) (error "PRED? must be a function"))
;;  (unless (list? lst)  (error "LST must be a list"))
;;  (let (result)
;;   (while lst
;;    (if (pred? (car lst))
;;     (setq! result (cons (car lst) result)))
;;    (setq! lst (cdr lst)))
;;   (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter (pred? lst)
 "Return a list containing those members of lst satisfying pred?."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let (result tail)
  (while lst
   (if (pred? (car lst))
    (let ((new-cons (list (car lst))))
     (if tail
      (progn
       (rplacd! tail new-cons)
       (setq!   tail new-cons))
      (setq! result new-cons)
      (setq! tail   result))))
   (setq! lst (cdr lst)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun intercalate (intercalated lst)
;;  "Intercalate INTERCALATED between items in LST."
;;  (unless (list? lst) (error "LST must be a list"))
;;  (let (result)
;;   (while (cdr lst)
;;    (setq! result (cons (car lst) result))
;;    (setq! result (cons intercalated result))
;;    (setq! lst (cdr lst)))
;;   (if lst (setq! result (cons (car lst) result)))
;;   (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intercalate (intercalated lst)
 "Intercalate INTERCALATED between items in LST."
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (let* ((result (list (car lst)))
         (tail result))
   (setq! lst (cdr lst))
   (while lst
    (let ((new-cons (list intercalated (car lst))))
     (rplacd! tail new-cons)
     (setq! tail (cdr new-cons)))
    (setq! lst (cdr lst)))
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reverse (lst)
 "Return a new list that is the reverse of the input list LST."
 (unless (list? lst) (error "LST must be a list"))
 (let (result)
  (while lst
   (setq! result (cons (car lst) result))
   (setq! lst (cdr lst)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun butlast (lst . rest)
 "Returns a new list that contains all the elements of the input list except the last one."
 (unless (list? lst)      (error "LST must be a list"))
 (unless (not (cdr rest)) (error "butlast takes one or zero arguments"))
 (unless (or (nil? (car rest)) (and (integer? (car rest)) (positive? (car rest))))
  (error "If provided, butlast's argument must be a positive integer"))
 (let ((n (or (car rest) 1)))
  (when (and lst (nthcdr n lst))
   (let* ((result (list (car lst)))
          (tail   result))
    (setq! lst (cdr lst))
    (while (nthcdr n lst)
     (let ((new-cons (list (car lst))))
      (rplacd! tail new-cons)
      (setq!   tail new-cons))
     (setq! lst (cdr lst)))
    result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-list (lst)
 "Take a shallow copy of LST."
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (let* ((result (list (car lst)))
         (tail result))
   (setq! lst (cdr lst))
   (while lst
    (let ((new-cons (list (car lst))))
     (rplacd! tail new-cons)
     (setq!   tail new-cons))
    (setq! lst (cdr lst)))
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun-list-pred-fun (name combiner base-case)
 `(defun ,name (pred? lst)
   (if lst
    (,combiner
     (pred? (car lst))
     (,name pred? (cdr lst)))
    ,base-case)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-list-pred-fun any? or  nil)
(defun-list-pred-fun all? and t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all? (pred? lst)
 "t when all elems in LST? are PRED?"
 (unless (fun? pred?) (error "PRED? must be a function"))
 (while (and lst (pred? (car lst)))
  (setq! lst (cdr lst)))
 (nil? lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun any? (pred? lst)
 "t when any elem in LST? is PRED?."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (let (result)
  (while (and lst (not result))
   (setq! result (pred? (car lst)))
   (setq! lst (cdr lst)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun-list-transform-fun (name transformer)
 `(defun ,name (lsts)
   (when lsts
    (cons (,transformer (car lsts)) (,name (cdr lsts))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-list-transform-fun heads caar)
(defun-list-transform-fun tails cdar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-list-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; log toggle helpers, these should be replaced with macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun with-toggled-fun (toggled-fun)
 "Get a function that enables TOGGLED-FUN, evaluates FUN-OR-EXPRS and sets"
 "TOGGLED-FUN back to it's prior state."
 (unless (fun? toggled-fun) (error "TOGGLED-FUN must be a function"))
 (lambda funs-or-exprs
  (last (mapcar (with-toggled-fun1 toggled-fun) funs-or-exprs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! with-log-eval  (with-toggled-fun log-eval))
(setq! with-log-core  (with-toggled-fun log-core))
(setq! with-log-macro (with-toggled-fun log-macro))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'log-toggle-helpers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy output macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro princn  exprs           $('progn (cons 'princ exprs) $('nl)))
(defmacro printn  exprs           $('progn (cons 'print exprs) $('nl)))
(defmacro putn    exprs           $('progn (cons 'put  exprs)  $('nl)))
(defmacro writen  exprs           $('progn (cons 'write exprs) $('nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro princni (delim . exprs) $('apply 'princn $('intercalate delim $('quote exprs))))
(defmacro printi  (delim . exprs) $('apply 'print  $('intercalate delim $('quote exprs))))
(defmacro putni   (delim . exprs) $('apply 'putn   $('intercalate delim $('quote exprs))))
(defmacro writeni (delim . exprs) $('apply 'writen $('intercalate delim $('quote exprs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro princns exprs           $('princni " " . exprs))
(defmacro prints  exprs           $('princti " " . exprs))
(defmacro putns   exprs           $('putni   " " . exprs))
(defmacro writens exprs           $('writeni " " . exprs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'print-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun benchmark (repetitions print-interval qexpr)
 "Benchmark QEXPR by running it REPETITIONS times and returning the"
 "total/average time in ms, printing updates ever PRINT-INTERVAL iterations."

 "THIS PROBABLY NEEDS AN UPDATE!"
 (unless (and (integer? repetitions)    (positive? repetitions))
  (error "REPETITIONS must be a positive integer"))
 (unless (and (integer? print-interval) (positive? print-interval))
  (error "PRINT-INTERVAL must be a positive integer"))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'benchmark)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random unsorted stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun apply* (fun . args)
 "Try to remember how this one works and document it."
 (unless (fun? fun) (error "FUN must be a function"))
 (apply fun (apply list* args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun curry1 (fun arg1)
 "Curry the first argument of FUN as ARG1. This would be better if it were a macro."
 (unless (fun? fun) (error "FUN must be a function"))
 (lambda args
  (apply fun arg1 args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun spc ()
 "Print a space character."
 (princ " "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-logging-to (fun)
 "Add logging to a function FUN."
 (unless (fun? fun) (error "FUN must be a function"))
 (if (has? fun :added-logging)
  (error "logging was already added to this fun")
  (let* ((fun-name      (get fun :last-bound-to))
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
  (put fun :added-logging t) (nl)
  fun))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max-delta (lst)
 "Find the maximum delte between elements in a list of integers LST."
 (unless (cons? lst)         (error "LST must not be empty."))
 (unless (all? integer? lst) (error "LST's elements must be integers."))
 (let ((min-val (car lst))
       (max-val (car lst)))
  (while lst
   (let ((current (car lst)))
    (setq! min-val (min min-val current))
    (setq! max-val (max max-val current)))
   (setq! lst (cdr lst)))
  (- max-val min-val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(setq! bin-list-to-int  (reduced  (lambda (acc bin) (+ (<< acc 1) bin))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! bin-list-to-int* (reduced* (lambda (acc bin) (+ (<< acc 1) bin))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'unsorted-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (vector-style list API):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-set! (lst index obj)
 "Destructively set the element at INDEX of LST to OBJ."
 (unless (list? lst) (error "LST must be a list"))
 (unless (and (integer? index) (positive? index)) (error "INDEX must be a positive integer"))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-ref (lst index)
 "Return the element at INDEX of LST."
 (unless (list? lst) (error "LST must be a list"))
 (unless (and (integer? index) (positive? index)) (error "INDEX must be a positive integer"))
 (let ((current-index 0))
  (while (and lst (not (= current-index index)))
   (setq! lst (cdr lst))
   (setq! current-index (+ 1 current-index)))
  (if lst
   (car lst)
   (error "INDEX out of bounds"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! list-length length)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-list (size init-val)
 "Make a new list of length SIZE with its cars set to INIT-VAL."
 (unless (and (integer? size) (positive? size)) (error "SIZE must be a positive integer"))
 (let (result
       (current-size 0))
  (until (= current-size size)
   (setq! result (cons init-val result))
   (setq! current-size (+ 1 current-size)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'vector-lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; split lists:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun split-list-alternate! (pred? lst)
;;  "Destructively split the LST into two sublists:"
;;  "1. The longest initial sublist of elements satisfying PRED?"
;;  "2. The rest of the elements."
;;  (unless (fun? pred?) (error "PRED? must be a function"))
;;  (unless (list? lst) (error "LST must be a list"))
;;  (let ((front nil)
;;        (back lst))
;;   (while (and back (pred? (car back)))
;;    (push! (pop! back) front))
;;   $((reverse front) back)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-list (pred? lst)
 "Destructivly split LST into two sublists:"
 "1. The longest initial sublist of elements satisfying PRED?"
 "2. The rest of the elements."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (let (prev
       (current lst))
  (while (and current (pred? (car current)))
   (setq! prev current)
   (setq! current (cdr current)))
  (if prev
   (progn
    (rplacd! prev nil)
    $(lst current))
   $(nil lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun split-list (pred? lst)
;;  "Split LST into two sublists:"
;;  "1. The longest initial sublist of elements satisfying PRED?"
;;  "2. The rest of the elements."
;;  (unless (fun? pred?) (error "PRED? must be a function"))
;;  (unless (list? lst) (error "LST must be a list"))
;;  (let ((front nil)
;;        (current lst))
;;   (while (and current (pred? (car current)))
;;    (setq front (cons (car current) front))
;;    (setq current (cdr current)))
;;   $((reverse front) current)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'split-list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delq:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delq! (item lst)
 "Destructively remove all items eq? to ITEM from LST."
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (while (and lst (eq? (car lst) item))
   (setq! lst (cdr lst)))
  (let ((pos lst))
   (while (and pos (cdr pos))
    (if (eq? (cadr pos) item)
     (rplacd! pos (cddr pos))
     (setq! pos (cdr pos)))))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delql! (item lst)
 "Destructively remove all items eql? to ITEM from LST."
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (while (and lst (eql? (car lst) item))
   (setq! lst (cdr lst)))
  (let ((pos lst))
   (while (and pos (cdr pos))
    (if (eql? (cadr pos) item)
     (rplacd! pos (cddr pos))
     (setq! pos (cdr pos)))))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'delq)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prime? (num)
 "Check if a number is prime."
 (unless (and (integer? num) (positive? num)) (error "NUM must be a positive integer"))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun primes (n)
 "Return the first N prime numbers."
 (unless (and (integer? n) (positive? n)) (error "N must be a positive integer"))
 (let ((count 0)
       (num 2)
       (primes '()))
  (while (< count n)
   (when (prime? num)
    (setq! count (+ count 1))
    (setq! primes (append2 primes (list num))))
   (setq! num (+ num 1)))
  primes))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'prime-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; selection sort parts;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'selection-sort-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; documentation funss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'doc-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test funs/macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore
 "An old version of require-equal that we are no longer using."

 "Signal an error unless VAL is equal? to TEST-VAL, otherwise return the result"
 "of evaluating EXPR."
 (defun require-equal (test-val val)
  (if (equal? test-val val)
   val
   (error "require-equal failed: " test-val " ≠ " val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro require-equal (test-val expr)
 "Signal an error unless EXPR evaluates to TEST-VAL, otherwise return the"
 "result of evaluating EXPR."
 $('let $($('val expr))
   $('if $('equal? test-val 'val)
     'val
     $('error
       $('concat
         '"require-equal failed: expected "
         $('string test-val)
         '" ≠ actual "
         $('string 'val))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! *confirm's-2nd-column* 72)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'test-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string funss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-string (size init-val)
 "Make a new string of consisting of SIZE repetitions of INIT-VAL."
 (unless (and (integer? size) (positive? size)) (error "SIZE must be a positive integer"))
 (unless (string? init-val)                     (error "INIT-VAL must be a string."))
 (let ((result "")
       (current-size 0))
  (until (= current-size size)
   (setq! result (concat result init-val))
   (setq! current-size (+ 1 current-size)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pad-string-right (size init-val str)
 "Pad STR to SIZE with INIT-VAL."
 (unless (and (integer? size) (positive? size)) (error "SIZE must be a positive integer"))
 (unless (string? init-val)                     (error "INIT-VAL must be a string."))
 (unless (one? (length init-val))               (error "INIT-VAL must be a string of length 1."))
 (concat str (make-string (- size (length str)) init-val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pad-string-left (size init-val str)
 "Pad STR to SIZE with INIT-VAL."
 (unless (integer? size)          (error "SIZE must be an integer."))
 (unless (string? init-val)       (error "INIT-VAL must be a string."))
 (unless (one? (length init-val)) (error "INIT-VAL must be a string of length 1."))
 (concat (make-string (- size (length str)) init-val) str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun left-justify  (size str)
 "Left justify a string STR."
 (unless (and (integer? size) (positive? size)) (error "SIZE must be a positive integer"))
 (unless (string? str) (error "STR must be a string."))
 (pad-string-right size " " str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun right-justify (size str)
 "Right justify a string STR."
 (unless (and (integer? size) (positive? size)) (error "SIZE must be a positive integer"))
 (unless (string? str) (error "STR must be a string."))
 (pad-string-left size " " str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun concat* args
 "Convert ARGS into strings and concatenate them."
 (apply concat (mapcar string args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! ljust left-justify)
(setq! rjust right-justify)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'string-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prog macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prog1 (expr1 . exprs)
 "Evaluate EXPR1, then evaluate EXPRS in order, and return the value of EXPR1."
 $('let $($('result $('eval expr1)))
   $('progn . exprs)
   'result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prog2 (expr1 expr2 . exprs)
 "Evaluate EXPR1, then evaluate EXPR2, then evaluate EXPRS in order, and return the value of"
 "EXPR2."
 $('progn
   expr1
   $('let $($('result2 $('eval expr2)))
     $('progn . exprs)
     'result2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'prog-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plist funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun plist-remove! (key plist)
;;  "Destructively remove the first occurrence of key PROP from plist PLIST."
;;  (unless (list? plist) (error "PLIST must be a list"))
;;  ;; Handle the case when the key is at the head of plist.
;;  (if (eql? key (car plist))
;;   (progn
;;     ;; Replace the head of the plist with the next key-value pair or nil.
;;     (rplaca! plist (caddr plist))
;;     (rplacd! plist (if (caddr plist) (cdddr plist) (list nil))) ;; special case for removing last element
;;     plist)
;;   ;; If the key is not at the head, iterate through the plist.
;;   (let ((prev plist)
;;         (current (cdr plist)))
;;    ;; Continue until there are no more pairs.
;;    (while current
;;     (if (eql? key (car current))
;;      (progn
;;       ;; Skip the key-value pair by modifying the cdr of the previous pair.
;;       (rplacd! prev (cddr current))
;;       ;; Exit the loop as we've removed the key-value pair.
;;       (setq current nil))
;;      ;; Update pointers to move to the next key-value pair.
;;      (setq prev current)
;;      (setq current (cdr current))))
;;    ;; Return the possibly modified plist.
;;    plist)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun plist-remove (prop plist)
;;  "Non-destructively remove key PROP from plist PLIST."
;;  (unless (list? plist) (error "PLIST must be a list"))
;;  (let* ((head (cons nil nil))  ; Dummy head
;;         (tail head))
;;   (while plist
;;    (unless (eql? prop (car plist))
;;     (rplacd! tail (cons (car plist) (cons (cadr plist) nil)))  ; Append current pair
;;     (setq! tail (cddr tail)))  ; Move tail pointer forward
;;    (setq! plist (cddr plist)))  ; Skip to the next pair
;;   (cdr head)))  ; Return the list after the dummy head.b
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun plist-set (prop plist value)
;;  "Non-destructively set key PROP in plist PLIST to VALUE."
;;  (unless (list? plist) (error "PLIST must be a list"))
;;  (let* ((head (cons nil nil))  ; Dummy head
;;         (tail head)
;;         (found nil))
;;   (while plist
;;    (if (eql? prop (car plist))
;;     (progn
;;      (setq! found t)
;;      (rplacd! tail (cons prop (cons value (cddr plist))))  ; Set new pair and link rest.
;;      (setq! plist nil))  ; End the loop.
;;     (progn
;;      (rplacd! tail (cons (car plist) (cons (cadr plist) nil)))  ; Append current pair.
;;      (setq! tail (cddr tail))  ; Move tail pointer forward.
;;      (setq! plist (cddr plist)))))  ; Move through the plist.
;;   (unless found
;;    (rplacd! tail (cons prop (cons value nil))))  ; Append the prop-value pair if not found.
;;   (cdr head)))  ; Return the list after the dummy head.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun make-plist (keys vals)
;;  "Build a plist from KEYS and VALS."
;;  (unless (list? keys) (error "KEYS must be a list."))
;;  (unless (list? vals) (error "VALS must be a list."))
;;  (unless (>= (length keys) (length vals)) (error "KEYS must be at least as long as VALS."))
;;  (let (plist
;;        (rkeys (reverse keys))
;;        (rvals (reverse vals)))
;;   (while (not (= (length rkeys) (length rvals)))
;;    (setq! rvals (cons nil rvals)))
;;   (while rkeys
;;    (if plist
;;     (plist-set! plist (car rkeys) (car rvals))
;;     (setq! plist (plist-set plist (car rkeys) (car rvals))))
;;    (setq! rkeys (cdr rkeys))
;;    (setq! rvals (cdr rvals)))
;;   plist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-plist (keys vals)
 "Build a plist from KEYS and VALS."
 (unless (list? keys) (error "KEYS must be a list."))
 (unless (list? vals) (error "VALS must be a list."))
 (unless (>= (length keys) (length vals)) (error "KEYS must be at least as long as VALS."))
 (when keys
  (let* ((result (list (car keys) (car vals)))
         (tail (cdr result)))
   (setq! keys (cdr keys))
   (setq! vals (cdr vals))
   (while keys
    (let ((new-tail (list (car keys) (car vals))))
     (rplacd! tail new-tail)
     (setq!   tail (cdr new-tail))
     (setq! keys (cdr keys))
     (setq! vals (cdr vals))))
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun plist-to-alist (plist)
;;  "Convert a plist PLIST to an alist. If the number of elements in plist is odd, the last"
;;  "cons in the resulting alist's value cell will be nil."
;;  (unless (list? plist)          (error "PLIST must be a list"))
;;  (when plist
;;   (let (alist (plist plist))
;;    (while plist
;;     (setq! alist (cons (cons (car plist) (cadr plist)) alist))
;;     (setq! plist (cddr plist)))
;;    (reverse alist))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-to-alist (plist)
 "Convert a plist PLIST to an alist. If the number of elements in plist is odd, the last"
 "cons in the resulting alist's value cell will be nil."
 (unless (list? plist)          (error "PLIST must be a list"))
 (when plist
  (let* ((result (list (cons (car plist) (cadr plist))))
         (tail   result)
         (plist  (cddr plist)))
   (while plist
    (let ((new-alist-item (list (cons (car plist) (cadr plist)))))
     (rplacd! tail new-alist-item)
     (setq!   tail new-alist-item))
    (setq! plist (cddr plist)))
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun alist-to-plist (alist)
;;  "Convert an alist ALIST to a plist."
;;  (unless (list? alist)          (error "ALIST must be a list"))
;;  (let (plist (alist alist))
;;   (while alist
;;    (setq! plist (cons (car alist) (cons (cadr alist) plist)))
;;    (setq! alist (cddr alist)))
;;   plist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-to-plist (alist)
 "Convert an alist ALIST to a plist."
 (unless (list? alist)          (error "ALIST must be a list"))
 (let (result tail)
  (while alist
   (let* ((pair (car alist))
          (key  (car pair))
          (value (cdr pair)))
    (if tail
     (progn
      (rplacd! tail (cons key (cons value nil)))
      (setq!   tail (cdr tail))
      (setq!   tail (cdr tail)))
     (progn
      (setq! result (cons key (cons value nil)))
      (setq! tail   result)
      (setq! tail   (cdr tail)))))
   (setq! alist (cdr alist)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun plist-keys (plist)
;;  "Extracts the keys from a plist PLIST."
;;  (unless (list? plist)          (error "PLIST must be a list"))
;;  (when plist (cons (car plist) (plist-keys (cddr plist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-keys (plist)
 "Extracts the keys from a plist PLIST."
 (unless (list? plist) (error "PLIST must be a list"))
 (when plist
  (let* ((result (list (car plist)))
         (tail result))
   (setq! plist (cddr plist))
   (while plist
    (let ((new-cons (list (car plist))))
     (rplacd! tail new-cons)
     (setq!   tail new-cons))
    (setq! plist (cddr plist)))
   result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-vals (plist)
 "Extracts the values from a plist PLIST."
 (unless (list? plist)          (error "PLIST must be a list"))
 (unless (even? (length plist)) (error "PLIST must have an even number of elements"))
 (plist-keys (cdr plist)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! keys      plist-keys)
(setq! vals-base vals)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vals args
 "Retrieve the values from a plist or environment."
 (when (cdr args) (error "vals takes one or zero arguments"))
 (let ((arg (car args)))
  (cond
   ((nil? arg)  (vals-base (env (env (env)))))
   ((list? arg) (plist-vals arg))
   ((env? arg)  (vals-base arg))
   (t           (error "vals takes a plist or an environment")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'plist-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; remove property macro:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro remove (obj prop)
;;  "Remove a property PROP from OBJ."
;;  $('prog1
;;    $('quote $('get obj prop))
;;    $('props! obj $('plist-remove $('props obj) prop))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (provide 'remove-prop-macro)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic integer math functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! *u8-max* (- (<<  8) 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u8  (n)
 "Trunctate N to an 8 bit value."
 (unless (integer? n) (error "N must be an integer"))
 (& n *u8-max*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! *u16-max* (- (<< 16) 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u16 (n)
 "Trunctate N to a 16 bit value."
 (unless (integer? n) (error "N must be an integer"))
 (& n *u16-max*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! *u32-max* (- (<< 32) 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u32 (n)
 "Trunctate N to a 32 bit value."
 (unless (integer? n) (error "N must be an integer"))
 (& n *u32-max*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun even? (n)
 "t if N is even."
 (unless (integer? n) (error "N must be an integer"))
 (zero? (% n 2 )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun odd? (n)
 "t if N is odd."
 (unless (integer? n) (error "N must be an integer"))
 (one? (% n 2 )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max2 (a b)
 "Return the larger of A and B."
 (unless (integer? a) (error "A must be an integer"))
 (unless (integer? b) (error "B must be an integer"))
 (if (> a b) a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun min2 (a b)
 "Return the smaller of A and B."
 (unless (integer? a) (error "A must be an integer"))
 (unless (integer? b) (error "B must be an integer"))
 (if (< a b) a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun min lst
 "Get the least element in LST."
 (unless (all? integer? lst) (error "LST must contain only integers"))
 (let ((current-min (first lst)))
  (mapc
   (lambda (x) 
    (when (< x current-min) 
     (setq! current-min x)))
   lst)
  current-min))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max lst
 "Get the greatest element in LST."
 (unless (all? integer? lst) (error "LST must contain only integers"))
 (let ((current-max (first lst)))
  (mapc
   (lambda (x) 
    (when (> x current-max) 
     (setq! current-max x)))
   lst)
  current-max))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun 1+ (n)
;;  "Return N + 1."
;;  (unless (integer? n) (error "N must be an integer"))
;;  (+ 1 n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun 1- (n)
;;  "Return N - 1."
;;  (unless (integer? n) (error "N must be an integer"))
;;  (- n 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun double (n)
 "Return N * 2."
 (unless (integer? n) (error "N must be an integer"))
 (<< n 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abs (n)
 "Return the absolute value of N."
 (unless (integer? n) (error "N must be an integer."))
 (if (> n 0) n (- n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! 2* double)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sum (lst)
 "Get the sum of the numbers in LST."
 (unless (all? integer? lst) (error "LST must contain only integers"))
 (let ((total 0))
  (while lst
   (unless (number? (car lst))
    (error "The elements of LST must be numbers."))
   (setq! total (+ total (car lst)))
   (setq! lst (cdr lst)))
  total))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gcd (a b)
 "Get the greatest common divisor of A and B."
 (unless (and (integer? a) (integer? b))
  (error "gcd's arguments must be integers"))
 (if (zero? b) a (gcd b (mod a b))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'int-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rational math funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if *use-soft-rationals*
 (progn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun rational (n d)
   "Construct a rational number with numerator N and denominator D."
   (unless (integer? n)                     (error "N must be an integer."))
   (unless (and (integer? d) (positive? d)) (error "D must be a positive integer."))
   (cons n d))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (defun integer-to-rational (n)
   "Construct a rational number from integer N."
   (unless (integer? n) (error "N must be an integer."))
   (cons n 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun numer (num)
   "Get NUM's numerator"
   (unless (number? num) (error "NUM must be a number"))
   (if (integer? num)
    num
    (car num)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun denom (num)
   "Get NUM's denominator"
   (unless (number? num) (error "NUM must be a number"))
   (if (integer? num)
    1
    (cdr num)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun rational? (obj)
   "t if OBJ is a rational number."
   (and (cons? obj) (integer? (car obj)) (integer? (cdr obj))))) ;; end when *use-soft-rationals*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 "When (not *use-soft-rationals*), integer-to-rational can just be id since the"
 "built-in numer and denom functions already handle integers correctly."
 (setq! integer-to-rational id))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun number? (obj)
 "t if OBJ is a number."
 (or (integer? obj) (rational? obj)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simplify-number (number)
 "Simplify a rational number NUMBER."
 (unless (number? number) (error "NUMBER must be a number"))
 (if (integer? number)
  number
  (let* ((num (numer number))
         (den (denom number))
         (common-divisor (gcd num den)))
   (if (zero? den) (error "Denominator is 0, something has gone awry"))
   (let ((rat (rational (/ num common-divisor) (/ den common-divisor))))
    (if (one? (denom rat))
     (numer rat)
     rat)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-rational (a b)
 "Add the number B to the number A."
 (unless (number? a) (error "A must be a number"))
 (unless (number? b) (error "B must be a number")) 
 (if (integer? a) (setq! a (integer-to-rational a)))
 (if (integer? b) (setq! b (integer-to-rational b)))
 (let* ((num (+ (* (numer a) (denom b)) (* (numer b) (denom a))))
        (den (* (denom a) (denom b))))
  (simplify-number (rational num den))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sub-rational (a b)
 "Subtract the number B from the number A."
 (unless (number? a) (error "A must be a number"))
 (unless (number? b) (error "B must be a number")) 
 (if (integer? a) (setq! a (integer-to-rational a)))
 (if (integer? b) (setq! b (integer-to-rational b)))
 (let* ((num (- (* (numer a) (denom b)) (* (numer b) (denom a))))
        (den (* (denom a) (denom b))))
  (simplify-number (rational num den))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mul-rational (a b)
 "Multiply the number A by the number B."
 (unless (number? a) (error "A must be a number"))
 (unless (number? b) (error "B must be a number")) 
 (if (integer? a) (setq! a (integer-to-rational a)))
 (if (integer? b) (setq! b (integer-to-rational b)))
 (let* ((num (* (numer a) (numer b)))
        (den (* (denom a) (denom b))))
  (simplify-number (rational num den))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun div-rational (a b)
 "Divide the number A by the number B."
 (unless (number? a) (error "A must be a number"))
 (unless (number? b) (error "B must be a number")) 
 (if (integer? a) (setq! a (integer-to-rational a)))
 (if (integer? b) (setq! b (integer-to-rational b)))
 (let* ((num (* (numer a) (denom b)))
        (den (* (denom a) (numer b))))
  (when (zero? den) (error "Division by zero!"))
  (simplify-number (rational num den))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rational-less-than? (a b)
 "t when rational A is less than rational B."
 (unless (rational? a) (error "A must be a rational."))
 (unless (rational? b) (error "B must be a rational."))
 (let* ((cross1 (* (car a) (cdr b)))
        (cross2 (* (cdr a) (car b))))
  (< cross1 cross2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun approx-sqrt (num . rest)
 "Calculate the approximate square root of NUM."
 (unless (integer? num) (error "NUM must be an integer"))
 (unless (or (nil? (car rest)) (integer? (car rest)))
  (error "If present, first REST arg must be an integer"))
 (unless (or (nil? (cadr rest)) (integer? (cadr rest)))
  (error "If present, second REST arg must be an integer"))
 (when (caddr rest)
  (error "approx-sqrt takes no more than three arguments"))
 ;; Initial guess
 (let ((denominator-limit (or (car rest) (<< 24)))
       (max-iterations    (or (cadr rest) 1000))
       (guess (integer-to-rational num))
       (last-guess (integer-to-rational 0))
       (iterations 0)
       (continue t))  ;; continue flag
  ;; Iterative method
  (while (and continue 
          (not (eql? guess last-guess))
          (<= iterations max-iterations))
   (setq! last-guess guess)
   (setq! guess (div-rational (add-rational guess (div-rational (integer-to-rational num) guess)) (integer-to-rational 2)))
   ;; Check if the denominator is too large
   (if (> (denom guess) denominator-limit)
    (setq! continue nil))
   (setq! iterations (+ 1 iterations)))  
  guess))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-square? (num)
 "t when NUM is a square number."
 (unless (integer? num) (error "NUM must be an integer"))
 (let ((approx (round (approx-sqrt num))))
  (= (* approx approx) num)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun round-up-to-square (num)
 "Round NUM up to the next square number."
 (unless (integer? num) (error "NUM must be an integer"))
 (while (not (is-square? num))
  (setq! num (1+ num)))
 num)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun floor (n)
;;  "Round N down to the nearest integral value."
;;  (unless (number? n) (error "N must be a number."))
;;  (if (integer? n)
;;   n
;;   (let ((quotient  (/ (numer n) (denom n)))
;;         (remainder (% (numer n) (denom n))))
;;    (if (and (< (numer n) 0) (not (zero? remainder)))
;;     (- quotient 1)
;;     quotient))))
(defun floor (n)
 "Round N down to the nearest integral value."
 (unless (number? n) (error "N must be a number."))
 (if (integer? n)
  n
  (let ((quotient  (/ (numer n) (denom n)))
        (remainder (% (numer n) (denom n))))
   (if (and (< (numer n) 0) (!= 0 remainder))
    (- quotient 1)
    quotient))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun floor (n)
;;  "Round N down to the nearest integral value. This is probably more complex than it needs"
;;  "to be and could just be replaced with (div num den)..."
;;  (unless (number? n) (error "N must be a number."))
;;  (let ((num (numer n))
;;        (den (denom n)))
;;   (while (not (zero? (mod num den)))
;;    (setq! num (- num 1)))
;;   (simplify-number (rational num den))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun round (num)
 "If the number NUM is a rational number, round it to the nearest integer."
 "Otherwise, just return it."
 (unless (number? num) (error "NUM must be a number"))
 (if (integer? num)
  num
  (/ (+ (numer num) (>> (denom num) 1)) (denom num))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redefinitions of integer functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abs (n)
 "Return the absolute value of N."
 (unless (number? n) (error "N must be a number."))
 (if (integer? n)
  (if (> n 0) n (- n))
  (let ((num (numer n)))
   (simplify-number (rational (if (> num 0) num (- num)) (denom n))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases:h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! addr add-rational)
(setq! subr sub-rational)
(setq! mulr mul-rational)
(setq! divr div-rational)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'rational-math)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new content that hasn't been sorted of merged into split std's modules yet:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sys* args
 "A splat version of sys that flattens and stringifies ARGS."
 (sys (reduce concat (intercalate " " (mapcar string (flatten args))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! stdout    (curry1 pget :stdout))
(setq! stderr    (curry1 pget :stderr))
(setq! exit-code (curry1 pget :exit))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun not-nil? (o)
 "t when O."
 (not (nil? o)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! compact (curry1 filter not-nil?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun round-up-to-nearest-multiple (num multiple-of)
 "Round NUM up to the nearest multiple of MULTIPLE-OF."
 (unless (integer? num)         (error "NUM must be an integer"))
 (unless (integer? multiple-of) (error "MULTIPLE-OF must be an integer"))
 (unless (> multiple-of 0)      (error "MULTIPLE-OF must be greater than zero"))
 (unless (> num 0)              (error "NUM must be greater than zero"))
 (let ((remainder (% num multiple-of)))
  (if (zero? remainder)
   num
   (+ num (- multiple-of remainder)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random number generator functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uint64_t xorshift64(struct xorshift64_state *state)
;; {
;; 	uint64_t x = state->a;
;; 	x ^= x << 13;
;; 	x ^= x >> 7;
;; 	x ^= x << 17;
;; 	return state->a = x;
;; }
(setq! *xorshift64-seed* (now-us))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xorshift64 ()
 "Generate a pseudo-random positive integer."
 (when (zero? *xorshift64-seed*) (setq! *xorshift64-seed* (now-us)))  
 (setq! *xorshift64-seed* (^ *xorshift64-seed* (<< *xorshift64-seed* 13)))
 (setq! *xorshift64-seed* (^ *xorshift64-seed* (>> *xorshift64-seed* 7)))
 (setq! *xorshift64-seed* (^ *xorshift64-seed* (<< *xorshift64-seed* 17)))
 (abs *xorshift64-seed*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun random args
 "Return a psuedo-random integer between MIN and MAX inclusive."
 (unless (or (nil? args) (nil? (cddr args)))
  (error "random takes either 0, 1 or 2 arguments"))
 (unless (or (nil? (car args)) (integer? (car args)))
  (error "If provided, first argument must be an integer"))
 (unless (or (nil? (cdr args)) (integer? (cadr args)))
  (error "If provided, second argument must be an integer"))
 (let ((randval (xorshift64)))
  (if args
   (let* ((arg1 (if (cadr args) (car  args) 0))
          (arg2 (if (cadr args) (cadr args) (car args)))
          (min (min arg1 arg2))
          (max (max arg1 arg2))
          (range (+ 1 (- max min))))
    (+ min (mod (xorshift64) range)))
   randval)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'random)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matrix-related functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix? (obj)
 "t when OBJ is a matrix (list of lists)."
 (and (list? obj) (all? list? obj)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-transpose (matrix)
 "Convert the rows of MATRIX into columns."
 (unless (rectangular-matrix? matrix) (error "MATRIX must be a rectangular matrix (all rows must have the same number of columns)."))
 (when (car matrix)
  (cons (mapcar car matrix) (matrix-transpose (mapcar cdr matrix)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-reverse-rows (matrix)
 "Reverse each row in MATRIX."
 (unless (matrix? matrix) (error "MATRIX must be a list of lists"))
 (mapcar reverse matrix))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rectangular-matrix? (matrix)
 "t if the rows in MATRIX all have the same length."
 (unless (matrix? matrix) (error "MATRIX must be a list of lists"))
 (let ((first-row-length (length (car matrix))))
  (all?
   (lambda (row)
    (unless (cons? matrix) (error "MATRIX's rows must all be non-empty lists"))
    (= (length row) first-row-length))
   (cdr matrix))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-rotate-right (matrix)
 "Rotate MATRIX right by 90 degrees."
 (unless (matrix? matrix) (error "MATRIX must be a list of lists"))
 (unless (rectangular-matrix? matrix)
  (error "MATRIX is not rectangular. All rows must have the same number of columns."))
 (matrix-reverse-rows (matrix-transpose matrix)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-set! (matrix row col value)
 "Destructively set the element at ROW and COL of MATRIX to VALUE."
 (unless (matrix? matrix)                  (error "MATRIX must be a list of lists"))
 (unless (and (integer? row) (>= row 0))   (error "ROW must be a non-negative integer"))
 (unless (and (integer? col) (>= col 0))   (error "COL must be a non-negative integer"))
 ;;(princ "Set row " row " col " col " to " value) (nl)
 (let ((target-row (list-ref matrix row)))
  (list-set! target-row col value)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-ref (matrix row col)
 "Retrieve the element at ROW and COL in MATRIX."
 (unless (matrix? matrix)                  (error "MATRIX must be a list of lists"))
 (unless (and (integer? row) (>= row 0))   (error "ROW must be a non-negative integer"))
 (unless (and (integer? col) (>= col 0))   (error "COL must be a non-negative integer"))
 (let ((target-row (list-ref matrix row)))
  (unless target-row                       (error "ROW index out of bounds"))
  (list-ref target-row col)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-matrix (matrix)
 "Display a matrix MATRIX by applying write to each row."
 (unless (matrix? matrix)                  (error "MATRIX must be a list of lists"))
 (let ((row-count (length matrix))
       (current-row 0))
  (while (< current-row row-count)
   (write (list-ref matrix current-row))
   (nl)
   (setq! current-row (+ 1 current-row)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-matrix (rows cols init-val)
 "Create a new matrix of size ROWS x COLS with all values set to INIT-VAL."
 (unless (and (integer? rows) (integer? cols) (positive? rows) (positive? cols))
  (error "Both ROWS and COLS must be positive integers"))
 (let (result
       (current-row 0))
  (until (= current-row rows)
   (setq! result (cons (make-list cols init-val) result))
   (setq! current-row (+ 1 current-row)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;s
(defun matrix-transform (matrix rows cols ternary-func)
 "Modify each cell of the MATRIX using the TERNARY-FUNC."
 "TERNARY-FUNC takes three arguments: row, column, and current value of the cell."
 "The resulting value of TERNARY-FUNC is then set to the corresponding cell in the matrix."
 (unless (matrix? matrix)
  (error "MATRIX must be a list of lists"))
 (unless (and (integer? rows) (integer? cols) (positive? rows) (positive? cols))
  (error "Both ROWS and COLS must be positive integers"))
 (let ((current-row 0)
       (current-col 0))
  (until (= current-row rows)
   (setq! current-col 0)
   (until (= current-col cols)
    (let* ((current-value (matrix-ref matrix current-row current-col))
           (new-value (ternary-func current-row current-col current-value)))
     ;; (princ "Setting row " current-row " column " current-col " to " new-value) (nl)
     (matrix-set! matrix current-row current-col new-value)
     (setq! current-col (+ current-col 1))))
   (setq! current-row (+ current-row 1))))
 matrix)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'matrix)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; struct-related macros/funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-struct-getter (struct-type slot)
 "Generate a getter function for STRUCT-TYPE's slot SLOT."
 (unless (symbol? struct-type) (error "STRUCT-TYPE must be a symbol"))
 (unless (symbol? slot)        (error "SLOT must be a symbol"))
 (let ((getter-name (intern (concat (symbol-name struct-type) "-" (symbol-name slot))))
       (slot-kw (intern (concat ":" (symbol-name slot)))))
  $('defun getter-name $('obj)
    $('unless $('eq? $('get 'obj ':struct-type) $('quote struct-type))
      $('error (concat "OBJ must be a struct of type " (symbol-name struct-type))))
    $('plist-get slot-kw 'obj))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-struct-setter (struct-type slot)
 "Generate a setter function for STRUCT-TYPE's slot SLOT."
 (unless (symbol? struct-type) (error "STRUCT-TYPE must be a symbol"))
 (unless (symbol? slot)        (error "SLOT must be a symbol"))
 (let ((setter-name (intern (concat "set-" (symbol-name struct-type) "-" (symbol-name slot))))
       (slot-kw (intern (concat ":" (symbol-name slot)))))
  $('defun setter-name $('obj 'val)
    $('unless $('eq? $('get 'obj ':struct-type) $('quote struct-type))
      $('error (concat "OBJ must be a struct of type " (symbol-name struct-type))))
    $('plist-set slot-kw 'obj 'val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-struct-constructor (struct-type . slots)
 "Generate a constructor function for STRUCT-TYPE with SLOTS."
 (unless (symbol? struct-type) (error "STRUCT-TYPE must be a symbol"))
 (unless (all? symbol? slots)  (error "SLOTS must be a list of symbols"))
 (let ((constructor-name (intern (concat "make-" (symbol-name struct-type))))
       (slot-kws (mapcar (lambda (slot) (intern (concat ":" (symbol-name slot)))) slots)))
  $('defun constructor-name 'slot-values
    $('let $($('struct $('make-plist (cons 'list slot-kws) 'slot-values)))
      $('put 'struct ':struct-type $('quote struct-type))
      'struct))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-struct-predicate (struct-type)
 "Generate a predicate function for STRUCT-TYPE."
 (unless (symbol? struct-type) (error "STRUCT-TYPE must be a symbol"))
 (let ((predicate-name (intern (concat (symbol-name struct-type) "?"))))
  $('defun predicate-name $('obj)
    $('eq? $('get 'obj ':struct-type) $('quote struct-type)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun struct? (obj)
 "t when OBJ is a struct."
 (and (cons? obj) (has? obj ':struct-type)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defstruct (struct-type . slots)
 "Define a new struct type STRUCT type with slots SLOTS."
 (unless (symbol? struct-type) (error "STRUCT-TYPE must be a symbol"))
 (unless (list? slots)         (error "SLOTS must be a list"))
 (unless (all? symbol? slots)  (error "SLOTS must be a list of symbols"))
 (let
  ((getters (mapcar (lambda (slot) $('make-struct-getter struct-type slot)) slots))
   (setters (mapcar (lambda (slot) $('make-struct-setter struct-type slot)) slots)))
  (cons 'list
   (append
    $($('make-struct-constructor struct-type . slots))
    $($('make-struct-predicate struct-type))
    getters
    setters))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'struct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos scheme compat aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! #f            nil)
(setq! #t            t)
(setq! ???           'unspecified-result)
;; (setq! assoc         ahas?) 
;; (setq! assq          aget) 
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
;; (setq! remove        removeq)
(setq! set!          setq!) ;should should be a macro that avoids re-defining what-scheme-implementation
(setq! vector-length list-length)
(setq! vector-ref    list-ref)
(setq! vector-set!   list-set!)
(setq! null?         nil?)  
(setq! pair?         cons?) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'scheme-compat-aliases)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp compat aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! nreverse         reverse)
(setq! setq             setq!)
(setq! rplaca           rplaca!)
(setq! rplacd           rplacd!)
(setq! nconc            nconc!)
(setq! null             nil?)
(setq! identity         id)
(setq! expand-file-name expand-path)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'elisp-compat-aliases)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mono-std)
(provide 'std) ;; this counts as an implementation of 'std.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *log-loading-std-enabled*
 (nl)
 (princ "Loaded in   ")
 (princ (elapsed-us *time-before-loading-std*))
 (princ " us.")
 (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
