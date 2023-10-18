;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; 'standard library', such as it is:                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; crucial macros, without which nothing else in stdlib will even work:       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! defmacro                                                               ;)
 (macro (name params . body)                                                  ;)
  $(setq! name $(macro  params . body))))                                     ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defmacro defun (name params . body)                                          ;)
 $('setq! name $('lambda params . body)))                                     ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

(defmacro expand-quasiquoted (obj)
  (cond
   ;; Handle end of list
   ((nil? obj) nil)

   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle cons cells
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (cond
       ;; If the cdr is an unquote form, we use append2
       ((and (cons? tail) (eq? 'unquote (car tail)))
        (list 'append2
              (list 'list 
                    (if (cons? head) 
                        (list 'expand-quasiquoted head)
                        (list 'quote head)))
              (cadr tail)))
       
       ;; If the car is an unquote form, handle it and recursively expand the cdr
       ((and (cons? head) (eq? 'unquote (car head)))
        (list 'cons
              (cadr head)
              (list 'expand-quasiquoted tail)))

       ;; Otherwise, handle as a regular cons cell
       (t
        (list 'cons 
              (if (cons? head) 
                  (list 'expand-quasiquoted head)
                  (list 'quote head))
              (list 'expand-quasiquoted tail))))))

   ;; Handle everything else
   (t (list 'quote obj))))


(defmacro expand-quasiquoted (obj)
  (cond
   ;; Handle end of list
   ((nil? obj) nil)

   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle cons cells
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (cond
       ;; If the car is an unquote form, handle it and recursively expand the cdr
       ((and (cons? head) (eq? 'unquote (car head)))
        (list 'cons (cadr head) (list 'expand-quasiquoted tail)))

       ;; If the cdr is an unquote form, we use append2
       ((and (cons? tail) (eq? 'unquote (car tail)))
        (list 'append2 (list 'list (list 'expand-quasiquoted head)) (cadr tail)))

       ;; Otherwise, handle as a regular cons cell
       (t (list 'cons 
                (list 'expand-quasiquoted head) 
                (list 'expand-quasiquoted tail))))))

   ;; Handle everything else
   (t (list 'quote obj))))

(defmacro expand-quasiquoted (obj)
  (cond
   ;; Handle end of list
   ((nil? obj) nil)

   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle cons cells
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (cond
       ;; If the car is an unquote form, handle it and recursively expand the cdr
       ((and (cons? head) (eq? 'unquote (car head)))
        (list 'cons (cadr head) (list 'expand-quasiquoted tail)))

       ;; If the cdr is an unquote form, we use append2
       ((and (cons? tail) (eq? 'unquote (car tail)))
        (list 'append2 (list 'list (list 'expand-quasiquoted head)) (cadr tail)))

       ;; Otherwise, handle as a regular cons cell
       (t (list 'cons 
                (list 'expand-quasiquoted head) 
                (list 'expand-quasiquoted tail))))))

   ;; Handle everything else
   (t (list 'quote obj))))

(defmacro expand-quasiquoted (obj)
  (cond
   ;; Handle end of list
   ((nil? obj) nil)

   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle cons cells
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (cond
       ;; If the car is an unquote form, we should splice its content into the outer list.
       ((and (cons? head) (eq? 'unquote (car head)))
        `(append2 ,(cadr head) ,(expand-quasiquoted tail)))

       ;; If the cdr is an unquote form, we use append2
       ((and (cons? tail) (eq? 'unquote (car tail)))
        `(append2 (list ,@(if (cons? head) 
                              `(,(expand-quasiquoted head))
                              `(',head)))
                  ,(cadr tail)))

       ;; Otherwise, handle as a regular cons cell
       (t
        `(cons ,(if (cons? head) 
                   (expand-quasiquoted head)
                   (list 'quote head))
              ,(expand-quasiquoted tail))))))

   ;; Handle everything else
   (t (list 'quote obj))))

(defmacro expand-quasiquoted (obj)
  (cond
   ;; Handle end of list
   ((nil? obj) nil)

   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle cons cells
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (cond
       ;; If the car is an unquote form, we should splice its content into the outer list.
       ((and (cons? head) (eq? 'unquote (car head)))
        (list 'append2 
              (cadr head)
              (list 'expand-quasiquoted tail)))

       ;; If the cdr is an unquote form, we use append2
       ((and (cons? tail) (eq? 'unquote (car tail)))
        (list 'append2 
              (list 'list ,(if (cons? head) 
                               (list 'expand-quasiquoted head)
                               head))
              (cadr tail)))

       ;; Otherwise, handle as a regular cons cell
       (t
        (list 'cons 
              (if (cons? head) 
                  (list 'expand-quasiquoted head)
                  head)
              (list 'expand-quasiquoted tail))))))

   ;; Handle everything else
   (t (list 'quote obj))))

(defmacro expand-quasiquoted (obj)
  (cond
   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle (unquote-splicing something)
   ((and (cons? obj) (eq? 'unquote-splicing (car obj)))
    (error "unquote-splicing is not handled at top level"))

   ;; Handle cons cells
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (cond
       ;; If the car is an unquote-splicing form, error out
       ((and (cons? head) (eq? 'unquote-splicing (car head)))
        (error "unquote-splicing found in the car of a cons"))

       ;; If the cdr is an unquote-splicing form, splice it in
       ((and (cons? tail) (eq? 'unquote-splicing (car tail)))
        (list 'append2 
              (list 'list (list 'expand-quasiquoted head))
              (cadr tail)))

       ;; Otherwise, handle as a regular cons cell
       (t
        (list 'cons 
              (list 'expand-quasiquoted head)
              (list 'expand-quasiquoted tail))))))

   ;; Handle everything else
   (t obj)))

(defmacro expand-quasiquoted (obj)
  (cond
   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle (unquote-splicing something)
   ((and (cons? obj) (eq? 'unquote-splicing (car obj)))
    (error "unquote-splicing is not handled at top level"))

   ;; Handle cons cells
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (cond
       ;; If the car is an unquote-splicing form, error out
       ((and (cons? head) (eq? 'unquote-splicing (car head)))
        (error "unquote-splicing found in the car of a cons"))

       ;; If the cdr is an unquote-splicing form, splice it in
       ((and (cons? tail) (eq? 'unquote-splicing (car tail)))
        (list 'append2 
              (list 'list (list 'expand-quasiquoted head))
              (cadr tail)))

       ;; Otherwise, handle as a regular cons cell
       (t
        (let ((expanded-head (expand-quasiquoted head))
              (expanded-tail (expand-quasiquoted tail)))
         
          ;; Check if the tail contains an unquote
          (if (and (cons? expanded-tail) (eq? (car expanded-tail) 'unquote))
              (list 'append2 (list 'list expanded-head) (cadr expanded-tail))
              (list 'cons expanded-head expanded-tail)))))))

   ;; Handle symbols
   ((symbol? obj) 
    (list 'quote obj))

   ;; Handle everything else
   (t obj)))

(defun unquote? (expr)
  (and (cons? expr) (eq? (car expr) 'unquote)))

(defun unquote-splicing? (expr)
  (and (cons? expr) (eq? (car expr) 'unquote-splicing)))

(defmacro expand-quasiquoted (expr)
  (cond
    ((not (cons? expr)) expr) ; base case: atoms are unchanged
    ((unquote? expr)
     (cadr expr)) ; directly replace (unquote X) with X
    ((unquote-splicing? expr)
     (error "unquote-splicing not at top level"))
    (t
     (let* ((head (car expr))
            (tail (cdr expr))
            ;; Transform the head
            (expanded-head (list 'expand-quasiquoted head))
            ;; Transform the tail
            (expanded-tail (if (unquote? tail)
                               (cadr tail)
                               (list 'expand-quasiquoted tail))))
       ;; Combine the head and tail appropriately
       (if (unquote-splicing? tail)
           (list 'append2 (list 'list expanded-head) (cadr tail))
           (list 'cons expanded-head expanded-tail))))))

(defmacro expand-quasiquoted (expr)
  (cond
    ((not (cons? expr)) expr) ; base case: atoms are unchanged
    ((unquote? expr)
     (cadr expr)) ; directly replace (unquote X) with X
    ((unquote-splicing? expr)
     (error "unquote-splicing not at top level"))
    (t
     (let* ((head (car expr))
            (tail (cdr expr))
            ;; Transform the head
            (expanded-head (if (unquote? head)
                               (cadr head)
                               (list 'expand-quasiquoted head)))
            ;; Transform the tail
            (expanded-tail (if (unquote? tail)
                               (cadr tail)
                               (list 'expand-quasiquoted tail))))
       ;; Combine the head and tail appropriately
       (if (unquote? tail)
           (list 'append2 (list 'list expanded-head) expanded-tail)
           (list 'cons expanded-head expanded-tail))))))

(defmacro expand-quasiquoted (expr)
  (cond
    ((not (cons? expr)) expr) ; base case: atoms are unchanged
    ((eq? (car expr) 'unquote)
     (cadr expr)) ; directly replace (unquote X) with X
    ((eq? (car expr) 'unquote-splicing)
     (error "unquote-splicing not at top level"))
    (t
     (let* ((head (car expr))
            (tail (cdr expr))
            ;; Transform the head
            (expanded-head (cond
                             ((eq? (car head) 'unquote) (cadr head))
                             (t (list 'expand-quasiquoted head))))
            ;; Transform the tail
            (expanded-tail (cond
                             ((eq? (car tail) 'unquote) (cadr tail))
                             (t (list 'expand-quasiquoted tail)))))
       ;; Combine the head and tail appropriately
       (cond
        ((eq? (car tail) 'unquote)
         (list 'append2 (list 'list expanded-head) expanded-tail))
        (t
         (list 'cons expanded-head expanded-tail)))))))

(defmacro expand-quasiquoted (expr)
  (cond
    ((not (cons? expr)) expr) ; base case: atoms are unchanged
    ((eq? (car expr) 'unquote)
     (cadr expr)) ; directly replace (unquote X) with X
    ((eq? (car expr) 'unquote-splicing)
     (error "unquote-splicing not at top level"))
    (t
     (let* ((head (car expr))
            (tail (cdr expr))
            ;; Transform the head
            (expanded-head (cond
                             ((and (cons? head) (eq? (car head) 'unquote)) (cadr head))
                             (t (list 'expand-quasiquoted head))))
            ;; Transform the tail
            (expanded-tail (cond
                             ((and (cons? tail) (eq? (car tail) 'unquote)) (cadr tail))
                             (t (list 'expand-quasiquoted tail)))))
       ;; Combine the head and tail appropriately
       (cond
        ((and (cons? tail) (eq? (car tail) 'unquote))
         (list 'append2 (list 'list expanded-head) expanded-tail))
        (t
         (list 'cons expanded-head expanded-tail)))))))

(defmacro expand-quasiquoted (expr)
  (cond
    ;; If it's not a cons and not being unquoted, then it's an atom we should quote
    ((not (cons? expr)) (list 'quote expr))
    ;; Directly replace (unquote X) with X
    ((eq? (car expr) 'unquote) (cadr expr))
    ;; Error out for splicing outside of list context
    ((eq? (car expr) 'unquote-splicing) (error "unquote-splicing not at top level"))
    (t
     (let* ((head (car expr))
            (tail (cdr expr))
            ;; Transform the head
            (expanded-head (cond
                             ((and (cons? head) (eq? (car head) 'unquote)) (cadr head))
                             (t (list 'expand-quasiquoted head))))
            ;; Transform the tail
            (expanded-tail (cond
                             ((and (cons? tail) (eq? (car tail) 'unquote)) (cadr tail))
                             (t (list 'expand-quasiquoted tail)))))
       ;; Combine the head and tail appropriately
       (cond
        ((and (cons? tail) (eq? (car tail) 'unquote))
         (list 'append2 (list 'list expanded-head) expanded-tail))
        (t
         (list 'cons expanded-head expanded-tail)))))))

(defmacro expand-quasiquoted (expr)
  (cond
    ;; If it's not a cons and not being unquoted, then it's an atom we should quote
    ((not (cons? expr)) (list 'quote expr))
    ;; Directly replace (unquote X) with X
    ((eq? (car expr) 'unquote) (cadr expr))
    ;; Error out for splicing outside of list context
    ((eq? (car expr) 'unquote-splicing) (error "unquote-splicing not at top level"))
    ((and (cons? (cdr expr)) (cons? (car (cdr expr))) (eq? (car (car (cdr expr))) 'unquote))
     ;; If the second element of the list is an unquote, we want to use append2
     (list 'append2 (list 'list (list 'expand-quasiquoted (car expr))) (cadr (car (cdr expr)))))
    (t
     (let* ((head (car expr))
            (tail (cdr expr))
            ;; Transform the head
            (expanded-head (cond
                             ((and (cons? head) (eq? (car head) 'unquote)) (cadr head))
                             (t (list 'expand-quasiquoted head))))
            ;; Transform the tail
            (expanded-tail (cond
                             ((and (cons? tail) (eq? (car tail) 'unquote)) (cadr tail))
                             (t (list 'expand-quasiquoted tail)))))
       ;; Combine the head and tail appropriately
       (list 'cons expanded-head expanded-tail)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! quasiquote expand-quasiquoted)                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

;; (defun expand-quasiquoted (expr)
;;   (cond
;;     ;; If it's an unquoted expression, just return the second item
;;     ((and (cons? expr) (eq? 'unquote (car expr))) (cadr expr))
;;     ;; If it's a list with a head and tail, and the tail has an unquote
;;     ((and (cons? expr)
;;           (cons? (cdr expr))
;;           (eq? 'unquote (car (cdr expr))))
;;      (list 'append2
;;            (list 'list (expand-quasiquoted (car expr)))
;;            (cadr (cdr expr))))
;;     ;; If it's a list, recursively expand it
;;     ((cons? expr) (list 'cons (expand-quasiquoted (car expr))
;;                         (expand-quasiquoted (cdr expr))))
;;     ;; If it's an atom, quote it
;;     (t (list 'quote expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; type predicates:                                                           ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun type?     (typ o) (eq?   typ     (type           o)))                  ;)
(defun atom?     (o)     (not  (type?   :CONS           o)))                  ;)
(defun char?     (o)           (type?   :CHAR           o))                   ;)
(defun cons?     (o)           (type?   :CONS           o))                   ;)
(defun core?     (o)           (type?   :CORE           o))                   ;)
(defun env?      (o)           (type?   :ENV            o))                   ;)
(defun error?    (o)           (type?   :ERROR          o))                   ;)
(defun float?    (o)           (type?   :FLOAT          o))                   ;)
(defun integer?  (o)           (type?   :INTEGER        o))                   ;)
(defun lambda?   (o)           (type?   :LAMBDA         o))                   ;)
(defun macro?    (o)           (type?   :MACRO          o))                   ;)
(defun rational? (o)           (type?   :RATIONAL       o))                   ;)
(defun string?   (o)           (type?   :STRING         o))                   ;)
(defun symbol?   (o)           (type?   :SYMBOL         o))                   ;)
(defun improper? (o)     (and  (tail? o) (not (proper?  o))))                 ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; fancy output funs:                                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; (defmacro princn args       $(progn (cons princ args)                 $(nl)))
;; (defmacro printn args       $(progn (cons print args)                 $(nl)))
;; (defmacro putn   args       $(progn (cons put   args)                 $(nl)))
;; (defmacro writen args       $(progn (cons write args)                 $(nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun princn  args       (apply princ   args) (nl))                          ;)
(defun printn  args       (apply print   args) (nl))                          ;)
(defun putn    args       (apply put     args) (nl))                          ;)
(defun writen  args       (apply write   args) (nl))                          ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun princni (i . args) (apply princn  (intercalate i args)))               ;)
(defun printni (i . args) (apply printn  (intercalate i args)))               ;)
(defun putni   (i . args) (apply putn    (intercalate i args)))               ;)
(defun writeni (i . args) (apply writen  (intercalate i args)))               ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun princns args       (apply princni (cons " " args)))                    ;)
(defun printns args       (apply printni (cons " " args)))                    ;)
(defun putns   args       (apply putni   (cons " " args)))                    ;)
(defun writens args       (apply writeni (cons " " args)))                    ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; compound car/cdrs:                                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun caar     (lst)      (car (car lst)))                                   ;)
(defun cadr     (lst)      (car (cdr lst)))                                   ;)
(defun cdar     (lst)      (cdr (car lst)))                                   ;)
(defun cddr     (lst)      (cdr (cdr lst)))                                   ;)
(defun caaar    (lst) (car (car (car lst))))                                  ;)
(defun caadr    (lst) (car (car (cdr lst))))                                  ;)
(defun cadar    (lst) (car (cdr (car lst))))                                  ;)
(defun caddr    (lst) (car (cdr (cdr lst))))                                  ;)
(defun cdaar    (lst) (cdr (car (car lst))))                                  ;)
(defun cdadr    (lst) (cdr (car (cdr lst))))                                  ;)
(defun cddar    (lst) (cdr (cdr (car lst))))                                  ;)
(defun cdddr    (lst) (cdr (cdr (cdr lst))))                                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun first    (lst)                      (car   lst))                       ;)
(defun second   (lst)                      (cadr  lst))                       ;)
(defun third    (lst)                      (caddr lst))                       ;)
(defun fourth   (lst)               (car   (cdddr lst)))                      ;)
(defun fifth    (lst)               (cadr  (cdddr lst)))                      ;)
(defun sixth    (lst)               (caddr (cdddr lst)))                      ;)
(defun seventh  (lst)        (car   (cdddr (cdddr lst))))                     ;i
(defun eighth   (lst)        (cadr  (cdddr (cdddr lst))))                     ;)
(defun ninth    (lst)        (caddr (cdddr (cdddr lst))))                     ;)
(defun tenth    (lst) (car   (cdddr (cdddr (cdddr lst)))))                    ;)
(defun eleventh (lst) (cadr  (cdddr (cdddr (cdddr lst)))))                    ;)
(defun twelfth  (lst) (caddr (cdddr (cdddr (cdddr lst)))))                    ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; equal? predicate:                                                          ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun equal? (o1 o2)                                                         ;)
 "True when o1 and o2 are eql? or cons trees whose atomic members are equal?.";)
 (cond                                                                        ;i
  ((and (atom? o1) (atom? o2)) (eql? o1 o2))                                  ;)
  ((and (cons? o1) (cons? o2))                                                ;)
   (and (equal? (car o1) (car o2))                                            ;)
    (equal? (cdr o1) (cdr o2))))                                              ;)
  (t nil)))                                                                   ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; even?/odd? predicates:                                                     ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun even?     (n)        (== 0 (% n 2 )))                                  ;)
(defun odd?      (n)        (== 1 (% n 2 )))                                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (map variants):                                                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun mapcar (fun lst)                                                       ;)
 "Map fun over list, returning the resulting list."                           ;)
 (when lst                                                                    ;)
  (cons (fun (car lst)) (mapcar fun (cdr lst)))))                             ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun mapc (fun lst)                                                         ;)
 "Map fun over list for side-effects only, ignoring the results and returning";)
 "nil."                                                                       ;)
 (when lst                                                                    ;)
  (fun (car lst))                                                             ;)
  (mapc fun (cdr lst))))                                                      ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun mapconcat (fun lst delimiter)                                          ;)
 "Map fun over list, returning the result of concatenating the resulting"     ;)
 "strings."                                                                   ;)
 (if lst                                                                      ;)
  (reduce                                                                     ;)
   (lambda (acc item)                                                         ;)
    (concat acc delimiter item))                                              ;)
   (fun (car lst))                                                            ;)
   (mapcar fun (cdr lst)))                                                    ;)
  ""))                                                                        ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun mapcan (fun lst)                                                       ;)
 "Map fun over list and concatenate the results by altering them."            ;)
 (when lst                                                                    ;)
  (let ((result (fun (car lst)))                                              ;)
        (rest   (mapcan fun (cdr lst))))                                      ;)
   (if result                                                                 ;)
    (nconc! result rest)                                                      ;)
    rest))))                                                                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (nth/last):                                                      ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun nth (n lst)                                                            ;)
 "Get nth item in a list."                                                    ;)
 (cond                                                                        ;)
  ((nil? lst)  nil)                                                           ;)
  ((eql? n 0)  (car lst))                                                     ;)
  (t           (nth (- n 1) (cdr lst)))))                                     ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun last                                                                   ;)
 (lst)                                                                        ;)
 "Get last item in a list."                                                   ;)
 (cond                                                                        ;)
  ((nil? lst) nil)                                                            ;)
  ((nil? (cdr lst)) lst)                                                      ;)
  (t (last (cdr lst)))))                                                      ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; reduced:                                                                   ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun reduced (fun)                                                          ;)
 "Return a function that is a reduction of the binary function fun."          ;)
 (lambda args                                                                 ;)
  (reduce fun (car args) (cdr args))))                                        ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (append/nconc variants):                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun append2 (lst1 lst2)                                                    ;)
 "Append two lists."                                                          ;)
 (if lst1                                                                     ;)
  (cons (car lst1) (append2 (cdr lst1) lst2))                                 ;i
  lst2))                                                                      ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! append (reduced append2))                                              ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun nconc2! (lst1 lst2)                                                    ;)
 "Destructively join two lists."                                              ;)
 (cond                                                                        ;)
  ((nil? lst1) lst2)                                                          ;)
  (t (rplacd! (last lst1) lst2) lst1)))                                       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! nconc! (reduced nconc2!))                                              ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (push/push-back):                                                ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun push-back! (lst elem)                                                  ;)
 "Destructively push elem onto the end of lst."                               ;)
 (rplacd! (last lst) (cons elem nil))                                         ;)
 lst)                                                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun push! (elem lst)                                                       ;)
 "Destructively push elem onto the front of lst."                             ;)
 (let ((old-car (car lst)))                                                   ;)
  (rplaca! lst elem)                                                          ;)
  (rplacd! lst (cons old-car (cdr lst)))                                      ;)
  lst))                                                                       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun push-back (lst elem)                                                   ;)
 "Non-destructively push elem onto the end of lst."                           ;)
 (append lst (cons elem nil)))                                                ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun push (elem lst)                                                        ;)
 "Non-destructively push elem onto the front of lst, aka cons."               ;)
 (cons elem lst))                                                             ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (reduce/rreduce/filter):                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun reduce (fun acc lst)                                                   ;)
 "Left reduce ('fold') list by applying fun to successive pairs."             ;)
 (if (nil? lst)                                                               ;)
  acc                                                                         ;)
  (reduce fun (fun acc (car lst)) (cdr lst))))                                ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun rreduce (fun acc lst)                                                  ;)
 "Right reduce ('foldr') list by applying fun to successive pairs."           ;)
 (if (nil? lst)                                                               ;)
  acc                                                                         ;)
  (fun (car lst) (rreduce fun acc (cdr lst)))))                               ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun filter (pred? lst)                                                     ;)
 "Return a list containing those members of lst satisfying pred?."            ;)
 (cond                                                                        ;)
  ((nil? lst) nil)                                                            ;)
  ((pred? (car lst))                                                          ;)
   (cons (car lst) (filter pred? (cdr lst))))                                 ;)
  (t (filter pred? (cdr lst)))))                                              ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (flattening):                                                    ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun flatten1 (lst)                                                         ;)
 (cond                                                                        ;)
  ((nil? lst) nil)                                                            ;)
  ((tail? (car lst))                                                          ;)
   (append (car lst) (flatten1 (cdr lst))))                                   ;)
  (t (cons (car lst) (flatten1 (cdr lst))))))                                 ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun flatten-left (lst)                                                     ;)
 "Flatten left-nested list structures."                                       ;)
 (if (cons? (car lst))                                                        ;)
  (append (flatten-left (car lst)) $(cadr lst))                               ;i
  lst))                                                                       ;o
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun flatten (lst)                                                          ;)
 (when lst                                                                    ;)
  (if (cons? (car lst))                                                       ;)
   (append (flatten (car lst)) (flatten (cdr lst)))                           ;)
   (cons (car lst) (flatten (cdr lst))))))                                    ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (zipping):                                                       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun zip2 (lst1 lst2)                                                       ;)
 "Zip two lists."                                                             ;)
 (cond                                                                        ;)
  ((or (nil? lst1) (nil? lst2)) nil)                                          ;)
  (t (cons $((car lst1) (car lst2))                                           ;)
      (zip2  (cdr lst1) (cdr lst2))))))                                       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun zip3 (l1 l2 l3)                                                        ;)
 "Zip three lists."                                                           ;)
 (mapcar flatten1 (reduce zip2 l1 $(l2 l3))))                                 ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! left-nested-zip (reduced zip2))                                        ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defmacro zip lists                                                           ;)
 "Zip many lists. This might not flatten properly if the zipped elements are" ;)
 "themselves lists."                                                          ;)
 (if (cdr lists)                                                              ;)
  `(mapcar flatten (cons left-nested-zip ,lists))
;;  (list 'mapcar 'flatten (cons 'left-nested-zip lists))                       ;)
  (list 'mapcar 'list    (car lists))))                                       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defmacro old-zip lists                                                           ;)
 "Zip many lists. This might not flatten properly if the zipped elements are" ;)
 "themselves lists."                                                          ;)
 (if (cdr lists)                                                              ;)
  (list 'mapcar 'flatten (cons 'left-nested-zip lists))                       ;)
  (list 'mapcar 'list    (car lists))))                                       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (transform);                                                     ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun transform! (obj pred? fun)                                             ;)
 "Destructively transform the cons tree obj by replacing members matching"    ;)
 "pred? with the result of applying fun to them."                             ;)
 (if (atom? obj)                                                              ;)
  (error "obj must be a list")                                                ;)
  (cond                                                                       ;)
   ((pred? obj) (set! obj (fun obj)))                                         ;)
   ((cons? obj)                                                               ;)
    (let ((head (car obj))                                                    ;)
          (tail (cdr obj)))                                                   ;)
     (cond                                                                    ;)
      ((pred? head)  (rplaca! obj (fun head)))                                ;)
      ((cons? head) (transform! head pred? fun)))                             ;)
     (cond                                                                    ;)
      ((pred? tail)  (rplacd! obj (fun tail)))                                ;)
      ((cons? tail) (rplacd! obj (transform! tail pred? fun))))))             ;)
   (t obj))                                                                   ;)
  obj))                                                                       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun transform (obj pred? fun)                                              ;)
 "Transform obj by replacing members matching pred? with the result of"       ;)
 "applying fun to them or, if obj is not a cons tree, by applying fun to obj.";)
 (if (atom? obj)                                                              ;)
  (if (pred? obj)                                                             ;)
   (fun obj)                                                                  ;)
   obj)                                                                       ;)
  (cons                                                                       ;)
   (transform (car obj) pred? fun)                                            ;)
   (transform (cdr obj) pred? fun))))                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun prefetch (expr)                                                        ;)
 "Try to optimize expr by replacing it's symbol? members with the result of"  ;)
 "looking them up. This is, mysteriously, not a very effective optimization." ;)
 (transform! expr                                                             ;)
  (lambda (x) (and (symbol? x) (bound? x)))                                   ;)
  (lambda (x) (eval x))))                                                     ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (unsorted):                                                      ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun any? (pred? lst)                                                       ;)
 "True when any lst members are pred?."                                       ;)
 (when lst                                                                    ;)
  (or                                                                         ;)
   (pred? (car lst))                                                          ;)
   (any? pred? (cdr lst)))))                                                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun all? (pred? lst)                                                       ;)
 "True when all lst members are pred?."                                       ;)
 (if lst                                                                      ;)
  (and                                                                        ;)
   (pred? (car lst))                                                          ;)
   (all? pred? (cdr lst)))                                                    ;)
  t))                                                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun heads (lsts)                                                           ;)
 (when lsts                                                                   ;)
  (cons (car (car lsts)) (heads (cdr lsts)))))                                ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun tails (lsts)                                                           ;)
 (when lsts                                                                   ;)
  (cons (cdr (car lsts)) (tails (cdr lsts)))))                                ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun compose-preds preds                                                    ;)
 "Does what it says on the tin and composes preds."                           ;)
 (lambda (val)                                                                ;)
  (let*                                                                       ;)
   ((fun                                                                      ;)
     (lambda (preds)                                                          ;)
      (cond                                                                   ;)
       ((nil? (car preds))       t)                                           ;)
       ((nil? ((car preds) val)) nil)                                         ;)
       (t     (fun (cdr preds)))))))                                          ;)
   (fun preds))))                                                             ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun intercalate (intercalated items)                                       ;)
 "Intercalate intercalated between items."                                    ;)
 (if (or (nil? items) (nil? (cdr items)))                                     ;)
  items                                                                       ;)
  (cons (car items)                                                           ;)
   (cons intercalated                                                         ;)
    (intercalate intercalated (cdr items))))))                                ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; log toggle helpers, these should be replaced with macros:                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun with-toggled-fun1 (toggled-fun)                                        ;)
 "Get a function that enables toggled-fun, evaluates fun-or-expr and sets"    ;)
 "toggled-fun back to it's prior state."                                      ;)
 (lambda (fun-or-expr)                                                        ;)
  (if (lambda? fun-or-expr)                                                   ;)
   (let* ((old    (toggled-fun t))                                            ;)
          (result (fun-or-expr))                                              ;)
          (new    (toggled-fun old)))                                         ;)
    result)                                                                   ;)
   (let* ((old    (toggled-fun t))                                            ;)
          (result (eval fun-or-expr))                                         ;)
          (new    (toggled-fun old)))                                         ;)
    result))))                                                                ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun with-toggled-fun (toggled-fun)                                         ;)
 "Get a function that enables toggled-fun, evaluates fun-or-exprs and sets"   ;)
 "toggled-fun back to it's prior state."                                      ;)
 (lambda funs-or-exprs                                                        ;)
  (last (mapcar (with-toggled-fun1 toggled-fun) funs-or-exprs))))             ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! with-log-eval (with-toggled-fun log-eval))                             ;)
(setq! with-log-core (with-toggled-fun log-core))                             ;)
(setq! with-log-all  (with-toggled-fun log-all))                              ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; random stuff that's all one section for now:                               ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun id     (x)    x)                                                       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun max    (a b) (if (> a b) a b))                                         ;)
(defun min    (a b) (if (< a b) a b))                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun 1+     (n) (+ 1 n))                                                    ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun double (n) (<< n 1))                                                   ;)
(setq! 2*     double)                                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun benchmark (repetitions print-interval qexpr)                           ;)
 "Benchmark expr by running it repetitions time and returning the"            ;)
 "total/average time in ms, printing updates ever print-interval iterations." ;)
 (nl)                                                                         ;)
 (let ((ctr   0)                                                              ;)
       (total 0))                                                             ;)
  (repeat repetitions                                                         ;)
   (setq! ctr (+ 1 ctr))                                                      ;)
   (let ((before (time)))                                                     ;)
    (eval qexpr)                                                              ;)
    (setq! total (+ total (elapsed before))))                                 ;)
   (when (== 0 (% ctr print-interval))                                        ;)
    (nl)                                                                      ;)
    (princ "Iteration #")                                                     ;)
    (princ ctr)                                                               ;)
    (princ ", ")                                                              ;)
    (princ (/ total 1000))                                                    ;)
    (princ " ms so far.")))                                                   ;)
  (nl)                                                                        ;)
  (princ "total ums: ")u                                                      ;)
  (princ total) (nl)                                                          ;)
  (princ "total ms: ")                                                        ;)
  (princ (/ total 1000)) (nl)                                                 ;)
  (princ "total s: ")                                                         ;)
  (princ (/ total 1000000)) (nl)                                              ;)
  (let ((each-ms (/ total repetitions 1000)))                                 ;)
   (princ "each ms: ")                                                        ;)
   (princ (/ total repetitions 1000))                                         ;)
   (nl)                                                                       ;)
   each-ms)))                                                                 ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun depth (lst)                                                            ;)
 "Get the depth of a nested list structure. This one is untested."            ;)
 (if (atom? lst)                                                              ;)
  0                                                                           ;)
  (max 1 (+ (depth (car lst)) (depth (cdr lst))))))                           ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
