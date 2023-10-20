;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos scheme compat:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! else         t)
(setq! #t           t)
(setq! #f           nil)
(setq! assq         aget)
(setq! define       setq!)
;; ^ should be a macro that avoids re-definining what-scheme-implementation
(setq! null?        nil?)
(setq! set!         setq!)
(setq! ???          'unspecified-result)
(setq! letrec       let*)
(setq! list*        list) ;; might need support.scm's version
(setq! map          mapcar)
(setq! position-of  indexq)
(defun gsort (predicate lst)
 (sort lst predicate))
(defun %allocate-instance-internal (head . tail)
 head) ;; FAKE PLACEHOLDER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-list (size init-val)
 (cond
  ((== 0 size)  nil)
  (t            (cons init-val (make-list (- size 1) init-val)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-set! (lst index val)
 (cond
  ((nil? lst)   (error "list-set! out of range"))
  ((== 0 index) (rplaca! lst val))
  (t            (list-set! (cdr lst) (- index 1) val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-ref (lst index)
 "This is basically nth but with the parameter order revered and raising an"
 "error if the index is out of range."
 (cond
  ((nil? lst)   (error "list-set! out of range"))
  ((== 0 index) (car lst))
  (t            (list-ref (cdr lst) (- index 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! make-vector make-list)  ;; for tiny-clos scheme compat.
(setq! vector-set! list-set!)  ;; for tiny-clos scheme compat.
(setq! vector-ref  list-ref)   ;; for tiny-clos scheme compat.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! v (make-list 10 0))

(when t
 (setq! ix 0)
 
 (until  (== ix 10)
  (princ "setting ") (princ ix) (nl)
  (list-set! v ix (* 2 ix))
  (write v)
  (nl)
  (setq! ix (1+ ix))))

;;(log-eval t)

;;(list-set! v 10 99)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun mapcar! (fun lst)                                                      ;)
 "Map fun over list, altering the list."                                      ;)
 (when lst                                                                    ;)
  (rplaca! (mapcar! fun (cdr lst)) (fun (car lst)))))                         ;)
(defun mapcar! (fun lst)
  "Map fun over list, altering the list."
  (when lst
    (rplaca! lst (fun (car lst))) ; replace the current element
    (when (cdr lst)               ; check if there's a next element
      (mapcar! fun (cdr lst)))   ; recursively process the rest of the list
    lst))                        ; return the modified list
                                        ; return the modified list
(defun mapcar! (fun lst)
  "Map fun over list, altering the list."
  (cond
   ((nil? lst) nil) ; base case: if the list is nil, return nil
   (t
    (rplaca! lst (fun (car lst))) ; replace the current element
    (mapcar! fun (cdr lst))       ; recursively process the rest of the list
    lst)))                        ; return the modified list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

(setq! lst '(1 2 3))

(log-eval t)

(mapcar! double lst)

(write lst)
