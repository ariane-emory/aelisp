;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos scheme compat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! else         t)
(setq! #t           t)
(setq! #f           nil)
(setq! assq         aget)
(setq! define       setq!)
;; ^ should be a macro to avoid redefinining what-scheme-implementation
(setq! null?        nil?)
(setq! set!         setq!)
(setq! ???          'unspecified-result)
(setq! letrec       let*)
(setq! list*        list) ;; might need support.scm's version
(setq! map          mapcar)
(setq! position-of  indexq)
(defun gsort (predicate lst)
 (sort lst predicate))
(defun vector-ref   (lst n)
 (nth n lst))
(defun %allocate-instance-internal (head . tail)
 head) ;; FAKE PLACEHOLDER
(defun make-vector  (head . tail)
 head) ;; FAKE PLACEHOLDER
(defun vector-set!  (head . tail)
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

(setq! v (make-list 10 0))

;; (nl)
;; (write v)
;; (nl)

(nl)


(when t
 (setq! ix 0)
 
 (until  (== ix 10)
  (princ "setting ") (princ ix) (nl)
  (list-set! v ix (* 2 ix))
  (write v)
  (nl)
  (setq! ix (1+ ix))))

;(log-eval t)

;(list-set! v 10 99)
