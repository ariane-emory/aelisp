;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos scheme compat:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! else          t)
(setq! #t            t)
(setq! #f            nil)
(setq! define        setq!)
;; ^ should be a macro that avoids re-definining what-scheme-implementation
(setq! null?         nil?)
(setq! set!          setq!)
(setq! ???           'unspecified-result)
(setq! getl          pget)
(setq! map           mapcar)
(setq! every         all?)
(setq! remove        removeq)
(setq! collect-if    filter)
(setq! map-append    mapcan)
(setq! position-of   indexq)
(setq! make-vector   make-list)
(setq! vector-set!   list-set!)
(setq! vector-ref    list-ref)
(setq! vector-length list-length)
(defun gsort (predicate lst)
 (sort lst predicate))
(defun %allocate-instance-internal (head . tail)
 head) ;; FAKE PLACEHOLDER
;; remove-duplicates
;; union
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! v (make-list 10 0))

(when t

 (setq! ix 0)
 (until  (== ix 10)
  (princ "setting ") (princ ix) (nl)
  (list-set! v ix (* 2 ix))
  (write v)
  (nl)
  (setq! ix (1+ ix)))

 (setq! ix 0)
 (until  (== ix 10)
  (princ "getting ") (princ ix) (princ ": ")
  (princ (list-ref v ix))
  (nl)
  (setq! ix (1+ ix))))

(write (list* 'a 'b '(c d)))
(nl)




(defun union lists
 (let*
  ((clean
    (lambda (list result)
		 (cond ((nil? list) result)
			((memq? (car list) result)
			 (clean (cdr list) result))
			(else
			 (clean (cdr list) (cons (car list) result)))))))
	(clean (apply append lists))))

(log-eval t)

(union '(1 2 3) '(a b c))
