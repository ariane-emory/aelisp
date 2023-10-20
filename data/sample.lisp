;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tiny-clos scheme compat:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! else          t)
(setq! #t            t)
(setq! #f            nil)
(setq! define        setq!)
(setq! display       write)
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







(defun union-helper (lst result)
 (if (null? lst)
  result
  (let* ((current (car lst))
         (new-result (if (memq? current result)
                      result
                      (cons current result))))
   (union-helper (cdr lst) new-result))))

(defun union (list1 list2)
 (union-helper list2 (union-helper list1 '())))

;; (define memq?
;;   (lambda (item list)
;;     (cond ((null? list) #f)
;;           ((eq? item (car list)) #t)
;;           (else (memq? item (cdr list))))))

(defun union-helper (lst result)
 (if (null? lst)
  result
  (let* ((current (car lst))
         (new-result (if (memq? current result)
                      result
                      (cons current result))))
   (union-helper (cdr lst) new-result))))

(defun union (list1 list2)
 (union-helper list2 (union-helper list1 '())))


(defun memq? (item list)
  (cond ((null? list) nil)
        ((eq? item (car list)) t)
        (t (memq? item (cdr list)))))

(defun union (list1 list2)
  (let* ((union1 (reduce (lambda (acc x) (if (memql? x acc) acc (cons x acc))) '() list1)))
    (reduce (lambda (acc x) (if (memql? x acc) acc (cons x acc))) union1 list2)))

(display (union '(1 2 3 4) '(4 5 2 2)))





;; (log-eval t)

;; (union '(1 2 3) '(a b c))
