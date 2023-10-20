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

;; (defun union-helper (lst result)
;;  (if (null? lst)
;;   result
;;   (let* ((current (car lst))
;;          (new-result (if (memq? current result)
;;                       result
;;                       (cons current result))))
;;    (union-helper (cdr lst) new-result))))

;; (defun union (list1 list2)
;;  (union-helper list2 (union-helper list1 '())))

(defun union (test? lst1 lst2)
  (if (null? lst1)
      lst2
      (if (apply memq? lst1 lst2)
          (union test? (cdr lst1) lst2)
          (cons (car lst1) (union test? (cdr lst1) lst2)))))

(defun mem? (test? elem lst)
  (cond ((null? lst) nil)
        ((test? elem (car lst)) t)
        (t (memq? test? elem (cdr lst)))))



(defun union (memp? list1 list2)
 (let* ((union1 (reduce
                 (lambda (acc x) (if (memp? x acc)
                                  acc
                                  (cons x acc)))
                 nil list1)))
  (reduce
   (lambda (acc x)
            (if (memp? x acc)
             acc
             (cons x acc)))
    union1 list2)))

(defun union (memp? list1 list2)
  (let* ((combine (reduced (lambda (acc x) (if (memp? x acc) acc (cons x acc)))))
         (union1 (combine '() list1)))
    (combine union1 list2)))

(log-eval t)
(display (union memql? '(1 2 3 4) '(4 5 2 2)))


 




;; (union '(1 2 3) '(a b c))
