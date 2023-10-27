(require 'std-minimal)
(require 'measure-time)

(report-time-us "def union                      "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (unions):                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun union2 (equalityp? lst1 lst2)
  "Return the union of LST1 and LST2, using memp? to test for duplicates."
  (unless (fun? equalityp?) (error "EQUALITYP? must be a function"))
  (unless (list? lst1)      (error "LST1 must be a list"))
  (unless (list? lst2)      (error "LST2 must be a list"))
  (let* ((memp?    (make-member-pred equalityp?))
         (combine  (lambda (acc x) (if (memp? x acc) acc (cons x acc))))
         (union1   (reduce combine lst1 '())))
   (reduce combine lst2 union1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! union2q
  (lambda (lst1 lst2)
   "Return the union of LST1 and LST2, using eq? to test for duplicates."
   (unless (list? lst1) (error "LST1 must be a list"))
   (unless (list? lst2) (error "LST2 must be a list"))
   (union2 eq? lst1 lst2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! unionq
  (reduced* union2q))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! union2ql
  (lambda (lst1 lst2)
   "Return the union of LST1 and LST2, using eql? to test for duplicates."
   (unless (list? lst1) (error "LST1 must be a list"))
   (unless (list? lst2) (error "LST2 must be a list"))
   (union2 eql? lst1 lst2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! unionql
  (reduced* union2ql))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'union)
