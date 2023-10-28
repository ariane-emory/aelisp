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
