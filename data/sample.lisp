(defun union2 (memp? list1 list2)
 "Return the union of two lists."
  (let* ((combine (lambda (acc x) (if (memp? x acc) acc (cons x acc))))
         (union1 (reduce combine '() list1)))
    (reduce combine union1 list2)))

;; (defun union (memp? list1 list2)
;;   (let ((combine (reduced (lambda (acc x) (if (memp? x acc) acc (cons x acc))))))
;;     (combine list1 list2 '())))

;; ;; (defun reduced (fun init)
;; ;;   "Return a function that is a reduction of the binary function fun with initial value init."
;; ;;   (lambda lsts (reduce fun init lsts)))

;; (defun union (memp? list1 list2)
;;   (let ((combine (reduced (lambda (acc x) (if (memp? x acc) acc (cons x acc))) '())))
;;     (combine list1 list2)))

;; (defun union (memp? list1 list2)
;;   (let ((combine (lambda (acc x)
;;                    (if (memp? x acc) acc (cons x acc)))))
;;     ((reduced combine) (cons list1 list2))))
;; (defun union (memp? list1 list2)
;;   (let ((combine (lambda (acc x)
;;                    (if (memp? x acc) acc (cons x acc)))))
;;     ((reduced combine) (append list1 list2))))

;;(log-eval t)
(display (union2 memql? '(1 2 3 4) '(4 5 2 2)))
(nl)

 




;; (union '(1 2 3) '(a b c))
