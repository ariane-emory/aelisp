(report-time-us "def plist funs                 " 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun plist-keys (plist)
  "Extracts the keys from a plist PLIST."
  (unless (list? plist)          (error "PLIST must be a list"))
  (when plist (cons (car plist) (plist-keys (cddr plist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun plist-values (plist)
  "Extracts the values from a plist PLIST."
  (when plist (plist-keys (cdr plist))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun plist-remove (pred? prop plist)
  (unless (list? plist)          (error "PLIST must be a list"))
  (unless (even? (length plist)) (error "PLIST must have an even number of elements"))
  (when plist
   (if (pred? prop (car plist))
    (plist-remove pred? prop (cddr plist))
    (cons (car plist) (cons (cadr plist) (plist-remove pred? prop (cddr plist)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun plist-removeq  (prop plist) (plist-remove eq?  prop plist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun plist-removeql (prop plist) (plist-remove eql? prop plist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'plist-funs)
