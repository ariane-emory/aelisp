;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; log toggle helpers, these should be replaced with macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun with-toggled-fun1 (toggled-fun)
 "Get a function that enables TOGGLED-FUN, evaluates FUN-OR-EXPR and sets"
 "TOGGLED-FUN back to it's prior state."
 (unless (fun? toggled-fun) (error "TOGGLED-FUN must be a function"))
 (lambda (fun-or-expr)
  (if (lambda? fun-or-expr)
   (let* ((old    (toggled-fun t))
          (result (fun-or-expr))
          (new    (toggled-fun old)))
    result)
   (let* ((old    (toggled-fun t))
          (result (eval fun-or-expr))
          (new    (toggled-fun old)))
    result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun with-toggled-fun (toggled-fun)
 "Get a function that enables TOGGLED-FUN, evaluates FUN-OR-EXPRS and sets"
 "TOGGLED-FUN back to it's prior state."
 (unless (fun? toggled-fun) (error "TOGGLED-FUN must be a function"))
 (lambda funs-or-exprs
  (last (mapcar (with-toggled-fun1 toggled-fun) funs-or-exprs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! with-log-eval  (with-toggled-fun log-eval))
(setq! with-log-core  (with-toggled-fun log-core))
(setq! with-log-macro (with-toggled-fun log-macro))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'log-toggle-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
