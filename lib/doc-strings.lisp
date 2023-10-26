(report-time-us "def documentation funs         "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun doc (obj)
  "Get an object OBJ's documentation."
  (let* ((doc       (or (get :doc obj) "This object has no documentation."))
         (binding   (get :last-bound-to obj))
         (is-fun    (or (lambda? obj) (macro? obj)))
         (name      (or binding (string obj)))
         (params    (when is-fun
                     (string (cons name (params obj)))))
         (docstring (if is-fun
                     (concat params ": " doc)
                     (concat name   ": " doc))))
   docstring))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'doc-strings)
