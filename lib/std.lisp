;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "Loading split std...")
(nl)

(require 'std-fundamental)

(when *log-loading-std-enabled*
 (log-eval t)
 (log-core t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *std-mode*
 (let ((module
        (list
         'basic-funs
         'core-aliases
         'compound-cars-and-cdrs
         'append-and-nconc
         'quasiquote
         'list-access-funs
         'tail-chaser-macros
         'reduce
         'map
         'zip
         'push-pop-funs
         'flatten
         'transform
         'merge-sort
         'list-member
         'list-index
         'list-remove
         'union
         'misc-list-funs
         'misc-predicates
         'log-toggle
         'print-macros
         'benchmark
         'unsorted-funs
         'vector-list
         'split-list
         'delq
         'prime-funs
         'selection-sort
         'doc
         'test-macros
         'string-funs
         'prog-macros
         'plist-funs
         'remove-prop-macro
         'scheme-compat-aliases
         'elisp-compat-aliases
         'std-aliases
         )))
  (while module
   (require (car module))
   (setq! module (cdr module)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'std)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(princ "Loaded in   ")
(princ (elapsed-us *time-before-loading-std*))
(princ " us.")
(nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
