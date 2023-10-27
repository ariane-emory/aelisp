;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple std load time measuerement:                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! before (now-us))
(setq! print-load-time-us
 (lambda ()
  (princ "Loaded in ")
  (princ (elapsed-us before))
  (princ " us.")
  (nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'map)

(mapc* require
 'core-aliases
 'compound-cars-and-cdrs
 'quasiquote
 'numbered-access
 'reduce
 'append
 'nconc
 'push-funs
 'flatten
 'zip
 'tail-chaser-macros
 'transform
 'sort
 'list-funs
 'union
 'predicates
 'log-toggle
 'vector-lists
 'split-list
 'print-macros
 'std-misc
 'tail-chaser-funs
 'selection-sort
 'delq
 'doc-strings
 'primes
 'confirm
 'strings
 'benchmark
 'prop-removeb
 'prog-macros
 'plist-funs
 'prop-removeb
 'scheme-compat
 'elisp-compat
 'std-aliases)

(set! keys pkeys)
(set! core-vals vals)
(defun vals (o) (if (env? o) (core-vals o) (pvals o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print-load-time-us)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'std)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
