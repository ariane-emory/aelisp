(require 'measure-time)

(report-time-us "def aliases for builtins       "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; simple aliases:                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! s       setq!)
 (setq! setcar! rplaca!)
 (setq! setcdr! rplacd!)
 (setq! ¬       not)          
 (setq! ∨       or )           
 (setq! ∧       and)
 (setq! setcdr! rplacd!)
 (setq! setcar! rplaca!)
 (setq! λ       lambda)
 (setq! lte     <=)
 (setq! gte     >=)
 (setq! lt      <)
 (setq! gt      >)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'core-aliases)
