(report-time-us "def scheme compat              "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; tiny-clos scheme compat:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! #f            nil)
 (setq! #t            t)
 (setq! ???           'unspecified-result)
 (setq! assoc         ahas?) 
 (setq! assq          aget) 
 (setq! collect-if    filter)
 (setq! define        setq!)
 (setq! display       write)
 (setq! else          t)
 (setq! every         all?)
 (setq! getl          pget)
 (setq! gsort         sort!!)
 (setq! make-vector   make-list)
 (setq! map           mapcar)
 (setq! map-append    mapcan)
 (setq! position-of   indexq)
 (setq! remove        removeq)
 (setq! set!          setq!) ;should should be a macro that avoids re-defining what-scheme-implementation
 (setq! vector-length list-length)
 (setq! vector-ref    list-ref)
 (setq! vector-set!   list-set!)
 (setq! null?         nil?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'scheme-compat)
