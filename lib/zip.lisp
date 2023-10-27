(require 'std-minimal)
(require 'flatten)
(require 'reduce)
(require 'map)

(report-time-us "def zipping funs               "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (zipping):                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun zip2 (lst1 lst2)
  "Zip LST1 and LST2."
  (unless (list? lst1) (error "LST1 must be a list"))
  (unless (list? lst2) (error "LST2 must be a list"))
  (cond
   ((∨ (nil? lst1) (nil? lst2)) nil)
   (t  (cons  $((car lst1) (car lst2))
        (zip2   (cdr lst1) (cdr lst2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun zip3 (l1 l2 l3)
  "Zip the three lists LST1, LST2 and LST3."
  (unless (list? l1) (error "LST1 must be a list"))
  (unless (list? l2) (error "LST2 must be a list"))
  (unless (list? l3) (error "LST3 must be a list"))
  (mapcar flatten1 (reduce zip2 $(l2 l3) l1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! left-nested-zip (reduced* zip2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro zip lists
  "Zip many lists. This might not flatten properly if the zipped elements are
  themselves lists."
  (unless (list? lists) (error "LISTS must be a list"))
  (if (cdr lists)
   $('mapcar 'flatten (cons 'left-nested-zip lists))
   $('mapcar 'list  (car lists))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'zip)
