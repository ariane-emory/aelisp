(require 'prog-macros)
(require 'plist-funs)

(report-time-us "def prop remove!               "
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro remove! (prop obj)
  $('prog1
    $('quote $('get prop obj))
    $('props! obj $('plist-removeql prop $('props obj)))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'prop-removeb)
