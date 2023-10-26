(report-time "def prog1/prog2                "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro prog1 (expr1 . exprs)
  "Evaluate EXPR1, then evaluate EXPRS in order, and return the value of EXPR1."
  $('let $($('result $('eval expr1)))
    $('progn . exprs)
    'result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro prog2 (expr1 expr2 . exprs)
  "Evaluate EXPR1, then evaluate EXPR2, then evaluate EXPRS in order, and return the value of"
  "EXPR2."
  $('progn
    expr1
    $('let $($('result2 $('eval expr2)))
      $('progn . exprs)
      'result2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'prog-macros)
