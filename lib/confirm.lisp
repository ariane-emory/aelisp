(report-time-us "def require-equal and test     "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (ignore
  "An old version of require-equal that we are no longer using."

  "Signal an error unless VAL is equal? to TEST-VAL, otherwise return the result"
  "of evaluating EXPR."
  (defun require-equal (test-val val)
   (if (equal? test-val val)
    val
    (error "require-equal failed: " test-val " ≠ " val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro require-equal (test-val expr)
  "Signal an error unless EXPR evaluates to TEST-VAL, otherwise return the"
  "result of evaluating EXPR."
  $('let $($('val expr))
    $('if $('equal? test-val 'val)
      'val
      $('error
        $('concat
          '"require-equal failed: "
          $('string test-val)
          '" ≠ "
          $('string 'val))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! confirm-2nd-column 70)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmacro confirm (that expr returns expected)
  "Test whether EXPR evaluates to EXPECTED."
  (unless (eq? 'that   that)     (error "expected 'that as 2nd argument"))
  (unless (eq? 'returns returns) (error "expected 'returns as 4th argument"))
  $('progn
    $('let $($('printed $('princ $('string $('quote expr)))))
      $('while $('< 'printed confirm-2nd-column)
        $('princ '" ")
        $('setq! 'printed $('+ 1 'printed)))
      $('princ '" ⇒ ")
      (when (cons? expected)
       $('princ "'"))
      $('write $('require-equal
                 expected
                 expr))
      $('nl))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'confirm)
