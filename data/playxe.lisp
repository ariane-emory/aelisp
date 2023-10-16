;; https://github.com/mishoo/SLip/blob/f11078c94bbbbc5b2d55a9a3669edb17bf5bb8fc/tmp/t2.lisp#L9
;; also: https://www.brics.dk/NS/99/1/BRICS-NS-99-1.pdf
;;       https://github.com/maolonglong/bogoscheme/blob/main/primitives.scm#L147

(set! qq-expand-list
      (lambda (x)
        (if (consp x)
            (if (eq (car x) 'qq-unquote)
                (list 'list (cadr x))
                (if (eq (car x) 'qq-splice)
                    (cadr x)
                    (if (eq (car x) 'quasiquote)
                        (qq-expand-list (qq-expand (cadr x)))
                        (list 'list (list 'append
                                          (qq-expand-list (car x))
                                          (qq-expand (cdr x)))))))
            (list 'quote (list x)))))

(set! qq-expand
      (lambda (x)
        (if (consp x)
            (if (eq (car x) 'qq-unquote)
                (cadr x)
                (if (eq (car x) 'quasiquote)
                    (qq-expand (qq-expand (cadr x)))
                    (list 'append
                          (qq-expand-list (car x))
                          (qq-expand (cdr x)))))
            (list 'quote x))))

(defmacro quasiquote (thing)
  (qq-expand thing))
