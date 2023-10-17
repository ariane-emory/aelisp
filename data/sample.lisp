(setq! lst-base '(1 2 3 4 5 6 ((7 8 (unquote 9) 10 11 12 13))))
(setq! lst lst-base)

(write (transform lst integer? 2*))
(nl)

(princ "Done.")
(nl)
