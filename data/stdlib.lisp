#pragma once

(setq nl (lambda ()  (princ "
")))

(setq stop   (lambda ()  (terpri) (exit 0)))

(setq sleep  (lambda (x) (msleep (* 1000 x))))

;; (setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body))))  

;; (defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body)))
