;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string funss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-string (size init-val)
 "Make a new string of length SIZE with it's chars set to INIT-VAL."
 (unless (integer? size)         (error "SIZE must be an integer."))
 (unless (string? init-val)      (error "INIT-VAL must be a string."))
 (unless (= 1 (length init-val)) (error "INIT-VAL must be a string of length 1."))
 (apply concat (make-list size init-val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pad-string-right (size init-val str)
 "Pad STR to SIZE with INIT-VAL."
 (unless (integer? size)         (error "SIZE must be an integer."))
 (unless (string? init-val)      (error "INIT-VAL must be a string."))
 (unless (= 1 (length init-val)) (error "INIT-VAL must be a string of length 1."))
 (let ((pad (make-string (- size (length str)) init-val)))
  (concat str pad)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pad-string-left (size init-val str)
 "Pad STR to SIZE with INIT-VAL."
 (unless (integer? size)         (error "SIZE must be an integer."))
 (unless (string? init-val)      (error "INIT-VAL must be a string."))
 (unless (= 1 (length init-val)) (error "INIT-VAL must be a string of length 1."))
 (let ((pad (make-string (- size (length str)) init-val)))
  (concat pad str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun left-justify  (size str) (pad-string-right size " " str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun right-justify (size str) (pad-string-left size " " str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'string-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
