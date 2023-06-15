#!/usr/local/bin/emacs --script
(defun print-if-string (x)
       (cond ((stringp x) (print x))
             (t (error "Invalid argument %s in add-on" x))))
(defun print-elements-of-list (list)
 "Print each element of LIST on a line of its own."
 (while list
   (print-if-string (car list))
   (setq list (cdr list))))
(load "~/.emacs.d/recentf")
(print-elements-of-list recentf-list)
