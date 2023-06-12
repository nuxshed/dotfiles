;; -*- lexical-binding: t; -*-

(defun mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
	  (- (window-width)
	    (+ (length (format-mode-line left))
	      (length (format-mode-line right))))))
    (append left
      (list (format (format "%%%ds" available-width) ""))
      right)))

(setq-default mode-line-format
  '((:eval (mode-line-render
	     '((:eval (propertize " %b" 'face `(:slant italic)))
	       (:eval (if (and buffer-file-name (buffer-modified-p))
			  (propertize "*" 'face `(:inherit face-faded))))
	       (:eval (if (buffer-narrowed-p)
			 (propertize "-" 'face `(:inherit face-faded)))))
	     '("%p %l:%c "
	       (:eval (propertize " %m" 'face 'font-lock-string-face)))))))

(provide 'modeline)
;;; modeline.el ends here
