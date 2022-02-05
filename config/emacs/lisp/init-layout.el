;; -*- lexical-binding: t; -*-

(setq display-buffer-alist
      '(
	("*Help*"
	 (display-buffer-in-side-window)
	 (window-width 0.25)
	 (side . right)
	 (slot . 0))
	("\\*e?shell.*"
	 (display-buffer-in-side-window)
	 (window-height . 0.35)
	 (side . bottom)
	 (slot . -1))))

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "wont work lol")
    (let ((func (if (window-full-height-p)
		    #'split-window-vertically
		  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
	(other-window 1)
	(switch-to-buffer (other-buffer))))))

;; got this off of some random gist from 2012
(defun toggle-split-view ()
  "Toggle between split window and single window."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (progn
	(if (< 1 (count-windows))
	    (progn
	      (window-configuration-to-register ?u)
	      (delete-other-windows))
	  (jump-to-register ?u))))
  (my-iswitchb-close))

(define-key global-map (kbd "C-`") 'toggle-split-view)

;; iswitchb hasn't been included in emacs 24.4, so i have no idea what this does
(defun my-iswitchb-close()
 "Open iswitchb or, if in minibuffer go to next match."
 (interactive)
 (if (window-minibuffer-p (selected-window))
    (keyboard-escape-quit)))

(use-package ace-window
  :defer t)

(provide 'init-layout)
;;; init-layout.el ends here
