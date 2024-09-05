;; -*- lexical-binding: t; -*-

(defface mode-line-evil-normal
  '((t (:background "green" :foreground "black")))
  "Face for Evil normal state in the mode line."
  :group 'mode-line-faces)

(defface mode-line-evil-insert
  '((t (:background "red" :foreground "black")))
  "Face for Evil insert state in the mode line."
  :group 'mode-line-faces)

(defface mode-line-evil-visual
  '((t (:background "orange" :foreground "black")))
  "Face for Evil visual state in the mode line."
  :group 'mode-line-faces)

(defface mode-line-evil-motion
  '((t (:background "purple" :foreground "black")))
  "Face for Evil motion state in the mode line."
  :group 'mode-line-faces)

(defface mode-line-evil-replace
  '((t (:background "blue" :foreground "black")))
  "Face for Evil replace state in the mode line."
  :group 'mode-line-faces)

(defface mode-line-evil-operator
  '((t (:background "cyan" :foreground "black")))
  "Face for Evil operator state in the mode line."
  :group 'mode-line-faces)

(defun mode-line-evil-indicator ()
  "Return the Evil mode indicator with appropriate face."
  (let ((evil-mode-string
         (cond
          ((evil-normal-state-p) (propertize " NORMAL " 'face 'mode-line-evil-normal))
          ((evil-insert-state-p) (propertize " INSERT " 'face 'mode-line-evil-insert))
          ((evil-visual-state-p) (propertize " VISUAL " 'face 'mode-line-evil-visual))
          ((evil-motion-state-p) (propertize " MOTION " 'face 'mode-line-evil-motion))
          ((evil-replace-state-p) (propertize " REPLACE " 'face 'mode-line-evil-replace))
          ((evil-operator-state-p) (propertize " OPERATOR " 'face 'mode-line-evil-operator))
          (t (propertize " -" 'face 'mode-line)))))
    evil-mode-string))

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
            (list (mode-line-evil-indicator)
                  (propertize " %b" 'face '(:slant italic))
                  (if (and buffer-file-name (buffer-modified-p))
                      (propertize "*" 'face '(:inherit font-lock-warning-face)))
                  (if (buffer-narrowed-p)
                      (propertize "-" 'face '(:inherit font-lock-warning-face))))
            '(" %m | %p | %l:%c ")))))

(provide 'modeline)
;;; modeline.el ends here
