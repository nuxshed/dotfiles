;; -*- lexical-binding: t; -*-

(use-package corfu
  :init (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package vertico)

(setq global-corfu-minibuffer
      (lambda ()
        (not (or (bound-and-true-p mct--active)
                 (bound-and-true-p vertico--input)
                 (eq (current-local-map) read-passwd-map)))))
(setq corfu-auto t
      corfu-quit-no-match 'separator) ;; or t

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

(use-package kind-icon
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(setq kind-icon-default-style 
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :scale 0.5))

(use-package hotfuzz
  :config
  (setq completion-styles '(hotfuzz)))

(defvar +hotfuzz--is-empty nil)
(defun +hotfuzz-all-completions--enable-history-a (orig content &rest args)
  "Set a variable needed for showing most recent entries."
  (setq +hotfuzz--is-empty (string-empty-p content))
  (apply orig content args))
(advice-add #'hotfuzz-all-completions
            :around #'+hotfuzz-all-completions--enable-history-a)
(defun +hotfuzz--adjust-metadata--enable-history-a (orig metadata)
  "Enable showing most recent entries for empty input."
  (if +hotfuzz--is-empty
      metadata
    (funcall orig metadata)))
(advice-add #'hotfuzz--adjust-metadata
            :around #'+hotfuzz--adjust-metadata--enable-history-a)

(use-package marginalia)

(vertico-mode)
(marginalia-mode)

;; snippets
(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))

  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
)

(use-package tempel-collection)

(provide 'completion)
