;; -*- lexical-binding: t; -*-

(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-select-next)))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-scrollbar nil))

(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'company-completion-started-hook
          (lambda (&rest _)
            (setq-local lsp-inhibit-lsp-hooks t)
            (lsp--capf-clear-cache))
          nil
          t)

(use-package vertico)

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

(provide 'completion)
