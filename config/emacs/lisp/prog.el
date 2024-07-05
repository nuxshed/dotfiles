;; -*- lexical-binding: t; -*-

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package clojure-mode :mode "\\.clj\\'")
(use-package nix-mode :mode "\\.nix\\'")
(use-package lua-mode :mode "\\.lua\\'")
(use-package rust-mode :mode "\\.rs\\'"
  :init (setq rust-mode-treesitter-derive t))

(use-package direnv
 :config
 (direnv-mode))

(use-package lispy
  :hook ((emacs-lisp-mode clojure-mode) . lispy-mode))

(use-package cider
  :commands cider-jack-in)

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package flycheck-rust)

(use-package consult-flycheck
  :after flycheck-mode)

(use-package format-all
  :commands (format-all-buffer))

(use-package lsp-mode)
(use-package lsp-ui :commands lsp-ui-mode)

(provide 'prog)
;;; prog.el ends here
