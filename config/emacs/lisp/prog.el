;; -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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

(use-package format-all
  :commands (format-all-mode))

(use-package lsp-mode)
(use-package lsp-ui)

(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'lua-mode-hook #'lsp-deferred)

(setq lsp-clients-clangd-args '("--query-driver=/nix/store/*/bin/*"
                                "--clang-tidy"
                                "--header-insertion=never"
                                "--completion-style=detailed"))

(provide 'prog)
;;; prog.el ends here
