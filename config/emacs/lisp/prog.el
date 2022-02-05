;; -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook 'electric-pair-local-mode)

(use-package yasnippet
  :hook (org-mode . yas-global-mode))

(use-package clojure-mode :mode "\\.clj\\'")
(use-package nix-mode :mode "\\.nix\\'")
(use-package lua-mode :mode "\\.lua\\'")

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package lispy
  :hook ((emacs-lisp-mode clojure-mode) . lispy-mode))

(use-package cider :defer t)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package format-all
  :commands (format-all-buffer))

(provide 'prog)
;;; prog.el ends here
