;; -*- lexical-binding: t; -*-

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package clojure-mode :mode "\\.clj\\'")
(use-package nix-mode :mode "\\.nix\\'")
(use-package lua-mode :mode "\\.lua\\'")

(use-package lispy
  :hook ((emacs-lisp-mode clojure-mode) . lispy-mode))

(use-package cider
  :commands cider-jack-in)

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package consult-flycheck
  :after flycheck-mode)

(use-package format-all
  :commands (format-all-buffer))

(provide 'prog)
;;; prog.el ends here
