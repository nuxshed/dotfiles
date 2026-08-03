;; -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package clojure-mode :mode "\\.clj\\'")
(use-package nix-mode :mode "\\.nix\\'")
(use-package lua-mode :mode "\\.lua\\'")
(use-package rust-mode :mode "\\.rs\\'")
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package lispy
  :hook ((emacs-lisp-mode clojure-mode) . lispy-mode))

(use-package cider
  :commands cider-jack-in)

(use-package format-all
  :commands (format-all-mode))

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'lua-mode-hook #'eglot-ensure)

(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook (racket-mode . (lambda ()
                         (when (executable-find racket-program)
                           (racket-xp-mode 1)))))

(use-package nael
  :ensure t
  :hook (nael-mode . eglot-ensure))

(provide 'prog)
;;; prog.el ends here
