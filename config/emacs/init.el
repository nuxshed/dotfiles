;; init.el -*- lexical-binding: t; -*-

(package-initialize)
(require 'use-package)

(add-to-list 'load-path (expand-file-name "./lisp" user-emacs-directory))

(setq user-full-name "nuxsh"
      user-mail-address "nuxshed@gmail.com")

(setq use-short-answers t
      frame-inhibit-implied-resize t
      initial-major-mode 'fundamental-mode
      file-name-handler-alist nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package gcmh
  :init
  (gcmh-mode 1))

(recentf-mode 1)
(setq recentf-max-saved-items 25)

(setq make-backup-files nil)
(setq-default truncate-lines t)

(require 'init-evil)
(require 'init-org)
(require 'init-layout)
(require 'ui)
(require 'prog)
(require 'completion)
(require 'prettify-symbols)

;;; init.el ends here
