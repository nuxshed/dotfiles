;; init.el -*- lexical-binding: t; -*-

(package-initialize)
(require 'use-package)

(add-to-list 'load-path (expand-file-name "./lisp" user-emacs-directory))

(setq user-full-name "nuxsh"
      user-mail-address "nuxshed@gmail.com")

(setq-default require-final-newline t
              vc-follow-symlinks)

(setq undo-limit 80000000            ; moar undo
      auto-save-default t            ; who knows what could happen?
      truncate-string-ellipsis "…")  ; prettier ellipsis

(setq use-short-answers t)

(setq frame-inhibit-implied-resize t
      initial-major-mode 'fundamental-mode
      file-name-handler-alist nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(recentf-mode) ;; recent files
(electric-pair-mode) ;; autopairs

(use-package no-littering)
(use-package gcmh
  :init
  (gcmh-mode 1))

(require 'init-evil)
(require 'prog)
(require 'completion)
(require 'ui)
(require 'init-layout)
(require 'org)
(require 'prettify-symbols)

;; init.el ends here
