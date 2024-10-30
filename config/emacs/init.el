;; init.el -*- lexical-binding: t; -*-

(package-initialize)
(require 'use-package)

(add-to-list 'load-path (expand-file-name "./lisp" user-emacs-directory))

(setq user-full-name "nuxsh"
      user-mail-address "nuxsh@proton.me")

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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'init-evil)
(require 'init-org)
(require 'init-layout)
(require 'prettify-symbols)

(require 'ui)
(require 'prog)
(require 'completion)

;;; init.el ends here
