;; init.el -*- lexical-binding: t; -*-

(setq user-full-name "nuxsh"
      user-mail-address "nuxshed@gmail.com")

(setq-default require-final-newline t
              vc-follow-symlinks)

(setq undo-limit 80000000            ; moar undo
    auto-save-default t            ; who knows what could happen?
    truncate-string-ellipsis "…")   ; prettier ellipsis

;; scrolling related settings
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      smooth-scroll-margin 1)

(defalias 'yes-or-no-p 'y-or-n-p) ;; y/n is shorter than yes/no

(setq inhibit-startup-echo-area-message t
      inhibit-startup-message t)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(scroll-bar-mode -1)        ; Disable the scrollbar

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(global-display-line-numbers-mode) ;; line numbers
(global-subword-mode) ;; iterate through camelCase
(electric-pair-mode) ;; autopairs
(recentf-mode) ;; recent files

(setq-default indent-tabs-mode nil
    tab-width 2)
(setq indent-line-function 'insert-tab)

;; init package sources
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package consult)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

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

(use-package vterm
  :ensure t)

(use-package which-key
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))

(use-package nix-mode)
(use-package lua-mode)
(use-package markdown-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-idle-change-delay 1.0)
        (setq-local flycheck-emacs-lisp-initialize-packages t)
        (setq-local flycheck-emacs-lisp-package-user-dir package-user-dir)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package projectile
  :config (projectile-mode 1))

(use-package magit)

(set-window-margins (selected-window) 10 10)

(use-package doom-themes
  :config
  (load-theme 'doom-nord t))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

(use-package org-contrib)
(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(defun org/prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
      '(("#+begin_src" . "")
        ("#+BEGIN_SRC" . "")
        ("#+end_src" . "")
        ("#+END_SRC" . "")
        ("#+begin_example" . "")
        ("#+BEGIN_EXAMPLE" . "")
        ("#+end_example" . "")
        ("#+END_EXAMPLE" . "")
        ("#+results:" . "")
        ("#+RESULTS:" . "")
        ("#+begin_quote" . "❝")
        ("#+BEGIN_QUOTE" . "❝")
        ("#+end_quote" . "❞")
        ("#+END_QUOTE" . "❞")
        ("[ ]" . "☐")
        ("[-]" . "◯")
        ("[X]" . "☑"))))
(add-hook 'org-mode-hook 'org/prettify-set)

(defun prog/prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
      '(("lambda" . "λ")
        ("->" . "→")
        ("<-" . "←")
        ("<=" . "≤")
        (">=" . "≥")
        ("!=" . "≠")
        ("~=" . "≃")
        ("=~" . "≃"))))
(add-hook 'prog-mode-hook 'prog/prettify-set)

(global-prettify-symbols-mode)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))

(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/config/emacs/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; tangle on save
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

;; init.el ends here
