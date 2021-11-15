;; init.el -*- lexical-binding: t; -*-

(setq inhibit-startup-message t)

(setq inhibit-startup-echo-area-message t
      inhibit-startup-message t)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(scroll-bar-mode -1)        ; Disable the scrollbar

(setq inhibit-splash-screen 'nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; useful modes
(global-display-line-numbers-mode) ;; line numbers
(electric-pair-mode) ;; autopairs

;; indents
(setq-default indent-tabs-mode nil
    tab-width 4)
(setq indent-line-function 'insert-tab)

;; init package sources
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
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
  (setq vertico-cycle t)
)

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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (setq evil-leader/leader "<SPC>")
  (evil-leader/set-key
    ;; Files
    "fr" 'consult-recent-file
    "fb" 'consult-bookmark
    "ff" 'find-file
    ;; Bufffers
    "bv" 'split-window-right
    "bh" 'split-window-below
    "bd" 'kill-current-buffer
    "bb" 'consult-buffer
    "bx" 'switch-to-scratch
    ;; Help
    "hh" 'help
    "hk" 'describe-key
    "hv" 'describe-variable
    "hf" 'describe-function
    "hs" 'describe-symbol
    "hm" 'describe-mode))

(use-package which-key
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))

(use-package nix-mode)
(use-package lua-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-idle-change-delay 1.0)
        (setq-local flycheck-emacs-lisp-initialize-packages t)
        (setq-local flycheck-emacs-lisp-package-user-dir package-user-dir)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Project Management
(use-package projectile
  :config (projectile-mode 1))

;; UI

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-day t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15))

;; Org

(use-package org-contrib)
(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(set-face-attribute 'org-headline-done nil :strike-through t)

;; TODO
;; - literate config
;; - more orgmode customization
;;   - org-roam
;;   - org-export
;; - magit
;; - custom modeline
;; - some custom themes

;; init.el ends here
