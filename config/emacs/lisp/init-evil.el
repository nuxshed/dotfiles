;; -*- lexical-binding: t; -*-

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(setq evil-cross-lines t
      evil-move-beyond-eol t
      evil-symbol-word-search t
      evil-want-Y-yank-to-eol t
      evil-cross-lines t)

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ;; General
    ".f" 'consult-isearch
    ".q" 'delete-frame
    ".e" 'eval-region
    ;; Files
    "fr" 'consult-recent-file
    "fb" 'consult-bookmark
    "ff" 'find-file
    "fd" 'treemacs-select-directory
    "ft" 'treemacs
    ;; Search
    "sr" 'consult-ripgrep
    "sf" 'consult-fd
    ;; Org
    "oa" 'org-agenda
    "fh" 'consult-org-heading
    ;; Open
    "os" 'eshell
    ;; vterm
    "tt" 'vterm-toggle
    ;; Toggle
    "tn" 'display-line-numbers-mode
    ;; Notes
    "no" 'deft
    "nf" 'deft-find-file
    "nn" 'deft-new-file-named
    ;; Bufffers
    "bd" 'kill-current-buffer
    "bb" 'consult-buffer
    "bx" 'switch-to-scratch
    "bi" 'ibuffer
    "b]" 'next-buffer
    "b[" 'previous-buffer
    ;; Windows
    "wv" 'split-window-right
    "wh" 'split-window-below
    "wt" 'window-split-toggle
    "ws" 'ace-window
    ;; Packages
    "pi" 'package-install
    "pd" 'package-delete
    "pu" 'package-upgrade-all
    "pr" 'package-refresh-contents
    "pc" 'package-autoremove
    ;; Help
    "hh" 'help
    "hk" 'describe-key
    "hv" 'describe-variable
    "hF" 'describe-function
    "hf" 'describe-face
    "hs" 'describe-symbol
    "hm" 'describe-mode))

;; not evil, but whatever
(global-set-key (kbd "<M-right>") 'next-buffer)
(global-set-key (kbd "<M-left>") 'previous-buffer)

(provide 'init-evil)
