;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "./lisp" user-emacs-directory))

(setq user-full-name "nuxsh"
      user-mail-address "nuxshed@gmail.com")

(setq-default require-final-newline t
              vc-follow-symlinks)

(setq undo-limit 80000000            ; moar undo
      auto-save-default t            ; who knows what could happen?
      truncate-string-ellipsis "…")  ; prettier ellipsis

;; scrolling related settings
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      smooth-scroll-margin 1)

(defalias 'yes-or-no-p 'y-or-n-p) ;; y/n is shorter than yes/no

(setq inhibit-startup-echo-area-message t
      inhibit-startup-message t)
(tool-bar-mode -1)    ; Disable the toolbar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Give some breathing room
(menu-bar-mode -1)    ; Disable the menu bar
(scroll-bar-mode -1)  ; Disable the scrollbar

(setq frame-inhibit-implied-resize t
      initial-major-mode 'fundamental-mode
      file-name-handler-alist nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(global-display-line-numbers-mode) ;; line numbers
(global-hl-line-mode) ;; highlight current line
(global-visual-line-mode) ;; wrap lines
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
(customize-set-variable 'package-enable-at-startup nil)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package gcmh
  :init
  (gcmh-mode 1))

(use-package evil
  :defer t
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
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ;; General
    ".f" 'consult-isearch
    ".q" 'delete-frame
    ".e" 'eval-region
    ".s" 'straight-use-package
    ;; Files
    "fr" 'consult-recent-file
    "fb" 'consult-bookmark
    "ff" 'find-file
    "fd" 'dired
    ;; Open
    "oa" 'org-agenda
    "om" 'mu4e
    "og" 'magit
    ;; Notes
    "no" 'deft
    "nf" 'deft-find-file
    "nn" 'deft-new-file-named
    ;; Bufffers
    "bv" 'split-window-right
    "bh" 'split-window-below
    "bd" 'kill-current-buffer
    "bb" 'consult-buffer
    "bx" 'switch-to-scratch
    "bi" 'ibuffer
    ;; Help
    "hh" 'help
    "hk" 'describe-key
    "hv" 'describe-variable
    "hf" 'describe-function
    "hs" 'describe-symbol
    "hm" 'describe-mode))

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
  :defer t
  :ensure t)

(use-package which-key
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-variable-pitch-cursor "block"
        mixed-pitch-set-height t))

(use-package nix-mode)
(use-package lua-mode)
(use-package markdown-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-elisp-load-path 'inherit)
  (setq flycheck-idle-change-delay 1.0)
  (setq-local flycheck-elisp-initialize-packages t)
  (setq-local flycheck-elisp-package-user-dir package-user-dir)
  (setq-default flycheck-disabled-checkers '(elisp-checkdoc)))

(use-package projectile
  :defer t
  :config (projectile-mode 1))

(use-package magit
  :defer t)

(with-eval-after-load 'dired
  (setq dired-dwim-target t
        dired-listing-switches "-Alh"
        dired-use-ls-dired t
        dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\|\\`[.].*\\'"
        dired-always-read-filesystem t
        dired-create-destination-dirs 'ask
        dired-hide-details-hide-symlink-targets nil
        dired-isearch-filenames 'dwim)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'dired-omit-mode)

(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq all-the-icons-dired-monochrome 'nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(with-eval-after-load 'ibuffer
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil)
  (defun my/human-readable-file-sizes-to-bytes (string)
    "Convert a human-readable file size into bytes."
    (interactive)
    (cond
     ((string-suffix-p "G" string t)
      (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "M" string t)
      (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "K" string t)
      (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
     (t
      (string-to-number (substring string 0 (- (length string) 1))))))

  (defun my/bytes-to-human-readable-file-sizes (bytes)
    "Convert number of bytes to human-readable file size."
    (interactive)
    (cond
     ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
     ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
     ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
     ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
     ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
     (t (format "%10d" bytes))))

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size"
           :inline t
           :summarizer
           (lambda (column-strings)
             (let ((total 0))
               (dolist (string column-strings)
                 (setq total
                       (+ (float (my/human-readable-file-sizes-to-bytes string))
                          total)))
               (my/bytes-to-human-readable-file-sizes total)))
           )
    (my/bytes-to-human-readable-file-sizes (buffer-size)))

(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 20 20 :left :elide)
              " "
              (size-h 11 -1 :right)
              " "
              (mode 16 16 :left :elide))
        (mark " "
              (name 16 -1)
              " " filename))))

(setq ibuffer-saved-filter-groups
      '(("main"
         ("modified" (and
                      (modified . t)
                      (visiting-file . t)))
         ("term" (or
                  (mode . vterm-mode)
                  (mode . eshell-mode)
                  (mode . term-mode)
                  (mode . shell-mode)))
         ("planning" (or
                      (name . "^\\*Calendar\\*$")
                      (name . "^diary$")
                      (mode . org-agenda-mode)))
         ("img" (mode . image-mode))
         ("config" (filename . "/dotfiles/"))
         ("site" (filename . "/projects/site/"))
         ("code" (filename . "/projects/"))
         ("notes" ( filename . "/notes/"))
         ("org" (mode . org-mode))
         ("dired" (mode . dired-mode))
         ("help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")
                     (mode . help-mode)))
         ("internal" (name . "^\*.*$"))
         ("other" (name . "^.*$"))
         )))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "main")))

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

(set-face-attribute 'default nil :font "Cascadia Code 10")
(set-face-attribute 'fixed-pitch nil :font "Cascadia Code 10")
(set-face-attribute 'variable-pitch nil :font "IBM Plex Sans 10")

(use-package all-the-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-cafe t))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

(require 'splash)
(splash-screen)

(use-package good-scroll
  :config
  (good-scroll-mode 1))

(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/.mail/"))

(setq mu4e-drafts-folder "/Gmail/[Gmail]/Drafts")
(setq mu4e-sent-folder   "/Gmail/[Gmail]/Sent Mail")
(setq mu4e-trash-folder  "/Gmail/[Gmail]/Trash")

(setq mu4e-get-mail-command "mbsync -a"
      mu4e-compose-signature-auto-include nil
      mu4e-compose-format-flowed t)

(setq
 user-mail-address "nuxshed@gmail.com"
 user-full-name  "nuxsh")

(setq mu4e-view-show-images t)

(setq smtpmail-smtp-server "smtp.gmail.com"
      user-mail-address "nuxshed@gmail.com"
      smtpmail-smtp-user "nuxshed"
      smtpmail-smtp-service 587)

(setq smtpmail-auth-credentials (expand-file-name "~/.authinfo"))

(use-package org-contrib)
(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(setq org-src-window-setup 'split-window-below
      org-agenda-window-setup 'split-window-below)

(setq org-agenda-files '("~/org/agenda.org"))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "q") 'org-agenda-exit)))
(use-package htmlize)
(add-hook 'org-mode-hook #'toggle-word-wrap)

(use-package deft
  :config
  (setq deft-directory "~/notes"
        deft-default-extension "org"
        deft-extensions '("txt" "md" "org")
        deft-use-filter-string-for-filename t))

(add-hook 'deft-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "n") 'deft-new-file-named)
            (define-key evil-normal-state-local-map (kbd "q") 'quit-window)
            (define-key evil-normal-state-local-map (kbd "f") 'deft-find-file)))

(defun sitemap-format-entry (entry style project)
  (format "
      [[file:%s][%s]]
      #+begin_article-info
      #+begin_date
      Last Modified %s
      #+end_date
      #+end_article-info"
      entry
              (org-publish-find-title entry project)
      (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
      (org-publish-find-property entry :keywords project 'html)
      (org-publish-find-property entry :description project 'html)))
(setq org-publish-project-alist
      '(
        ("blog"
         :base-directory "~/projects/site/org/"
         :base-extension "org"
         :publishing-directory "~/projects/site/blog/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-preamble "<div class=\"links\"><a href=../index.html>Home</a><a href=\"\" class=\"active\">Blog</a><a href=../about.html>About</a></div>"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/style.css\" />"
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Recent"
         :sitemap-format-entry sitemap-format-entry
         :sitemap-sort-files anti-chronologically)
        ("emacs-config"
         :base-directory "~/dotfiles/config/emacs/"
         :base-extension "org"
         :publishing-directory "~/projects/site/blog/"
         :recursive nil
         :publishing-function org-html-publish-to-html)))

(setq org-html-postamble nil)
(setq org-export-preserve-breaks t)

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
          ("#+END_QUOTE" . "❞"))))
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
  '((elisp . t)))

(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/config/emacs/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; tangle on save
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

;; init.el ends here
