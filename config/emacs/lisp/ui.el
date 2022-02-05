;; -*- lexical-binding: t; -*-

;; Dired
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
(setq all-the-icons-dired-monochrome 't)

;; Ibuffer
(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 20 20 :left :elide)
              " "
              (size-h 11 -1 :right)
              " "
              (mode 16 16 :left :elide))
        (mark " "
              (name 16 -1)
              " " filename)))

(setq ibuffer-show-empty-filter-groups nil)
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
		      (name . "^agenda")
		      (mode . org-agenda-mode)))
	 ("img" (mode . image-mode))
	 ("config" (filename . "/dotfiles/"))
	 ("blog" (filename . "/projects/site/"))
	 ("code" (or (filename . "/projects/")
		     (filename . "/code/")))
	 ("notes" ( filename . "/notes/"))
	 ("org" (mode . org-mode))
	 ("dired" (mode . dired-mode))
	 ("help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*")
		     (mode . help-mode)))
	 ("internal" (name . "^\*.*$"))
	 ("other" (name . "^.*$")))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "main")))

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

;; Appearance
(set-face-attribute 'default nil :font "Cartograph CF 10")
(set-face-attribute 'fixed-pitch nil :font "Cartograph CF 10")
(set-face-attribute 'variable-pitch nil :font "Commissioner 10")

(use-package all-the-icons :defer t)

;; Theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(use-package doom-themes
  :config
  (load-theme 'doom-kurai t))

(fringe-mode 10)
(add-to-list 'default-frame-alist '(internal-border-width . 24))

(require 'modeline)
(require 'splash)
(splash-screen)

(use-package good-scroll
  :config
  (good-scroll-mode 1))

(use-package consult
  :after vertico)

(use-package vterm
  :commands (vterm vterm-other-window))

(use-package which-key
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))

(provide 'ui)
;;; ui.el ends here
