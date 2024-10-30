;; -*- lexical-binding: t; -*-

;; Dired
(with-eval-after-load 'dired
  (setq dired-dwim-target t
        dired-listing-switches "-Alh"
        dired-use-ls-dired t
        dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\|\\`[.].*\\'"
        dired-always-read-filesystem t
        dired-hide-details-hide-symlink-targets nil
        dired-isearch-filenames 'dwim))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; Ibuffer
(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 30 30 :left :elide)
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
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons :defer t)


;; Theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(use-package doom-themes
  :config
  (load-theme 'doom-blackout t))

(setq display-line-numbers-type 'relative)

(fringe-mode 10)

(require 'modeline)

    (require 'splash)
  (splash-screen)

  (use-package good-scroll
    :config
    (good-scroll-mode 1))

  (use-package vterm
    :commands (vterm vterm-other-window))

  (use-package vterm-toggle)

  ;; Treemacs
  (use-package treemacs :defer t)
  (use-package treemacs-evil
    :after (treemacs evil)
    :ensure t)
  (use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once)
    :ensure t)

(use-package consult
  :after vertico)


(use-package which-key
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))

(provide 'ui)
;;; ui.el ends here
