;; -*- lexical-binding: t; -*-

(use-package org-contrib)
(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode))

(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
(setq org-agenda-files '("~/org/agenda.org"))

(setq org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-hide-macro-markers t
      org-link-descriptive t)

(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (local-set-key (kbd "q") 'org-agenda-exit)))
(use-package htmlize)
(add-hook 'org-mode-hook (lambda ()
			   (toggle-truncate-lines)
			   (flyspell-mode t)
			   (electric-indent-local-mode -1)))
(setq org-src-window-setup 'current-window)

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

(require 'publish)

(provide 'org)
;;; init-org.el ends here
