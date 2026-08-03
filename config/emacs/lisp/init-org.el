;; -*- lexical-binding: t; -*-

(use-package org-contrib)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-block-name nil
        org-modern-star nil))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(setq org-agenda-files '("~/org/agenda.org"))

(setq org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-hide-macro-markers t
      org-link-descriptive t
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-edit-src-content-indentation 0)

(use-package htmlize)

(require 'org-tempo)

(add-hook 'org-mode-hook (lambda ()
                           (visual-line-mode 1)
                           (org-indent-mode 1)
                           (electric-indent-local-mode -1)
                           (setq-local completion-at-point-functions
                                       (remove 'ispell-completion-at-point
                                               completion-at-point-functions))))

(setq org-src-window-setup 'current-window)

(setq org-capture-templates
      '(("n" "Note" entry (file "~/notes/inbox.org")
         "* %?\n%U\n")
        ("l" "Lecture" plain (file "~/notes/lectures.org")
         "#+title: %^{Title}\n\n* Intro\n\n* Admin\n")))

(defun my/org-transclusion-open ()
  "Open source file of transclusion at point, falling back to org-open-at-point."
  (interactive)
  (condition-case nil
      (org-transclusion-open-source)
    (error (org-open-at-point))))

(defun my/org-transclusion-new (path)
  "Create file at PATH (with parent dirs), insert a transclusion link, and open file."
  (interactive "FNew file: ")
  (let ((path (expand-file-name path)))
    (make-directory (file-name-directory path) t)
    (unless (file-exists-p path)
      (with-temp-file path
        (when (string= (file-name-extension path) "org")
          (insert (format "#+title: %s\n\n" (file-name-base path))))))
    (insert (format "#+transclude: [[file:%s]]\n" path))
    (find-file-other-window path)))

(use-package org-transclusion
  :after org
  :hook (org-mode . org-transclusion-mode)
  :bind (:map org-mode-map
         ("C-c t t" . org-transclusion-mode)
         ("C-c t n" . my/org-transclusion-new)
         ("C-c t a" . org-transclusion-add)
         ("C-c t A" . org-transclusion-add-all)
         ("C-c t r" . org-transclusion-remove)
         ("C-c t g" . org-transclusion-refresh)
         ("C-c t e" . org-transclusion-live-sync-start)
         ("C-c t E" . org-transclusion-live-sync-exit)
         ("C-c t o" . my/org-transclusion-open))
  :config
  (setq org-transclusion-include-first-section t))

(use-package ob-racket
  :vc (:url "https://github.com/DEADB17/ob-racket")
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(racket . t))
  (advice-add 'org-babel-execute:racket :around
              (lambda (orig body params)
                (let ((racket-program (or (executable-find racket-program) racket-program)))
                  (funcall orig body params)))))

(use-package deft
  :commands (deft deft-new-file deft-new-file-named deft-find-file)
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

(defun org-publish-get-date-from-property (file project)
  "Get date keyword from FILE in PROJECT and parse it to internal format."
     (let ((date (org-publish-find-property file :date project)))
       (cond ((let ((ts (and (consp date) (assq 'timestamp date))))
          (and ts
         (let ((value (org-element-interpret-data ts)))
           (and (org-string-nw-p value)
          (org-time-string-to-time value))))))
       (t (error "No timestamp in file \"%s\"" file)))))

(defun sitemap-format-entry (entry style project)
  (format "
              [[file:%s][%s]]
              #+begin_article-info
              #+begin_date
              Published %s
              #+end_date
              #+end_article-info"
          entry
          (org-publish-find-title entry project)
          (format-time-string "%b %d, %Y"
                                (org-publish-get-date-from-property entry project))))

(setq org-publish-project-alist
      '(
        ("blog"
         :base-directory "~/projects/site/org/"
         :base-extension "org"
         :publishing-directory "~/projects/site/blog/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-preamble "<div class=\"links\"><a href=\"../index.html\">Home</a>\n<a href=\"\" class=\"active\">Blog</a>\n<a href=\"../about.html\">About</a></div>"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/style.css\" />"
	 :html-doctype "html5"
	 :html-link-home "../index.html"
	 :html-link-up "index.html"
         :html-head-include-scripts nil
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

(provide 'init-org)
;;; init-org.el ends here
