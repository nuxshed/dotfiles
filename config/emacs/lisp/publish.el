;;; publish.el --- org-publish config -*- lexical-binding: t; -*-

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
         :html-preamble "<div class=\"links\"><a href=\"../index.html\">Home</a><a href=\"\" class=\"active\">Blog</a><a href=\"../about.html\">About</a></div>"
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

(provide 'publish)
;;; publish.el ends here
