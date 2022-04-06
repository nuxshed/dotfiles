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

(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ("blog"
         :base-directory "~/projects/site/org/"
         :base-extension "org"
         :publishing-directory "~/projects/site/blog/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-preamble "<div class=\"links\"><a href=\"../index.html\">Home</a>\n<a href=\"\" class=\"active\">Blog</a>\n<a href=\"../about.html\">About</a></div>"
	 :html-postamble "<div class=\"footer\"><p><a href=\"../index.html\">home</a> - <a href=\"index.html\">index</a> - <a href=\"https://github.com/nuxshed/nuxshed.github.io\">source</a></p></div>"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/style.css\" />"
	 :html-doctype "html5"
	 :html-link-home "../index.html"
	 :html-link-up "index.html"
         :html-head-include-scripts nil
	 :with-tags t
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Recent"
         :sitemap-format-entry sitemap-format-entry
         :sitemap-sort-files anti-chronologically)))

(setq org-export-preserve-breaks t)

(provide 'publish)
;;; publish.el ends here
