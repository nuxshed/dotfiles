;; -*- lexical-binding: t; -*-

(defun org/prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
	'(("#+begin_src" . ">")
	  ("#+BEGIN_SRC" . ">")
	  ("#+end_src" . "<")
	  ("#+END_SRC" . "<")
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

(provide 'prettify-symbols)
