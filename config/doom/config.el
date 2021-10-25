;;; config.el -*- lexical-binding: t; -*-

(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(setq user-full-name "nuxsh"
      user-mail-address "nuxshed@gmail.com")

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to lose work
      scroll-margin 2                             ; Having a little margin is nice
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      display-line-numbers-type t
      which-key-idle-delay 0.3                    ; Show key binding help quicker
      which-key-idle-secondary-delay 0
      shell-file-name "/bin/dash"                 ; Use dash as default shell for running term which is faster
      vterm-always-compile-module t               ; Compile the vterm-module when needed without asking
      vterm-shell "/bin/zsh")                     ; Use zsh in vterm)

(global-subword-mode 1) ; navigate through Camel Case words

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 11)
      doom-big-font (font-spec :family "FiraCode Nerd Font" :size 16)
      doom-variable-pitch-font (font-spec :family "Montserrat" :size 12))
      doom-serif-font (font-spec :family "Montserrat" :size 12)

(setq doom-theme 'doom-one)
(setq doom-one-padded-modeline t)
(setq doom-themes-treemacs-theme "doom-atom")

(after! projectile
  (setq projectile-project-root-files-bottom-up '("package.json" ".projectile" ".project" ".git")
        projectile-ignored-projects '("~/.emacs.d/")
        projectile-project-search-path '("~/projects"))
  (defun projectile-ignored-project-function (filepath)
    "Return t if FILEPATH is within any of `projectile-ignored-projects'"
    (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects))))

(after! popup
  (set-popup-rule! "^\\*Flycheck errors\\*$" :side 'bottom :size 0.2 :select t))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change)))

(after! (flycheck lsp-mode)
  (add-hook 'lsp-after-initialize-hook (lambda()
                                        (flycheck-add-next-checker 'lsp '(warning . javascript-eslint)))))

;; There's a weird bug where fringe-modes < 8 dont show the fringes
(after! git-gutter
  (fringe-mode 8)
  (after! git-gutter-fringe
    (fringe-mode 8))
  (setq +vc-gutter-diff-unsaved-buffer t))

(after! ibuffer
  (set-popup-rule! "^\\*Ibuffer\\*$" :side 'bottom :size 0.4 :select t :ignore nil))

(setq doom-themes-treemacs-theme "doom-colors")
(after! treemacs
  (setq +treemacs-git-mode 'extended)
  (treemacs-follow-mode t))
(after! rainbow-mode
  (setq rainbow-html-colors-major-mode-list '(html-mode css-mode php-mode nxml-mode xml-mode typescript-mode javascript-mode)))

(set-frame-parameter nil 'internal-border-width 15)
(setq-default left-margin-width 2)
(setq-default right-margin-width 2)

;;modeline (icons, config, battery)
(display-time-mode 1)                              ;Enable time in the mode-line
(display-battery-mode 1)                           ;display the battery
(setq doom-modeline-major-mode-icon t)             ;Show major mode name
(setq doom-modeline-enable-word-count t)           ;Show word count
(setq doom-modeline-modal-icon t)                  ;Show vim mode icon
(setq inhibit-compacting-font-caches t)            ;Don't compact font caches in gc
(setq doom-modeline-lsp t)

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding) ;;remove encoding

(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 30
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-group-by-projectile-project)

  (+popup-window-p) ; needed to prevent recursive auto-loading of popup

  ;; Automatically turn off tabs in popups
  (defun +fl/hide-tabs-in-popup ()
    (if (+popup-window-p)
        (centaur-tabs-local-mode)
      (centaur-tabs-local-mode 0)))
  (add-hook! 'buffer-list-update-hook '+fl/hide-tabs-in-popup))

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil; not anymore useful than flycheck
        lsp-ui-doc-enable nil
        lsp-enable-symbol-highlighting nil))

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("python" "ipython" "bash" "sh" "rust" "lua"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(set-docsets! 'python-mode "Python 3")
(set-docsets! 'lua-mode "Lua")
(set-docsets! 'emacs-lisp-mode "Emacs Lisp")
(setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)

(setq org-directory "~/org/"
      org-ellipsis "  "
      org-journal-file-type 'weekly
      org-use-property-inheritance t
      org-log-done 'time
      org-hide-emphasis-markers t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-log-into-drawer t
      org-log-state-notes-into-drawer t
      org-log-repeat 'time
      org-todo-repeat-to-state "TODO")

(after! org
  (setq org-tags-column -80
        org-agenda-sticky nil))

(advice-add 'org-refile :after 'org-save-all-org-buffers)
(advice-add 'org-gcal-fetch :after 'org-save-all-org-buffers)

(after! org
  (setq org-tags-column -80)
  (appendq! +ligatures-extra-symbols
            `(:checkbox      ""
              :doing         ""
              :checkedbox    ""
              :list_property "∷"))
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :doing         "[-]"
    :checkedbox    "[X]"
    :list_property "::"))

(setq deft-directory "~/notes")
(setq deft-extensions '("txt" "tex" "org" "md"))
(setq deft-recursive t)

(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

(after! web-mode
  (add-hook 'web-mode-hook #'flycheck-mode)

  (setq web-mode-markup-indent-offset 2 ;; Indentation
        web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil ;; disbale adding "" after an =
        web-mode-auto-close-style 2))

(set-email-account! "gmail"
  '((mu4e-sent-folder       . "/gmail/\[Gmail\]/Sent Mail")
    (mu4e-drafts-folder     . "/gmail/\[Gmail\]/Drafts")
    (mu4e-trash-folder      . "/gmail/\[Gmail\]/Trash")
    (mu4e-refile-folder     . "/gmail/\[Gmail\]/All Mail")
    (smtpmail-smtp-user     . "nuxshed@gmail.com")
    (user-mail-address      . "nuxshed@gmail.com")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "---\nnuxsh"))
  t)

(setq +mu4e-gmail-accounts '(("nuxshed@gmail.com" . "/gmail")))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "nuxshed@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)
