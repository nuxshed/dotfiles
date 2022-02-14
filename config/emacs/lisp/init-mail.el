;; -*- lexical-binding: t; -*-

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

(provide 'init-mail)
;;; init-mail.el ends here
