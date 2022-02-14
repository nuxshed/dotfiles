;; -*- lexical-binding: t; -*-

(setq inhibit-startup-echo-area-message t
      inhibit-startup-message t)
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(menu-bar-mode -1)     ; Disable the menu bar
(scroll-bar-mode -1)   ; Disable the scrollbar

(add-to-list 'default-frame-alist '(font . "Cartograph CF 10"))
(add-to-list 'default-frame-alist '(internal-border-width . 24))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t
      file-name-handler-alist nil
 
      ;; Package Starting Unwanted
      package-enable-at-startup nil
      package--init-file-ensured t

      ;; Set Initial Major Mode To Fundamental
      initial-major-mode 'fundamental-mode
      frame-inhibit-implied-resize t)
