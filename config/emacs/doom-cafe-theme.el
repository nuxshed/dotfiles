;;; doom-cafe-theme.el --- A nice light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: nuxsh
;; Created: January 2022
;; Version: 0.1
;; Keywords: custom themes, faces
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;; Commentary:
;;
;; theme.
;;
;;; Code:
(require 'doom-themes)

(defgroup doom-cafe-theme nil
  "Options for the `doom-cafe' theme."
  :group 'doom-themes)

(def-doom-theme doom-cafe
  "I made this theme."

  ;; name        default   256       16
  ((bg         '("#F0EDEC" "#F0EDEC" "white"))
   (fg         '("#685c56" "#685c56" "black"))

   (bg-alt     '("#e9e4e2" "#e9e4e2" "white"))
   (fg-alt     '("#948985" "#948985" "brightwhite"))

   (base0      '("#ffffff" "#ffffff" "white"))
   (base1      '("#f9f6f4" "#f9f6f4" "brightblack"))
   (base2      '("#f0edec" "#f0edec" "brightblack"))
   (base3      '("#e5dad9" "#e5dad9" "brightblack"))
   (base4      '("#ddd3d2" "#ddd3d2" "brightblack"))
   (base5      '("#d2c6c5" "#d2c6c5" "brightblack"))
   (base6      '("#bcafa9" "#bcafa9" "brightblack"))
   (base7      '("#a59995" "#a59995" "brightblack"))
   (base8      '("#948985" "#948985" "black"))

   (grey       base4)
   (red        '("#a8334c" "#a8334c" "red"))
   (orange     '("#944927" "#944927" "brightred"))
   (green      '("#597a37" "#597a37" "green"))
   (teal       '("#35a69c" "#35a69c" "brightgreen"))
   (yellow     '("#a8623e" "#a8623e" "yellow"))
   (blue       '("#286486" "#286486" "brightblue"))
   (dark-blue  '("#cbd9e3" "#cbd9e3" "blue"))
   (magenta    '("#88507D" "#88507D" "magenta"))
   (violet     '("#8850b4" "#8850b4" "brightmagenta"))
   (cyan       '("#3B8992" "#3B8992" "brightcyan"))
   (dark-cyan  '("#2a646b" "#2a646b" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base4)
   (selection      dark-blue)
   (builtin        blue)
   (comments       base8)
   (doc-comments   base8)
   (constants      fg)
   (functions      orange)
   (keywords       red)
   (methods        cyan)
   (operators      fg)
   (type           fg)
   (strings        green)
   (variables      fg)
   (numbers        magenta)
   (region         dark-blue)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))

   (region-fg base0)

   (modeline-fg     nil)
   (modeline-bg     bg)
   (modeline-fg-alt base6)
   (modeline-bg-l   base1)
   (modeline-bg-inactive   base2)
   (modeline-bg-inactive-l `(,(doom-darken (car bg) 0.025) ,@(cdr base2))))


  ;; Base theme face overrides
  ((fringe :foreground teal)
   ((line-number &override) :foreground base6)
   ((line-number-current-line &override) :foreground fg)
   ((tab-line &override) :background modeline-bg :foreground fg-alt)
   ((tab-line-tab-inactive &override) :foreground dark-blue)
   ((font-lock-comment-face &override)
    :inherit 'italic)
   (hl-line :background bg-alt)
   (mode-line
    :background bg :foreground modeline-fg :overline "#a59995"
    :inherit 'fixed-pitch)
   (mode-line-inactive
    :background modeline-bg-inactive :foreground "#a59995"
    :overline "#dbd6d4"
    :box `(:line-width 5 :color ,modeline-bg))
   ((region &override) :foreground region-fg)
   (link :foreground fg :underline t)

   ;; syntax
   (font-lock-keyword-face :inherit 'italic :foreground red)
   (font-lock-string-face :inherit 'italic :foreground green)

   ;; ibuffer
   (all-the-icons-ibuffer-mode-face :inherit 'italic)

   ;; completion
   (completions-common-part :inherit 'bold :foreground blue)

   ;;;; org <built-in>
   (org-hide :foreground hidden)
   (org-block :background "#ede8e6")
   (org-link :foreground blue :underline t)

   ;; org-mode headings
   (org-level-1 :height 120 :weight 'bold :inherit 'variable-pitch)
   (org-level-2 :height 120 :weight 'bold :inherit 'variable-pitch)
   (org-level-3 :height 120 :weight 'bold :inherit 'variable-pitch)
   (org-level-4 :height 110 :weight 'bold :inherit 'variable-pitch)
   (org-level-5 :height 110 :weight 'bold :inherit 'variable-pitch)
   (org-level-6 :inherit 'bold)
   (org-level-7 :inherit 'bold)
   (org-level-8 :inherit 'bold)

   ;; all-the-icons
   (all-the-icons-dblue :foreground blue)

   ;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)


   ;; ace-window
   (aw-leading-char-face :foreground red)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; mic-paren
   ((paren-face-match &override) :foreground blue :background bg-alt :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground red :background bg :weight 'ultra-bold))

  ;;;; Base theme variable overrides-
  ())

;;; doom-cafe-theme.el ends here
