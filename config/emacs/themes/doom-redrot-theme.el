;;; doom-redrot-theme.el --- A nice light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: nuxsh
;; Created: June 2024
;; Version: 1.0
;; Keywords: custom themes, faces
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;; Commentary:
;;
;; theme.
;;
;;; Code:
(require 'doom-themes)

(defgroup doom-redrot-theme nil
  "Options for the `doom-redrot' theme."
  :group 'doom-themes)

(def-doom-theme doom-redrot
  "I made this theme."

  ;; name        default   256       16
  ((bg         '("#1d2021" "#1d2021" "white"))
   (fg         '("#ddc7a1" "#ddc7a1" "black"))

   (bg-alt     '("#282828" "#282828" "white"))
   (fg-alt     '("#edd5ad" "#edd5ad" "brightwhite"))

   (base0      '("#141617" "#141617" "white"))
   (base1      '("#2d2d2d" "#2d2d2d" "brightblack"))
   (base2      '("#423f3e" "#423f3e" "brightblack"))
   (base3      '("#504945" "#504945" "brightblack"))
   (base4      '("#504945" "#504945" "brightblack"))
   (base5      '("#9b8d7a" "#9b8d7a" "brightblack"))
   (base6      '("#bcac95" "#bcac95" "brightblack"))
   (base7      '("#ddc7a1" "#ddc7a1" "brightblack"))
   (base8      '("#edd5ad" "#edd5ad" "black"))

   (grey       base4)
   (red        '("#ea6962" "#ea6962" "red"))
   (orange     '("#e78a4e" "#e78a4e" "brightred"))
   (green      '("#a9b665" "#a9b665" "green"))
   (teal       '("#89b482" "#89b482" "brightgreen"))
   (yellow     '("#d8a657" "#d8a657" "yellow"))
   (blue       '("#7daea3" "#7daea3" "brightblue"))
   (dark-blue  '("#445f71" "#445f71" "blue"))
   (magenta    '("#d3869b" "#d3869b" "magenta"))
   (violet     '("#d3869b" "#d3869b" "brightmagenta"))
   (cyan       '("#89b482" "#89b482" "brightcyan"))
   (dark-cyan  '("#89b482" "#89b482" "cyan"))

   ;; face categories -- required for all themes
   (highlight      base5)
   (vertical-bar   base4)
   (selection      base1)
   (builtin        yellow)
   (comments       base8)
   (doc-comments   base8)
   (constants      fg)
   (functions      orange)
   (keywords       red)
   (methods        yellow)
   (operators      fg)
   (type           fg)
   (strings        yellow)
   (variables      fg)
   (numbers        orange)
   (region         base1)
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
   ;; ((font-lock-comment-face &override)
   ;;  :inherit 'italic)
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
   ;; (font-lock-keyword-face :inherit 'italic :foreground red)
   ;; (font-lock-string-face :inherit 'italic :foreground green)

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

;;; doom-redrot-theme.el ends here
