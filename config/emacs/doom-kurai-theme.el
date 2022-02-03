;;; doom-kurai-theme.el --- A nice light theme -*- lexical-binding: t; no-byte-compile: t; -*-
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

(defgroup doom-kurai-theme nil
  "Options for the `doom-kurai' theme."
  :group 'doom-themes)

(def-doom-theme doom-kurai
  "I made this theme."

  ;; name        default   256       16
  ((bg         '("#2a2320" "#2a2320" "white"))
   (fg         '("#b4a99b" "#b4a99b" "black"))

   (bg-alt     '("#e9e4e2" "#e9e4e2" "white"))
   (fg-alt     '("#6b655c" "#6b655c" "brightwhite"))

   (base0      '("#000000" "#000000" "black"))
   (base1      '("#1e1917" "#1e1917" "brightblack"))
   (base2      '("#2a2320" "#2a2320" "brightblack"))
   (base3      '("#382e2a" "#382e2a" "brightblack"))
   (base4      '("#51433e" "#51433e" "brightblack"))
   (base5      '("#6b655c" "#6b655c" "brightblack"))
   (base6      '("#7a655d" "#7a655d" "brightblack"))
   (base7      '("#bcafa9" "#bcafa9" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "white"))

   (grey       base4)
   (red        '("#a37a77" "#a37a77" "red"))
   (orange     '("#a37a77" "#a37a77" "brightred"))
   (green      '("#84a377" "#84a377" "green"))
   (teal       '("#7798a3" "#7798a3" "brightgreen"))
   (yellow     '("#9fa377" "#9fa377" "yellow"))
   (blue       '("#7782a3" "#7782a3" "brightblue"))
   (dark-blue  '("#7782a3" "#7782a3" "blue"))
   (magenta    '("#a3728f" "#a3728f" "magenta"))
   (violet     '("#a3728f" "#a3728f" "brightmagenta"))
   (cyan       '("#7798a3" "#7798a3" "brightcyan"))
   (dark-cyan  '("#7798a3" "#7798a3" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base4)
   (selection      base3)
   (builtin        yellow)
   (comments       base5)
   (doc-comments   base5)
   (constants      fg)
   (functions      orange)
   (keywords       red)
   (methods        cyan)
   (operators      fg)
   (type           fg)
   (strings        green)
   (variables      fg)
   (numbers        magenta)
   (region         base3)
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
    :background bg :foreground modeline-fg :overline base5
    :inherit 'fixed-pitch)
   (mode-line-inactive
    :background bg :foreground base5 :overline base5
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

   ;; company
   (company-tooltip :foreground fg :background bg)

   ;;;; org <built-in>
   (org-hide :foreground hidden)
   (org-block :background "#2d2622")
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

;;; doom-kurai-theme.el ends here
