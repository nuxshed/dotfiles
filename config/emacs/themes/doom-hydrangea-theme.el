;;; doom-hydrangea-theme.el --- A nice light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: nuxsh
;; Created: June 2023
;; Version: 0.1
;; Keywords: custom themes, faces
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;; Commentary:
;;
;; theme.
;;
;;; Code:
(require 'doom-themes)

(defgroup doom-hydrangea-theme nil
  "Options for the `doom-hydrangea' theme."
  :group 'doom-themes)

(def-doom-theme doom-hydrangea
  "I made this theme."

  ;; name        default   256       16
  ((bg         '("#eaecf2" "#eaecf2" "white"))
   (fg         '("#343b58" "#343b58" "black"))

   (bg-alt     '("#bbc0d8" "#bbc0d8" "white"))
   (fg-alt     '("#343b58" "#343b58" "brightwhite"))

   (base0      '("#ffffff" "#ffffff" "white"))
   (base1      '("#f2f4f9" "#f2f4f9" "white"))
   (base2      '("#edeff4" "#edeff4" "brightblack"))
   (base3      '("#e3e5ea" "#e3e5ea" "brightblack"))
   (base4      '("#d4d6db" "#d4d6db" "brightblack"))
   (base5      '("#c5c7d2" "#c5c7d2" "brightblack"))
   (base6      '("#aaabb5" "#aaabb5" "brightblack"))
   (base7      '("#4b557f" "#4b557f" "black"))
   (base8      '("#2d334c" "#2d334c" "black"))

   (grey       base4)
   (red        '("#8c4351" "#8c4351" "red"))
   (orange     '("#965027" "#965027" "brightred"))
   (green      '("#33635c" "#33635c" "green"))
   (teal       '("#35a69c" "#35a69c" "brightgreen"))
   (yellow     '("#a8623e" "#a8623e" "yellow"))
   (blue       '("#34548a" "#34548a" "brightblue"))
   (dark-blue  '("#cbd9e3" "#cbd9e3" "blue"))
   (magenta    '("#935787" "#88507D" "magenta"))
   (violet     '("#8850b4" "#8850b4" "brightmagenta"))
   (cyan       '("#3B8992" "#3B8992" "brightcyan"))
   (dark-cyan  '("#2a646b" "#2a646b" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base4)
   (selection      dark-blue)
   (builtin        blue)
   (comments       base6)
   (doc-comments   base6)
   (constants      fg)
   (functions      blue)
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
   (font-lock-keyword-face :inherit 'italic :foreground magenta)
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

;;; doom-hydrangea-theme.el ends here
