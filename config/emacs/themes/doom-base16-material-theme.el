;;; doom-base16-material-theme.el --- A nice dark theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: nuxsh
;; Created: June 2022
;; Version: 0.1
;; Keywords: custom themes, faces
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;; Commentary:
;;
;; theme.
;;
;;; Code:
(require 'doom-themes)

(defgroup doom-base16-material-theme nil
  "Options for the `doom-base16-material' theme."
  :group 'doom-themes)

(def-doom-theme doom-base16-material
  "I made this theme."

  ;; name        default   256       16
  ((bg         '("#263238" "#263238" "white"))
   (fg         '("#EEFFFF" "#EEFFFF" "black"))

   (bg-alt     '("#2E3C43" "#2E3C43" "white"))
   (fg-alt     '("#546E7A" "#546E7A" "brightwhite"))

   (base0      '("#000000" "#000000" "black"))
   (base1      '("#1a2226" "#1a2226" "brightblack"))
   (base2      '("#263238" "#263238" "brightblack"))
   (base3      '("#2E3C43" "#2E3C43" "brightblack"))
   (base4      '("#314549" "#314549" "brightblack"))
   (base5      '("#3f595e" "#3f595e" "brightblack"))
   (base6      '("#546E7A" "#546E7A" "brightblack"))
   (base7      '("#B2CCD6" "#B2CCD6" "brightblack"))
   (base8      '("#EEFFFF" "#EEFFFF" "white"))

   (grey       base4)
   (red        '("#F07178" "#F07178" "red"))
   (orange     '("#F78C6C" "#F78C6C" "brightred"))
   (green      '("#C3E88D" "#C3E88D" "green"))
   (teal       '("#89DDFF" "#89DDFF" "brightgreen"))
   (yellow     '("#FFCB6B" "#FFCB6B" "yellow"))
   (blue       '("#82AAFF" "#82AAFF" "brightblue"))
   (dark-blue  '("#6887cc" "#6887cc" "blue"))
   (magenta    '("#C792EA" "#C792EA" "magenta"))
   (violet     '("#C792EA" "#C792EA" "brightmagenta"))
   (cyan       '("#89DDFF" "#89DDFF" "brightcyan"))
   (dark-cyan  '("#89DDFF" "#89DDFF" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base4)
   (selection      base4)
   (builtin        blue)
   (comments       fg-alt)
   (doc-comments   fg-alt)
   (constants      fg)
   (functions      orange)
   (keywords       red)
   (methods        cyan)
   (operators      fg)
   (type           fg)
   (strings        green)
   (variables      fg)
   (numbers        magenta)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))

   (region-fg base8)

   (modeline-fg     base7)
   (modeline-bg     bg)
   (modeline-bg-inactive   base2))


  ;; Base theme face overrides
  ((fringe :foreground teal)
   ((line-number &override) :foreground base6)
   ((line-number-current-line &override) :foreground fg)
   ((tab-line &override) :background modeline-bg :foreground fg-alt)
   ((tab-line-tab-inactive &override) :foreground base4)
   ((font-lock-comment-face &override)
    :inherit 'italic)
   (hl-line :background bg-alt)
   (mode-line
    :background bg :foreground modeline-fg :overline "#314549"
    :inherit 'fixed-pitch)
   (mode-line-inactive
    :background modeline-bg-inactive :foreground "#314549"
    :overline "#2E3C43"
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
   (org-block :background "#2E3C43")
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

;;; doom-base16-material-theme.el ends here
