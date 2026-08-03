;;; doom-nano-light-theme.el --- nano-emacs light palette as a doom theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: nuxsh
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;; Code:

(require 'doom-themes)

(defgroup doom-nano-light-theme nil
  "Options for the `doom-nano-light' theme."
  :group 'doom-themes)

(def-doom-theme doom-nano-light
  "Doom theme based on nano-emacs light."

  ;; name        default     256         16
  ((bg         '("#FAFAFA"  "#FAFAFA"   "white"      ))
   (bg-alt     '("#ECEFF1"  "#ECEFF1"   "brightwhite"))
   (base0      '("#FFFFFF"  "#FFFFFF"   "white"      ))
   (base1      '("#F5F5F5"  "#F5F5F5"   "brightwhite"))
   (base2      '("#ECEFF1"  "#ECEFF1"   "brightwhite"))
   (base3      '("#E0E0E0"  "#E0E0E0"   "brightblack"))
   (base4      '("#CFD8DC"  "#CFD8DC"   "brightblack"))
   (base5      '("#B0BEC5"  "#B0BEC5"   "brightblack"))
   (base6      '("#90A4AE"  "#90A4AE"   "brightblack"))
   (base7      '("#607D8B"  "#607D8B"   "black"      ))
   (base8      '("#455A64"  "#455A64"   "black"      ))
   (fg         '("#37474F"  "#37474F"   "black"      ))
   (fg-alt     '("#263238"  "#263238"   "black"      ))

   (grey       base6)
   (red        '("#B71C1C"  "#B71C1C"   "red"        ))
   (orange     '("#E65100"  "#E65100"   "brightred"  ))
   (green      '("#2E7D32"  "#2E7D32"   "green"      ))
   (teal       '("#00695C"  "#00695C"   "brightgreen"))
   (yellow     '("#F57F17"  "#F57F17"   "yellow"     ))
   (blue       '("#1565C0"  "#1565C0"   "brightblue" ))
   (dark-blue  '("#0D47A1"  "#0D47A1"   "blue"       ))
   (magenta    '("#673AB7"  "#673AB7"   "magenta"    ))
   (violet     '("#512DA8"  "#512DA8"   "brightmagenta"))
   (cyan       '("#00838F"  "#00838F"   "brightcyan" ))
   (dark-cyan  '("#006064"  "#006064"   "cyan"       ))

   ;; face categories
   (highlight      magenta)
   (vertical-bar   base3)
   (selection      base2)
   (builtin        fg)
   (comments       base6)
   (doc-comments   base6)
   (constants      fg)
   (functions      fg)
   (keywords       magenta)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        fg)
   (variables      fg)
   (numbers        fg)
   (region         base2)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   (hidden `(,(car bg) "black" "black"))

   (modeline-fg      fg)
   (modeline-fg-alt  base6)
   (modeline-bg      bg-alt)
   (modeline-bg-l    base2)
   (modeline-bg-inactive   base1)
   (modeline-bg-inactive-l base1))


  ;; face overrides
  (((font-lock-comment-face &override)       :slant 'italic :foreground base6)
   ((font-lock-type-face &override)          :slant 'italic :foreground base7)
   ((font-lock-keyword-face &override)       :foreground magenta)
   ((font-lock-function-name-face &override) :foreground fg :weight 'bold)
   ((font-lock-string-face &override)        :foreground base7)
   ((font-lock-constant-face &override)      :foreground fg)
   ((font-lock-variable-name-face &override) :foreground fg)

   (fringe               :background bg :foreground base5)
   (hl-line              :background bg-alt)
   ((line-number &override)              :foreground base5)
   ((line-number-current-line &override) :foreground fg :weight 'bold)

   (mode-line          :background modeline-bg :foreground modeline-fg :overline base4)
   (mode-line-inactive :background modeline-bg-inactive :foreground modeline-fg-alt :overline base3)

   (mode-line-evil-normal   :foreground bg :background fg)
   (mode-line-evil-insert   :foreground bg :background magenta)
   (mode-line-evil-visual   :foreground bg :background base7)
   (mode-line-evil-motion   :foreground bg :background blue)
   (mode-line-evil-replace  :foreground bg :background red)
   (mode-line-evil-operator :foreground bg :background teal)

   (header-line :background bg)

   (link :foreground magenta :underline t)

   ;;;; org
   (org-block            :background base1)
   (org-block-begin-line :foreground base5 :background base2)
   (org-block-end-line   :foreground base5 :background base2)
   (org-link             :foreground magenta :underline t)
   (org-level-1 :foreground fg-alt :weight 'bold)
   (org-level-2 :foreground fg     :weight 'bold)
   (org-level-3 :foreground fg     :slant 'italic)
   (org-level-4 :foreground base7  :slant 'italic)
   (org-level-5 :foreground base7)
   (org-level-6 :foreground base6)
   (org-level-7 :foreground base6)
   (org-level-8 :foreground base6)

   ;;;; outline
   (outline-1 :foreground fg-alt :weight 'bold)
   (outline-2 :foreground fg     :weight 'bold)
   (outline-3 :foreground fg     :slant 'italic)
   (outline-4 :foreground base7  :slant 'italic)
   (outline-5 :foreground base7)
   (outline-6 :foreground base6)
   (outline-7 :foreground base6)
   (outline-8 :foreground base6)

   ;;;; completions
   (completions-common-part :foreground magenta :weight 'bold)

   ;;;; markdown
   (markdown-markup-face    :foreground base5)
   (markdown-header-face    :foreground fg-alt :weight 'bold)
   ((markdown-code-face &override) :background base1)))

;;; doom-nano-light-theme.el ends here
