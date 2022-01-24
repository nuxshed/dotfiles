;;; doom-cafe-theme.el --- inspired by cafe -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: nuxsh
;; Created: January 2022
;; Version: 0.1
;; Keywords: custom themes, faces
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))

(require 'doom-themes)

;;
(defgroup doom-cafe-theme nil
  "Options for the `doom-cafe' theme."
  :group 'doom-themes)

(defcustom doom-cafe-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-cafe-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-cafe
  "A dark theme inspired by cafe."

  ;; name        default   256       16
  ((bg         '("#F0EDEC" "#F0EDEC" "white"        ))
   (fg         '("#685c56" "#685c56" "black"        ))

   (bg-alt     '("#e9e4e2" "#e9e4e2" "white"        ))
   (fg-alt     '("#948985" "#948985" "brightwhite"  ))

   (base0      '("#ffffff" "#ffffff" "white"        ))
   (base1      '("#f9f6f4" "#f9f6f4" "brightblack"  ))
   (base2      '("#f0edec" "#f0edec" "brightblack"  ))
   (base3      '("#e5dad9" "#e5dad9" "brightblack"  ))
   (base4      '("#ddd3d2" "#ddd3d2" "brightblack"  ))
   (base5      '("#d2c6c5" "#d2c6c5" "brightblack"  ))
   (base6      '("#bcafa9" "#bcafa9" "brightblack"  ))
   (base7      '("#a59995" "#a59995" "brightblack"  ))
   (base8      '("#948985" "#948985" "black"        ))

   (grey       base4)
   (red        '("#a8334c" "#a8334c" "red"          ))
   (orange     '("#944927" "#944927" "brightred"    ))
   (green      '("#597a37" "#597a37" "green"        ))
   (teal       '("#35a69c" "#35a69c" "brightgreen"  ))
   (yellow     '("#a8623e" "#a8623e" "yellow"       ))
   (blue       '("#286486" "#286486" "brightblue"   ))
   (dark-blue  '("#cbd9e3" "#cbd9e3" "blue"         ))
   (magenta    '("#88507D" "#88507D" "magenta"      ))
   (violet     '("#8850b4" "#8850b4" "brightmagenta"))
   (cyan       '("#3B8992" "#3B8992" "brightcyan"   ))
   (dark-cyan  '("#2a646b" "#2a646b" "cyan"         ))

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
   (-modeline-pad
    (when doom-cafe-padded-modeline
      (if (integerp doom-cafe-padded-modeline) doom-cafe-padded-modeline 4)))

   (region-fg base0)

   (modeline-fg     nil)
   (modeline-bg     bg)
   (modeline-fg-alt base6)
   (modeline-bg-l   base1)
   (modeline-bg-inactive   base2)
   (modeline-bg-inactive-l `(,(doom-darken (car bg) 0.025) ,@(cdr base2))))


  ;;;; Base theme face overrides
  ((fringe :foreground teal)
   ((line-number &override) :foreground (doom-lighten 'base5 0.2))
   ((line-number-current-line &override) :foreground base7)
   ((tab-line &override) :background modeline-bg :foreground blue)
   ((tab-line-tab-inactive &override) :foreground dark-blue)
   ((font-lock-comment-face &override)
    :inherit 'italic)
   (hl-line :background bg-alt)
   (mode-line
    :background bg :foreground modeline-fg :overline "#a59995"
    :box `(:line-width 5 :color ,modeline-bg)
    :inherit 'fixed-pitch)
   (mode-line-inactive
    :background modeline-bg-inactive :foreground "#a59995"
    :overline "#dbd6d4"
    :box `(:line-width 5 :color ,modeline-bg))
   ((region &override) :foreground region-fg)

   ;; ibuffer
   (all-the-icons-ibuffer-mode-face :inherit 'italic)

   ;; org-mode headings
   (org-level-1 :height 150)
   (org-level-2 :height 130)
   (org-level-3 :height 120)
   (org-level-4 :height 100)

   ;; all-the-icons
   (all-the-icons-dblue :foreground blue)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; highlight-symbol
   (highlight-symbol-face :background (doom-lighten base4 0.1) :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background (doom-lighten base4 0.1) :distant-foreground fg-alt)
   ;;;; ivy
   ((ivy-current-match &override) :foreground region-fg :weight 'semi-bold)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; mic-paren
   ((paren-face-match &override) :foreground bg :background teal :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base7 :background red :weight 'ultra-bold)
   ;;;; org <built-in>
   (org-hide :foreground hidden)
   (org-block :background "#ede8e6")
   ;;;; vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ((vimish-fold-fringe &override)  :foreground teal))

  ;;;; Base theme variable overrides-
  ())

;;; doom-cafe-theme.el ends here
