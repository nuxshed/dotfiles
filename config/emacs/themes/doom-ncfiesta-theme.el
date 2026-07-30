;;; doom-ncfiesta-theme.el --- ncfiesta -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: nuxsh
;; Created: September 2024
;; Version: 0.1
;; Keywords: custom themes, faces
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;; Commentary:
;;
;; theme.
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-ncfiesta-theme nil
  "Options for the `doom-ncfiesta' theme."
  :group 'doom-themes)

(defcustom doom-ncfiesta-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ncfiesta-theme
  :type 'boolean)

(defcustom doom-ncfiesta-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ncfiesta-theme
  :type '(or integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-ncfiesta
  "Theme inspired by gko's plain dark."

  ;; name        default   256       16
  ((bg         '("#141414" nil       nil ))
   (bg-alt     (doom-lighten bg 0))
   (base0      '("#838083" nil nil ))
   (base1      '("#202020" nil nil ))
   (base2      '("#bbbbbb" nil nil ))
   (base3      '("#444444" nil nil ))
   (base4      '("#202020" nil nil ))
   (base5      '("#373737" nil nil ))
   (base6      '("#050505" nil nil ))
   (base7      '("#727272" nil nil ))
   (base8      '("#050505" nil nil ))
   (fg         '("#e1e1e1" nil nil ))
   (fg-alt     '("#e7e5e3" nil nil ))

   (grey       fg)
   (red        '("#b46958" "#b46958" "brightred"))
   (orange     '("#ffa557" "#ffa557" "brightred"))
   (green      '("#90a959" "#90a959" "green"))
   (teal       '("#88afa2" "#88afa2" "brightgreen"))
   (yellow     '("#f4bf75" "#f4bf75" "yellow"))
   (blue       '("#bad7ff" "#bad7ff" "brightblue"))
   (dark-blue  '("#7e97ab" "#7e97ab" "blue"))
   (magenta    '("#aa759f" "#aa759f" "magenta"))
   (violet     '("#aa759f" "#aa759f" "brightmagenta"))
   (cyan       '("#88afa2" "#88afa2" "brightcyan"))
   (dark-cyan  '("#88afa2" "#88afa2" "cyan"))

   ;; extra colors
   (altred        '("#984936" "#984936" "red"))
   (altblue        '("#51675b" "#51675b" "blue"))
   (altgreen      '("#586935" "#586935" "green"))

   ;; face categories -- required for all themes
   (highlight      base2)
   (vertical-bar   fg)
   (selection      base4)
   (builtin        base0)
   (comments       base5)
   (doc-comments   base5)
   (constants      fg)
   (functions      teal)
   (keywords       blue)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        base0)
   (variables      base0)
   (numbers        base0)
   (region         base1)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    (doom-darken fg 0.4))
   (vc-added       (doom-lighten fg 0.4))
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-ncfiesta-padded-modeline
      (if (integerp doom-ncfiesta-padded-modeline) doom-ncfiesta-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base5)

   (modeline-bg
    `(,(doom-lighten (car bg) 0) ,@(cdr base0)))
   (modeline-bg-l
    `(,(doom-darken (car bg) 0) ,@(cdr base0)))
   (modeline-bg-inactive   `(,(car bg) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0)))


  ;;;; Base theme face overrides
  (((font-lock-constant-face &override)      :underline 't)
   ((font-lock-comment-face &override)       :slant 'italic)
   ((font-lock-function-name-face &override) :weight 'bold)
   ((font-lock-type-face &override)          :slant 'italic)
   (hl-line :background base8)
   ((line-number &override) :foreground base3)
   ((line-number-current-line &override) :foreground base2)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))


  (mode-line-evil-normal :foreground bg :background fg)
  (mode-line-evil-insert :foreground bg :background fg)
  (mode-line-evil-visual :foreground bg :background fg)
  (mode-line-evil-motion :foreground bg :background fg)
  (mode-line-evil-replace :foreground bg :background fg)
  (mode-line-evil-operator :foreground bg :background fg)

  (header-line :background bg)

   ;;;; lsp-mode
   (lsp-headerline-breadcrumb-symbols-face :foreground keywords :weight 'bold)
   ;;;; outline <built-in>
   (outline-1 :slant 'italic :foreground fg-alt)
   (outline-2 :inherit 'outline-1 :foreground base2)
   (outline-3 :inherit 'outline-2)
   (outline-4 :inherit 'outline-3)
   (outline-5 :inherit 'outline-4)
   (outline-6 :inherit 'outline-5)
   (outline-7 :inherit 'outline-6)
   (outline-8 :inherit 'outline-7)
   ;;;; org <built-in>
   (org-block-begin-line :foreground base2 :background base3)
   (org-block-end-line :foreground base2 :background base3)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; doom-ncfiesta-theme.el ends here
