;; -*- lexical-binding: t; -*-

(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-select-next)))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-scrollbar nil))

(add-hook 'after-init-hook 'global-company-mode)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package hotfuzz
  :config
  (setq completion-styles '(hotfuzz)))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(provide 'completion)
