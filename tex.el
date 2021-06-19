(use-package tex-mode
  :ensure auctex
  :config
  (setq TeX-auto-save t))

(use-package pdf-tools
  :ensure t
  :hook (pdf-view-mode . pdf-continuous-scroll-mode))

(use-package company-auctex
  :ensure t)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
