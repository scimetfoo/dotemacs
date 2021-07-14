(use-package kotlin-mode
  :ensure t
  :hook ((kotlin-mode . lsp-deferred)
         (kotlin-mode . flycheck-mode)
         (kotlin-mode . company-mode)))
