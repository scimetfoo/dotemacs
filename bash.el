(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (sh-mode . lsp))
