(use-package rust-mode
  :ensure t
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . lsp-deferred)
         (rust-mode . smartparens-mode)
         (rust-mode .
                    (lambda ()
                      (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode
  :ensure t)
