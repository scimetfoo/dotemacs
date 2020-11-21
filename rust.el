(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'smartparens-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package cargo
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
