(use-package rbenv
  :ensure t)

(use-package enh-ruby-mode
  :ensure t
  :init (progn
  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))))

(use-package rubocop
  :ensure t)

(use-package flymake-ruby
  :ensure t
  :init (add-hook 'enh-ruby-mode-hook 'flymake-ruby-load))

(use-package ruby-hash-syntax
  :ensure t)

(use-package rubocopfmt
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package rspec-mode
  :ensure t)

(use-package robe
  :ensure t
  :init (add-hook 'ruby-mode-hook 'robe-mode))

(provide 'ruby)
