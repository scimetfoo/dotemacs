(setq gc-cons-threshold (* 50 1000 1000)  ;; 50MB during init
      gc-cons-percentage 0.6
      package-enable-at-startup nil
      auto-window-vscroll nil
      inhibit-compacting-font-caches t
      read-process-output-max (* 1024 1024)) ;; 1mb

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000)))) ;; 2MB post init

(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)

(use-package use-package-ensure-system-package
  :ensure t)

(setq-default
 inhibit-startup-screen t
 initial-scratch-message nil
 sentence-end-double-space nil
 frame-title-format '((:eval (if (buffer-file-name)
                                (abbreviate-file-name (buffer-file-name))
                              "%b")))
 indent-tabs-mode nil
 tab-width 2
 js-indent-level 2
 css-indent-offset 2
 c-basic-offset 2
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 ring-bell-function 'ignore
 scroll-conservatively 10000
 pop-up-windows nil
 cursor-in-non-selected-windows nil
 vc-handled-backends '(Git)
 select-enable-clipboard t
 save-interprogram-paste-before-kill t)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq auto-revert-check-vc-info t
      auto-revert-verbose nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'super
        mac-option-modifier 'meta
        ns-use-native-fullscreen t)

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  (global-set-key (kbd "s-<right>") (kbd "C-e"))
  (global-set-key (kbd "s-<left>") (kbd "M-m"))
  (global-set-key (kbd "s-<backspace>") 'kill-whole-line))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package base16-theme
  :config
  (load-theme 'base16-gruvbox-dark-hard t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 10)
  (doom-modeline-bar-width 1)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-github nil)
  (doom-modeline-version nil))

(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-transformers '(company-sort-by-occurrence))
  :config
  (global-company-mode))

(use-package company-lsp
  :after company)

(use-package lsp-mode
  :commands lsp
  :hook ((clojure-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (java-mode . lsp-deferred))
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-completion-provider :capf)
  :config
  (setq lsp-idle-delay 0.500))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil))

(use-package projectile
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-sort-order 'recentf)
  :config
  (projectile-mode))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-traditional)
  (magit-set-upstream-on-push 'askifnotset))

;; Enhanced navigation and completion
(use-package ivy
  :bind (("s-b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :bind (("C-x C-r" . counsel-recentf)
         ("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x))
  :custom
  (counsel-rg-base-command "rg -i -w --no-heading --line-number %s .")
  (recentf-max-saved-items 50)
  (recentf-auto-cleanup (* 24 60 60)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("s-f" . swiper)))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;; Text editing enhancements
(use-package expand-region
  :bind ("C-;" . er/expand-region))

(use-package smartparens
  :hook ((prog-mode markdown-mode) . smartparens-mode))

(use-package undo-tree
  :bind ("s-Z" . undo-tree-redo)
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; File type support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2))

;; Additional utility packages
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 1.0))

(use-package yasnippet
  :demand t
  :config
  (setq yas-verbosity 1)
  (yas-global-mode 1))

(use-package yasnippet-snippets)
(use-package auto-yasnippet)

(use-package format-all
  :bind ("C-c SPC" . format-all-buffer))

;; Programming language support
(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package clojure-mode
  :ensure t
  :after clojure-mode-extra-font-locking
  :hook ((clojure-mode . lsp-deferred)
         (clojure-mode . paredit-mode)))

(use-package cider
  :ensure t
  :after (clojure-mode)
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . paredit-mode)
         (cider-repl-mode . company-mode)
         (cider-repl-mode . (lambda ()
                             (local-set-key (kbd "C-l") 'cider-repl-clear-buffer)))))

(use-package clj-refactor
  :ensure t
  :after (clojure-mode)
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (yas-minor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package paredit
  :ensure t
  :hook ((clojure-mode . enable-paredit-mode)
         (cider-repl-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)))

(use-package go-mode
  :ensure-system-package
  ((gopls . "go get -u golang.org/x/tools/gopls@latest"))
  :hook (go-mode . (lambda ()
                     (lsp-deferred)
                     (add-hook 'before-save-hook #'lsp-format-buffer t t)
                     (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(use-package rust-mode
  :hook ((rust-mode . lsp-deferred)
         (rust-mode . cargo-minor-mode)
         (rust-mode . smartparens-mode))
  :bind (:map rust-mode-map
              ("C-c <tab>" . rust-format-buffer)))

(use-package cargo)
(use-package toml-mode)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package elpy
  :init (elpy-enable)
  :config
  (define-key elpy-mode-map (kbd "M-<right>") nil)
  (define-key elpy-mode-map (kbd "M-<left>") nil))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :custom (pipenv-projectile-after-switch-function
           #'pipenv-projectile-after-switch-extended))

;; language support
(use-package haskell-mode
  :hook ((haskell-mode . lsp-deferred)
         (haskell-mode . interactive-haskell-mode))
  :custom (haskell-process-type 'stack-ghci))

(use-package lsp-haskell)

(use-package kotlin-mode
  :hook ((kotlin-mode . lsp-deferred)
         (kotlin-mode . company-mode)))

(use-package ruby-mode
  :hook ((ruby-mode . lsp-deferred)
         (ruby-mode . company-mode)))

(use-package rspec-mode)
(use-package rubocop)

(use-package tuareg
  :mode ("\\.ml[iyl]?\\'" . tuareg-mode))

(use-package merlin
  :hook ((tuareg-mode . merlin-mode)
         (merlin-mode . company-mode)))

(use-package merlin-eldoc
  :hook (tuareg-mode . merlin-eldoc-setup))

(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package protobuf-mode)
(use-package nix-mode)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init)
