(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)
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

(require 'package)
(setq package-enable-at-startup nil)   ; To prevent initialising twice
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(use-package use-package-ensure-system-package
  :ensure t)

;; Reduce the frequency of garbage collection by making it happen on
;; ~100MB of allocated data, lower it to the default after init.

(setq gc-cons-threshold 50000000
      gc-cons-percentage 0.6)

(setq auto-window-vscroll nil)

;; Automatically reload files when they change on disk
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)
(setq auto-revert-verbose nil)

;; prevent splits from being opened
(setq pop-up-windows nil)

;; don't show the tool bar
(tool-bar-mode -1)

;; don't show the scroll bar
(scroll-bar-mode -1)
(setq scroll-conservatively 10000)
;; Make the title bar blend with the background color
;; Set the appearance to light/dark depending on the theme
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;; Hide cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil)

;; Show full file path in the title bar
(setq
 frame-title-format
 '((:eval (if (buffer-file-name)
              (abbreviate-file-name (buffer-file-name))
            "%b"))))

;; Set font and line spacing
(set-frame-font "Menlo 15" nil t)
(set-face-attribute 'default nil
                    :font  "Menlo")

;; Line numbers
;; Add padding to line numbers
(setq linum-format "%5d ")
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Use y/n instead of full yes/no for confirmation messages
(fset 'yes-or-no-p 'y-or-n-p)

;; Use spaces instead of tabs
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

;; Type over selected text
(delete-selection-mode 1)

;; Kill whole line
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)

;; Use Cmd for movement
(global-set-key (kbd "s-<right>") (kbd "C-e"))  ;; End of line
(global-set-key (kbd "s-<left>") (kbd "M-m"))   ;; Beginning of line

;; Kills the current buffer without displaying the menu.
;; A confirmation will be asked for, if the buffer has been modified
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; copy the env variables over from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ns-pop-up-frames nil)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; workaround for alt not working as meta key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'super
      mac-option-modifier 'meta
      inhibit-splash-screen t
      inhibit-startup-message t
      ring-bell-function 'ignore
      select-enable-clipboard t
      save-interprogram-paste-before-kill t
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      vc-handled-backends '(Git)
      default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8
      org-ellipsis " ▶")

(blink-cursor-mode 0)

(global-hl-line-mode nil)
(set-face-attribute hl-line-face nil :underline nil)

;; Rewrite selected text
(delete-selection-mode 1)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-gruvbox-dark-hard t))

(use-package expand-region
  :config (global-set-key (kbd "C-;") 'er/expand-region))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-use-faces t
        ido-vertical-show-count t))

(use-package ivy
  :bind ("s-b". 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-count-format "(%d/%d) ")

  ;; Use [Enter] to navigate into the directory, not dired-open it.
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

  ;; (use-package flx
  ;;   :init
  ;;   (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
  )

(use-package wgrep
  :ensure t)

(use-package esup
  :ensure t
  :config   (setq esup-depth 0))

(use-package rg
  :ensure t
  :config
  (setq rg-command-line-flags '("-w"))
  (setq rg-ignore-case 'smart))

(use-package counsel
  :ensure t
  :after rg
  :config
  ;; (global-set-key (kbd "s-g") 'counsel-rg)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (setq counsel-rg-base-command "rg -i -w --no-heading --line-number %s .")
  (setq recentf-max-saved-items 50)
  (setq recentf-auto-cleanup (* 24 60 60)))

(use-package swiper
  :config
  (global-set-key (kbd "s-f") 'swiper))

(use-package multiple-cursors
  :config
  (setq-default mc/edit-lines-empty-lines 'ignore
                mc/insert-numbers-default 1)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :init
  (set-face-attribute 'mode-line nil :height 130)
  (set-face-attribute 'mode-line-inactive nil :height 130)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-github nil
        doom-modeline-version nil
        doom-modeline-height 10
        doom-modeline-bar-width 1
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 50
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil
        doom-modeline-window-width-limit fill-column))

(use-package undo-tree
  :bind ("s-Z" . 'undo-tree-redo)
  :config
  (global-undo-tree-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (add-hook 'gfm-mode-hook 'linum-mode)
  (add-hook 'markdown-mode-hook 'linum-mode))

(use-package smartparens
  :init
  (add-hook 'markdown-mode-hook 'smartparens-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package yaml-mode)
(use-package json-mode)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package protobuf-mode
  :hook (protobuf-mode . flycheck-mode))

(use-package dockerfile-mode
  :config
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1))

(use-package  yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :demand t
  :config
  (setq yas-verbosity 1 yas-wrap-around-region t)
  (yas-reload-all)
  (yas-global-mode 1))

(use-package auto-yasnippet
  :ensure t)

(use-package speed-type
  :ensure t)

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package format-all
  :ensure t
  :bind ("C-c SPC" . format-all-buffer))

(use-package company
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (global-company-mode)
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above nil)
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 1.0)
  :diminish which-key-mode)

;; Highlight matching parentheses
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 1)
(set-face-background 'show-paren-match (face-background 'default))
(if (eq (frame-parameter nil 'background-mode) 'dark)
    (set-face-foreground 'show-paren-match "red")
  (set-face-foreground 'show-paren-match "black"))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; shift between buffers using shift+arrow keys.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; navigate/edit s-expressions as blocks.
(use-package paredit
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package projectile
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :init
  (setq-default projectile-cache-file
                (expand-file-name ".projectile-cache" user-emacs-directory))
  (add-hook 'prog-mode-hook #'projectile-mode)

  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq-default projectile-enable-caching t
                projectile-mode-line-prefix ""
                projectile-sort-order 'recentf
                ;; Show project (if any) name in modeline
                projectile-mode-line '(:eval (projectile-project-name))))

(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-set-upstream-on-push 'askifnotset))


;; Lang: Zig
(use-package zig-mode
  :ensure t)

;; Lang: Clojure
(use-package lsp-mode
  :ensure t
  :config (setenv "PATH" (concat
                          "/usr/local/bin" path-separator
                          (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . lsp-deferred)
         (clojurec-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred)))

(use-package clojure-mode-extra-font-locking)

(define-clojure-indent
  (defrecord 1))

;; (add-to-list 'load-path "/Users/murtaza/dev/cider") ;; load the local instance of cider
;; (load "cider-autoloads" t t)
(use-package cider
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . paredit-mode)
         (cider-repl-mode . company-mode)
         (cider-repl-mode . (lambda ()
                              (local-set-key (kbd "C-l") 'cider-repl-clear-buffer)))
         (cider-mode . company-mode)))

(use-package clj-refactor
  :config
  (setq cljr-warn-on-eval nil)
  :hook
  (clojure-mode . (lambda ()
                    (clj-refactor-mode 1)
                    (yas-minor-mode 1)
                    (cljr-add-keybindings-with-prefix "C-c C-m"))))

(require 'ob-clojure)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)))
(setq org-babel-clojure-backend 'cider)

;; Lang: Clojure
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))


(use-package golint
  :ensure t)

(defun custom-go-mode ()
  (display-line-numbers-mode 1))

(use-package go-mode
  :ensure t
  :ensure-system-package
  ((gopls . "go get -u golang.org/x/tools/gopls@latest"))
  :init
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  :hook ((go-mode . lsp-deferred)
         (go-mode . custom-go-mode)
         (go-mode . lsp-go-install-save-hooks)))

;; Lang: Haskell
(defun haskell-mode-setup ()
  (setq haskell-process-type 'stack-ghci))

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'"    . haskell-mode))
  :interpreter ("haskell" . haskell-mode)
  :config
  (require 'haskell)
  ;; (require 'hindent)
  (require 'haskell-mode)
  (require 'haskell-interactive-mode)
  (require 'autoinsert)
  (setq haskell-compile-cabal-build-command "stack build")
  (setq haskell-process-log t)
  ;; (define-key hindent-mode-map (kbd "C-c SPC") 'hindent-reformat-buffer)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  :hook ((haskell-mode . lsp-deferred)
         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-mode-setup)
         (haskell-mode . hindent-mode)
         (haskell-mode- . (lambda () (yas-minor-mode)))))

(use-package lsp-haskell
  :ensure t)

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (setq dante-repl-command-line '("stack" "repl" dante-target))
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'dante-mode-hook 'haskell-mode-setup)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

;; Lang: Rust
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

;; Lang: Java
(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp-deferred))

;; Lang: Kotlin
(use-package kotlin-mode
  :ensure t
  :hook ((kotlin-mode . lsp-deferred)
         (kotlin-mode . flycheck-mode)
         (kotlin-mode . company-mode)))

;; Lang: Tex
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


;; Org
(use-package org
  :ensure t
  :config (progn
            (setq org-todo-keywords (quote ((sequence "DISCUSS" "SPIKE" "ESTIMATE" "TODO" "IN-PROGRESS" "REVIEW" "DEFERED" "REDO" "|" "CANCELLED" "PUNTED" "DELEGATED" "DONE"))))
            (add-hook 'org-shiftup-final-hook 'windmove-up)
            (add-hook 'org-shiftleft-final-hook 'windmove-left)
            (add-hook 'org-shiftdown-final-hook 'windmove-down)
            (add-hook 'org-shiftright-final-hook 'windmove-right)
            (setq org-startup-with-inline-images t))
  :init (progn
          (add-hook 'org-mode-hook 'inhibit-global-linum-mode)
          (add-hook 'org-mode-hook 'flyspell-mode)
          ;;(add-hook 'org-mode-hook 'auto-org-md-mode)
          (add-hook 'org-mode-hook 'org-bullets-mode)))

(use-package org-bullets
  :ensure t
  :config (setq org-bullets-bullet-list '("◉" "⌘" "○" "⌗")))

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

(use-package ox-reveal
  :ensure t)

(require 'ob)
(require 'ob-tangle)

;; Lang: Ruby
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

;; Lang: Purescript
(use-package purescript-mode
  :ensure t)

(use-package psc-ide
  :ensure t)

(require 'psc-ide)

(add-hook 'purescript-mode-hook
          (lambda ()
            (psc-ide-mode)
            (company-mode)
            (flycheck-mode)
            (turn-on-purescript-indentation)))

;; Lang: Python
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package anaconda-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(setq python-shell-completion-native-disabled-interpreters '("python"))

(setq python-shell-interpreter "python3")

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ea5822c1b2fb8bb6194a7ee61af3fe2cc7e2c7bab272cbb498a0234984e1b2d9" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
