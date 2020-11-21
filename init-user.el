(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

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

;; (set-default 'cursor-type '(bar . 2))
;;(set-default 'cursor-type 'box)
(blink-cursor-mode 0)

(global-hl-line-mode nil)
(set-face-attribute hl-line-face nil :underline nil)

;; Rewrite selected text
(delete-selection-mode 1)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))

;; Replace scratch buffer with contents of file if it exists
(let ((filename "~/.emacs.d/plan-the-day.org"))
  (when (file-exists-p filename)
    (setq initial-buffer-choice filename)))

;; ======
;; THEMES
;; ======

(use-package zenburn-theme
  :ensure t)

(add-hook 'after-init-hook (lambda () (load-theme 'zenburn)))

(use-package monokai-pro-theme :disabled t)
(use-package moe-theme
  :disabled t
  :config
  (setq moe-theme-highlight-buffer-id t
        show-paren-style 'parenthesis)
  (moe-theme-set-color 'blue)
  (moe-dark))
(use-package srcery-theme :disabled t)
(use-package nord-theme :disabled t)
(use-package darkokai-theme
  :disabled t
  :config
  (setq darkokai-mode-line-padding 1))
(use-package doom-themes
  :disabled t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(use-package abyss-theme :disabled t)
(use-package gruvbox-theme :disabled t)
(use-package github-theme :disabled t)
(use-package dracula-theme
  :disabled t
  :config)
(use-package bubbleberry-theme
  :disabled t
  :config)

;; ==========================
;; OTHER THIRD PARTY PACKAGES
;; ==========================

(use-package expand-region
  :config (global-set-key (kbd "C-;") 'er/expand-region))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-use-faces t
        ido-vertical-show-count t
        ido-vertical-define-keys 'C-n-and-C-p-only))

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

(use-package wgrep)

(use-package rg
  :config
  (setq rg-command-line-flags '("-w"))
  (setq rg-ignore-case 'smart)

  (rg-define-search kg/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git")))

(use-package counsel
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

(use-package fast-scroll
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))

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

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-face '(:family "Helvetica" :height 110)))

(use-package auto-yasnippet
  :ensure t)

(use-package speed-type
  :ensure t)

(require 'speed-type)

;; M-x with recently/frequently used commands
(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package helm-projectile
  :ensure t)

(use-package format-all
  :ensure t
  :bind ("C-c SPC" . format-all-buffer))

;; autocomplete
(use-package company
  :bind (("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (global-company-mode)
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above nil)
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  ;; weight by frequency
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package use-package-ensure-system-package
  :ensure t)

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

;; add colors to the parens
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

;; ==============
;;    EFUNS
;; ==============
(when (file-exists-p "~/.emacs.d/init-efuns.el")
  (setq user-custom-file "~/.emacs.d/init-efuns.el")
  (load user-custom-file))

;; ==================
;; GLOBAL KEYBINDINGS
;; ==================

(global-set-key (kbd "s-g") 'kg/search-marked-region-if-available)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x 3") 'kg/split-right-and-move)
(global-set-key (kbd "C-x 2") 'kg/split-below-and-move)
(global-set-key (kbd "C-a") 'kg/beginning-of-line-dwim)
(global-set-key [(meta shift down)] 'kg/duplicate-start-of-line-or-region)
(global-set-key (kbd "C-c C-l") 'org-capture)
(global-set-key (kbd "<f6>") 'kg/show-user-config)
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "<s-S-return>") 'kg/toggle-maximize-buffer)
