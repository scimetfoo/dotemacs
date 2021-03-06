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

;;(require 'shm)
