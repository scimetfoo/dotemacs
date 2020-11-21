(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(use-package clojure-mode-extra-font-locking)

(define-clojure-indent
  (defrecord 1))

(use-package cider
  :init
  (setq cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        cider-repl-history-size 100
        cider-repl-use-clojure-font-lock t
        cider-docview-fill-column 70
        cider-stacktrace-fill-column 76
        nrepl-popup-stacktraces nil
        nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";; => "
        cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var))

  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") 'cider-repl-clear-buffer)))
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook
            (lambda ()
              (local-set-key (kbd "<C-return>") 'cider-eval-last-sexp)
              (local-set-key (kbd "C-c C-n") 'cider-eval-buffer))))


(use-package clj-refactor
  :config
  (setq cljr-warn-on-eval nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1) ; for adding require/use/import statements
              ;; This choice of keybinding leaves cider-macroexpand-1 unbound
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(require 'ob-clojure)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)))
(require 'cider)
(setq org-babel-clojure-backend 'cider)

(use-package flycheck-clj-kondo
  :config
  (remove-hook 'clojure-mode-hook
               (lambda ()
                 (require 'flycheck-clj-kondo))))

(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defun transpose-words-with-hyphens (arg)
  "Treat hyphens as a word character when transposing words"
  (interactive "*p")
  (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
    (transpose-words arg)))

(define-key clojure-mode-map (kbd "M-t") 'transpose-words-with-hyphens)
