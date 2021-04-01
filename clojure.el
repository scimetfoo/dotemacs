(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(use-package clojure-mode-extra-font-locking)

(define-clojure-indent
  (defrecord 1))

;; (add-to-list 'load-path "/Users/murtaza/dev/cider") ;; load the local instance of cider (for when fiddling with cider)
;; (load "cider-autoloads" t t)
(use-package cider
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") 'cider-repl-clear-buffer)))
  (add-hook 'cider-mode-hook #'company-mode))

(use-package clj-refactor
  :config
  (setq cljr-warn-on-eval nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(require 'ob-clojure)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)))
(require 'cider)
(setq org-babel-clojure-backend 'cider)

(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

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
