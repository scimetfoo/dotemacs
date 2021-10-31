(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
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
(require 'cider)
(setq org-babel-clojure-backend 'cider)

(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :hook (clojure-mode . aggressive-indent-mode))

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
