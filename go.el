;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
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
