;; Reduce the frequency of garbage collection by making it happen on
;; ~100MB of allocated data, lower it to the ddefault after init.

(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Automatically reload files when they change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;;
;; APPEARANCE
;;

;; don't show the tool bar
(tool-bar-mode -1)

;; don't show the scroll bar
(scroll-bar-mode -1)

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
(set-frame-font "Menlo 13" nil t)
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
(global-set-key (kbd "s-<left>") (kbd "C-a"))   ;; Beginning of line

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

;; Provide it so that it may be required

(provide 'config)
