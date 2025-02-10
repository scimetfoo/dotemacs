(setq gc-cons-threshold (* 50 1000 1000)  ;; 50MB during init
      gc-cons-percentage 0.6
      auto-window-vscroll nil
      inhibit-compacting-font-caches t
      read-process-output-max (* 1024 1024)) ;; 1mb

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000)))) ;; 2MB post init

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Initialize use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
