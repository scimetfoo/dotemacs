;; -*- coding: utf-8; lexical-binding: t; -*-
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Bootstrap configuration for straight.el
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

(require 'package)
(setq package-enable-at-startup nil)   ; To prevent initialising twice
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (file-exists-p "~/.emacs.d/init-user.el")
  (setq user-custom-file "~/.emacs.d/init-user.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/custom.el")
  (setq user-custom-file "~/.emacs.d/custom.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/org.el")
  (setq user-custom-file "~/.emacs.d/org.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/config.el")
  (setq user-custom-file "~/.emacs.d/config.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/clojure.el")
  (setq user-custom-file "~/.emacs.d/clojure.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/haskell.el")
  (setq user-custom-file "~/.emacs.d/haskell.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/rust.el")
  (setq user-custom-file "~/.emacs.d/rust.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/ruby.el")
  (setq user-custom-file "~/.emacs.d/ruby.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/purs.el")
  (setq user-custom-file "~/.emacs.d/purs.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/python.el")
  (setq user-custom-file "~/.emacs.d/python.el")
  (load user-custom-file))

(when (file-exists-p "~/.emacs.d/go.el")
  (setq user-custom-file "~/.emacs.d/go.el")
  (load user-custom-file))
