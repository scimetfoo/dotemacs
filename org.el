(defun inhibit-global-linum-mode ()
  "Counter-act `global-linum-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda () (linum-mode 0))
            :append :local))

(use-package org
  :ensure t
  :config (progn
            (setq org-todo-keywords (quote ((sequence "DISCUSS" "SPIKE" "ESTIMATE" "TODO" "IN-PROGRESS" "REVIEW" "DEFERED" "REDO" "|" "CANCELLED" "PUNTED" "DELEGATED" "DONE"))))
            (add-hook 'org-shiftup-final-hook 'windmove-up)
            (add-hook 'org-shiftleft-final-hook 'windmove-left)
            (add-hook 'org-shiftdown-final-hook 'windmove-down)
            (add-hook 'org-shiftright-final-hook 'windmove-right))
  :init (progn
          (add-hook 'org-mode-hook 'inhibit-global-linum-mode)
          (add-hook 'org-mode-hook 'flyspell-mode)
          ;;(add-hook 'org-mode-hook 'auto-org-md-mode)
          (add-hook 'org-mode-hook 'org-bullets-mode)))

(use-package org-bullets
  :ensure t
  :config (setq org-bullets-bullet-list '("◉" "⌘" "○" "⌗")))

(require 'ob)
(require 'ob-tangle)
