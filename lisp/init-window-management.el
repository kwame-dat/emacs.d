;;; init-window-management.el --- window-manangement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Frame Scaling / Zooming
(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package shackle
  :config
  (setq shackle-rules '(
			(compilation-mode :noselect t :popup t :align below :size 0.4)
			("\\`\\*magit.*?\\*\\'" :regexp t :select t :align below :popup t :size 0.4)
			)
	shackle-default-rule '(:select t))
  (shackle-mode))

;; Window history
(use-package winner
  :after evil
  :config
  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

(general-def 'normal
  "C-j" 'evil-window-down
  "C-h" 'evil-window-left
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right)

(provide 'init-window-management)
;;; init-window-management.el ends here
