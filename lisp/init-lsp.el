;;; init-lsp.el --- lsp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq lsp-log-io nil) ;; Don't log everything = speed
;; (setq lsp-restart 'auto-restart)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-code-actions t)
;; (setq lsp-ui-sideline-enable t)
;; (setq lsp-ui-doc-position 'bottom)
(setq lsp-idle-delay 0.500)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (php-mode . lsp)
         (python-mode . lsp)
         (js2-mode . lsp)
         (web-mode . lsp)
         (typescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)

(kd/leader-key-def
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(provide 'init-lsp)
;;; init-lsp.el ends here
