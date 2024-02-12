;;; init-lsp.el --- lsp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq lsp-log-io nil) ;; Don't log everything = speed
(setq lsp-restart 'auto-restart)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-position 'bottom)

(use-package lsp-mode
  :defer t
  :straight t
  :config
  (setq lsp-enable-file-watchers nil
	lsp-auto-guess-root t
	lsp-file-watch-threshold 3000)
  :commands lsp
  :hook ((typescript-mode
          python-mode
          js2-mode
          web-mode
          php-mode
          c-mode) . lsp)
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable nil))

(use-package company-lsp
  :ensure t)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
  :ensure t)

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
