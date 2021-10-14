;;; init-autocompletion.el --- autocompletion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package company
  :defer t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :config
  (setq company-global-modes
	'(not
	  erc-mode
	  eshell-mode
	  gud-mode
	  org-mode
	  help-mode
	  message-mode
	  shell-mode
	  vterm-mode)

	company-selection-wrap-around t
	company-idle-delay 0
	company-minimum-prefix-length 1
	company-selection-wrap-around t
	company-tooltip-align-annotations t
	company-require-match 'never
	company-auto-commit nil
	company-auto-commit-chars nil
	company-dabbrev-other-buffers nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil)
  )

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-autocompletion)
;;; init-autocompletion.el ends here
