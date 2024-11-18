;;; init-eglot.el --- eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'eglot)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'php-mode-hook 'eglot-ensure)


(kd/leader-key-def
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'eldoc-doc-buffer
  "lr" 'xref-find-references)

(provide 'init-eglot)
;;; init-eglot.el ends here
