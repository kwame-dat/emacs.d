;;; init-eglot.el --- eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package eglot
  :straight t)

(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))
  (add-to-list 'eglot-server-programs '(web-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-stay-out-of 'eldoc)
  (with-eval-after-load "php-mode"
    (define-key php-mode-map (kbd "<f5>") #'eldoc-box-eglot-help-at-point)))



(provide 'init-eglot)
;;; init-eglot.el ends here
