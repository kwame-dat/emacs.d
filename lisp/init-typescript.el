;;; init-typescript.el --- Typescript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package typescript-mode
  :after tree-sitter
  :config
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; (use-package tsi
;;   :after tree-sitter
;;   :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))


;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; (use-package apheleia
;;   :ensure t
;;   :config
;;   (setf (alist-get 'prettier apheleia-formatters)
;;         '(npx "prettier"
;;               "--trailing-comma"  "es5"
;;               "--bracket-spacing" "true"
;;               "--single-quote"    "true"
;;               "--semi"            "false"
;;               "--print-width"     "100"
;;               file))
;;   (apheleia-global-mode +1))

(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--trailing-comma"  "es5"
              "--tab-width"       "2"
              "--semi"            "true"
              "--single-quote"    "true"
              "--quote-props"    "as-needed"
              "--bracket-same-line" "true"
              file))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier))
  (apheleia-global-mode t))

(provide 'init-typescript)
;;; init-typescript.el ends here
