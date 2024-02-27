;;; init-eglot.el --- eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package eglot
  :straight t)

(use-package eglot
  :straight t
  :hook ((( typescript-mode php-mode clojure-mode clojurec-mode clojurescript-mode
            java-mode scala-mode)
          . eglot-ensure)
         ((cider-mode eglot-managed-mode) . eglot-disable-in-cider))
  :preface
  (defun eglot-disable-in-cider ()
    (when (eglot-managed-p)
      (if (bound-and-true-p cider-mode)
          (progn
            (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
            (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet)))

(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))
  (add-to-list 'eglot-server-programs '(web-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-stay-out-of 'eldoc)
  (with-eval-after-load "php-mode"
    (define-key php-mode-map (kbd "<f5>") #'eldoc-box-eglot-help-at-point)))



(provide 'init-eglot)
;;; init-eglot.el ends here
