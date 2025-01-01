;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq package-enable-at-startup nil)
(package-initialize)
(setq tramp-histfile-override "/dev/null")
(setq org-element-use-cache nil)
(setq tramp-default-method "ssh")
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;; Native Compilation
(setq comp-async-report-warnings-errors nil)
(setq native-comp-async-report-warnings-errors nil)

;; (require 'init-benchmarking) ;; Measure startup time
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
(delete-selection-mode t)


(defun conditional-disable-modes ()
  (when (> (buffer-size) (* 3 1024 1024))
    (flycheck-mode -1)
    (font-lock-mode -1)
    (fundamental-mode)
    (which-function-mode -1)
    (linum-mode 0)
    )
  )
(add-hook 'prog-mode-hook 'conditional-disable-modes)
(add-hook 'text-mode-hook 'conditional-disable-modes)


;; Bootstrap config
(require 'init-package-management)
(require 'init-exec-path-from-shell)
(require 'init-directory-clean)
(require 'init-keyboard-bindings)

;; General Configuration
(require 'init-fonts)
(require 'init-user-interface)
(require 'init-themes) ;; stop theme compilation
(require 'init-modeline)
(require 'init-completion)
(require 'init-treemacs)
(require 'init-workspace) ;; fix persp mode
(require 'init-configuration-files)
(require 'init-search-n-lookup)
(require 'init-notifications)

;; File Browsing
(require 'init-dired)

;; Window
(require 'init-window-management)

;; Development
(require 'init-git)
(require 'init-projectile)

;; Tools
(require 'init-docker)
(require 'init-kubernetes)
(require 'init-vagrant)
(require 'init-editorconfig)
(require 'init-elastic-search)
(require 'init-debugger)
(require 'init-lsp)
;; (require 'init-eglot)

;; Productivity
(require 'init-syntax-checking)
(require 'init-spell-checking)
(require 'init-grammar-checking)
(require 'init-autocompletion)
(require 'init-snippets)
(require 'init-multiple-cursors)
(require 'init-smart-parens)
(require 'init-colour-highlighting)
(require 'init-writing)

;; Languages
(require 'init-php)
(require 'init-web)
(require 'init-javascript)
(require 'init-typescript)
(require 'init-dotenv)
(require 'init-css)
(require 'init-haskell)
(require 'init-restclient)
(require 'init-csv)
(require 'init-markdown)
(require 'init-fountain)
(require 'init-yaml)
(require 'init-finance)
(require 'init-org-mode)
(require 'init-diagrams)

;; Applications
(kd/leader-key-def
  "a"  '(:ignore t :which-key "apps"))
(require 'init-calendar)
(require 'init-alert)
(require 'init-chat)
(require 'init-eshell)
(require 'init-terminal)
(require 'init-password-manager)
(require 'init-reading)
(require 'init-touch-typing)
(require 'init-mail)
(require 'init-eaf)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f" "b11edd2e0f97a0a7d5e66a9b82091b44431401ac394478beb44389cf54e6db28" "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875" default))
 '(doom-modeline-check-simple-format t nil nil "Customized with use-package doom-modeline")
 '(shell-pop-full-span t)
 '(shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm))))
 '(shell-pop-term-shell "/usr/zsh")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 50))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
