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
(require 'init-tree-sitter)

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
(require 'init-eglot)

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

(use-package eaf
  :straight (eaf
             :type git
             :host github
             :repo "emacs-eaf/emacs-application-framework"           
             :files ("*.el" "*.py" "core" "app" "*.json")
             :includes (eaf-pdf-viewer eaf-browser) ; Straight won't try to search for these packages when we make further use-package invocations for them
             :pre-build (("python3" "install-eaf.py" "--install" "pdf-viewer" "browser" "--ignore-sys-deps"))
             )
  :init (evil-set-initial-state 'eaf-mode 'emacs))

(setq eaf-browser-dark-mode "false")
(use-package eaf-browser
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t))

(use-package eaf-pdf-viewer)
(use-package eaf-terminal)
;; (use-package eaf-map)
;; (use-package eaf-markdown-previewer)
;; (use-package eaf-file-browser)
;; (use-package eaf-file-manager)
;; (use-package eaf-mindmap)

(use-package eaf-video-player)
;; (use-package eaf-org-previewer)
;; (use-package eaf-git)

;; (require 'eaf-demo)
;; (require 'eaf-file-sender)
;; (require 'eaf-music-player)
;; (require 'eaf-js-video-player)
;; (require 'eaf-camera)
;; (require 'eaf-rss-reader)
;; (require 'eaf-terminal)
;; (require 'eaf-image-viewer)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-map)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-file-browser)
;; (require 'eaf-file-manager)
;; (require 'eaf-mindmap)
;; (require 'eaf-vue-tailwindcss)
;; (require 'eaf-video-player)
;; (require 'eaf-org-previewer)
;; (require 'eaf-airshare)
;; (require 'eaf-jupyter)
;; (require 'eaf-netease-cloud-music)
;; (require 'eaf-git)
;; (require 'eaf-system-monitor)
;; (require 'eaf-pyqterminal)
;; (require 'eaf-markmap)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1f82b7df8a4ce0e2ba3b0217d2c552b2fe5b5dd1244481cb65bef733f9818c63" "a74c85e1fdda49173902f7a1e92db7caf8bffcefc5c634bb37918cb8eb5174ec" "adb567dc2208b9e95ce3bc1930bf0e84a3eced91e674b4c35777e54650a60763" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" default))
 '(doom-modeline-check-simple-format t nil nil "Customized with use-package doom-modeline")
 '(org-agenda-files
   '("/Users/kwamedat/org/calendar/scal.org" "/Users/kwamedat/org/calendar/ecal.org" "/Users/kwamedat/org/calendar/jcal.org" "/Users/kwamedat/org/calendar/wcal.org" "/Users/kwamedat/org/calendar/pcal.org" "/Users/kwamedat/org/ptodo.org"))
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
