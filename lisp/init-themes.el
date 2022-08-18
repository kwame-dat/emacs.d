;;; init-themes.el --- themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (straight-use-package 'modus-themes)
(straight-use-package 'doom-themes)

;; mode-line
;; (setq modus-themes-mode-line '(accented borderless padded))

;; (setq modus-themes-region '(bg-only))

;; (setq modus-themes-bold-constructs t)
;; (setq modus-themes-italic-constructs t)
;; (setq modus-themes-paren-match '(bold intense))
;; (load-theme 'modus-vivendi t)

(load-theme 'doom-tomorrow-night t)

(provide 'init-themes)
;;; init-themes.el ends here
