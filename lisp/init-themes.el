;;; init-themes.el --- themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'modus-themes)

;; mode-line
(setq modus-themes-mode-line '(accented borderless padded))

(setq modus-themes-region '(bg-only))

(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-variable-pitch-ui t)
(setq modus-themes--weight-widget 'light)
(setq modus-themes-paren-match '(bold intense))
(load-theme 'modus-vivendi t)

(provide 'init-themes)
;;; init-themes.el ends here
