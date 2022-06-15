;;; init-modeline.el --- Modline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq doom-modeline-modal-icon t)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(provide 'init-modeline)
;;; init-modeline.el ends here
