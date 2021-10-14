;;; init-php.el --- PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package php-mode
  :defer t
  ;; :init
  ;; (setq-default php-mode-coding-style 'psr2)
  :config 
  (setq php-mode-template-compatibility nil))

(use-package phpactor
  :defer t
  :commands (phpactor-install-or-update))

(use-package phpunit
  :defer t)

(straight-use-package
 '(el-patch :type git :host github :repo "stardiviner/ob-php"))

(use-package php-cs-fixer
  :defer t)

(use-package psysh
  :defer t)

(kd/my-local-leader-def'normal php-mode-map
			       "tt" 'phpunit-current-test
			       "tp" 'phpunit-current-project
			       "tc" 'phpunit-current-class
			       "n" 'phpactor-create-new-class
			       "m" 'phpactor-move-class
			       "i" 'phpactor-import-class
			       "f" 'php-search-documentation
			       "r" 'counsel-imenu)

(provide 'init-php)
;;; init-php.el ends here
