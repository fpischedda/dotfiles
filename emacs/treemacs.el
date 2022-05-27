;;; package --- Summary
;;; Commentary:
;;; Setting up treemacs
;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-no-png-images t
	treemacs-width 24)
  :bind ("C-c t" . treemacs))

(use-package treemacs-evil
  :ensure t)

(use-package treemacs-projectile
  :ensure t)

;;; treemacs.el ends here
