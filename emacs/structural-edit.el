;;; package --- Summary
;;; Commentary:
;;; Setting up treemacs
;;; Code:

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :config
  (paredit-mode)
  (global-set-key (kbd "C->")   'paredit-forward-barf-sexp)
  (global-set-key (kbd "C-<")   'paredit-backward-barf-sexp)
  (global-set-key (kbd "C-M->") 'paredit-forward-slurp-sexp)
  (global-set-key (kbd "C-M-<") 'paredit-backward-slurp-sexp))

;;; structural-edit.el ends here
