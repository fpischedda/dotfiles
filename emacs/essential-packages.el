;;; package --- Summary
;;; Commentary:
;;; Some unrelated but essential packages
;;; Code:

(use-package restart-emacs
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; use ripgrep to grep
(use-package deadgrep
  :ensure t
  :bind (( "C-x C-g" . deadgrep)))

(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
	  insert-directory-program nil))
  :custom
  (dired-listing-switches "-aBhl --group-directories-first"))

(use-package magit
  :ensure t
  :config
  (setq magit-log-section-commit-count 20)
  (global-set-key (kbd "C-x g") 'magit-status))

;;; essential-packages.el ends here
