;;; package --- Summary
;;; Commentary:
;;; Startup settings
;;; Code:

(require 'package)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(setq column-number-mode t)

(show-paren-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq default-frame-alist '((fullscreen . maximized)))

(eval-when-compile
  (require 'use-package))

(package-initialize)

;; custom font
(set-frame-font "Hack-14" nil t)

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns x))
	    (exec-path-from-shell-initialize)))

(use-package modus-themes
  :ensure t)

;; load selected theme
(load-theme 'modus-vivendi t)

(use-package all-the-icons
  :ensure t)

;;; startup.el ends here
