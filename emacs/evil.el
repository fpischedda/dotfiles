;;; package --- Summary
;;; Commentary:
;;; Setting up treemacs
;;; Code:

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config (evil-mode 1)
  (add-to-list 'evil-emacs-state-modes 'lisp-mode))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :config (evilnc-default-hotkeys))

(use-package evil-surround
  :ensure t
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;; evil.el ends here
