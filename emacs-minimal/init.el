(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package magit
  :ensure t)

(use-package vertico
  :ensure t
  :custom
  (verticle-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(defun customize-corfu ()
  (corfu-cycle t)	;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)	;; Enable auto completion
  (corfu-separator ?\s)	;; Orderless field separator
  )

(if (display-graphic-p)
  (use-package corfu
    :ensure t
    ;; Optional customizations
    :custom
    (customize-corfu)

    :init
    (global-corfu-mode))

  (use-package corfu-terminal
    :ensure t
    ;; Optional customizations
    :custom
    (customize-corfu)

    :init
    (corfu-terminal-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package modus-themes
  :ensure t)

(defun g-eshell ()
  "Start eshell at the root of the current project, or in the
   current directory if the current buffer is not part of a
   project."
  (interactive)
  (if (project-current)
      (project-eshell)
    (eshell)))

(use-package project
  :config
  (setq project-switch-commands 'project-dired)
  :bind-keymap
  (("C-c p" . project-prefix-map)))

(use-package eglot
  :ensure t
  :hook ((clojure-mode . eglot-ensure)
	 (javascript-mode . eglot-ensure))

(use-package clojure-mode
  :after eglot
  :ensure t)

(use-package cider
  :after clojure-mode
  :ensure t
  :init ())

(use-package paredit
  :ensure t
  :hook prog-mode
  :config (paredit-mode 1)
  :bind (("C->" . paredit-forward-slurp-sexp)
	 ("C-M->" . paredit-forward-barf-sexp)
	 ("C-<" . paredit-backward-barf-sexp)
	 ("C-M-<" . paredit-backword-slurp-sexp)))

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package ob-mermaid
  :ensure t
  :after org
  :config
  (setq ob-mermaid-cli-path "/home/foca/.nvm/versions/node/v19.6.0/bin/mmdc"))

(use-package zig-mode
  :ensure t)

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t
  :after geiser)

(use-package visual-fill-column
  :ensure t
  :config
  (setq visual-fill-column-width 110
	visual-fill-column-center-text t))

(use-package org-present
  :ensure t
  :after org-mode)

;; Configure fill width

(defun my/org-present-start ()
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (global-display-line-numbers-mode nil))

(defun my/org-present-end ()
  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  (global-display-line-numbers-mode))

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)

(add-hook 'org-present-mode-quit-hook 'my/org-present-end)

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x))
	  (exec-path-from-shell-initialize)))

(use-package evil
  :ensure t
  :bind (("C-u" . evil-scroll-up))
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

(use-package emacs
  :ensure nil
  :config
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq make-backup-files nil)		; stop creating ~ files
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; custom font
  (set-frame-font "Hack-12" nil t)

  (setq js-indent-level 2)

  (load custom-file)
  (load-theme 'modus-operandi)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-display-line-numbers-mode)
  (enable-paredit-mode)
  (global-set-key (kbd "C-c s") 'g-eshell)
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  )
