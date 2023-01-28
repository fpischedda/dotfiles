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

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator

  :init
  (global-corfu-mode))

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
  :ensure t)

(use-package clojure-mode
  :after eglot
  :ensure t
  :hook (clojure-mode . eglot-ensure))
  
(use-package cider
  :after clojure-mode
  :ensure t
  :init ())

(use-package evil
  :ensure t
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package paredit
  :ensure t
  :hook prog-mode
  :config (paredit-mode 1)
  :bind (("C->" . paredit-forward-slurp-sexp)
	 ("C-M->" . paredit-forward-barf-sexp)
	 ("C-<" . paredit-backward-barf-sexp)
	 ("C-M-<" . paredit-backword-slurp-sexp)))

(use-package emacs
  :ensure nil
  :config
  (setq make-backup-files nil) ; stop creating ~ files
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)
  (load-theme 'modus-vivendi)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-display-line-numbers-mode)
  (enable-paredit-mode)
  (global-set-key (kbd "C-c s") 'g-eshell)
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  )
