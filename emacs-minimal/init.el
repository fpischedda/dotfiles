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

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package cider
  :ensure t)

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

(use-package emacs
  :ensure nil
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (line-number-mode t)
  (load-theme 'modus-vivendi)
  (global-set-key (kbd "C-c s") 'g-eshell))
