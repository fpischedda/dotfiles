(require 'package)

;; Fix path
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package magit
  :ensure t)

(use-package elfeed
  :ensure t)

(defun customize-corfu ()
  (setq
   corfu-cycle t	;; Enable cycling for `corfu-next/previous'
   corfu-auto t	;; Enable auto completion
   corfu-quit-no-match 'separator
   corfu-auto-prefix 2
   corfu-auto-delay 0
   corfu-preselect-first nil
   corfu-separator ?\s)	;; Orderless field separator
  )

(if (display-graphic-p)
    (use-package corfu
      :ensure t
      ;; Optional customizations
      :config
      (customize-corfu)

      :init
      (global-corfu-mode)
      )

  (use-package corfu-terminal
    :ensure t
    ;; Optional customizations
    :config
    (customize-corfu)

    :init
    (corfu-terminal-mode)
    ))

(use-package vertico
  :ensure t
  :custom
  (verticle-cycle t)
  :init
  (vertico-mode)
  :config
  (setq vertico-count 35))

(use-package vertico-posframe
  :ensure t
  :init (vertico-posframe-mode 1))

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

;; Add extensions
(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p" . completion-at-point) ;; capf
         ("M-p t" . complete-tag)        ;; etags
         ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ("M-p s" . cape-elisp-symbol)
         ("M-p e" . cape-elisp-block)
         ("M-p a" . cape-abbrev)
         ("M-p l" . cape-line)
         ("M-p w" . cape-dict)
         ("M-p :" . cape-emoji)
         ("M-p \\" . cape-tex)
         ("M-p _" . cape-tex)
         ("M-p ^" . cape-tex)
         ("M-p &" . cape-sgml)
         ("M-p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  )

(use-package consult
  :ensure t
  :config (advice-add #'project-find-regexp :override #'consult-ripgrep))

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package modus-themes
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

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
  (setq project-switch-commands 'project-find-file)
  :bind-keymap
  (("C-c p" . project-prefix-map)))

;; Reference post
;; https://www.adventuresinwhy.com/post/eglot/
(defun configure-lsp ()
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources
			     ["flake8"]
			     :plugins (:pycodestyle (:enabled :json-false)
						    :pyflakes (:enabled :json-false)
						    :flake8 (:enabled :json-false :maxLineLength 88)
						    :ruff (:enabled t :lineLength 88)
						    :pydocstyle (:enabled t :convention "numpy")
						    :yapf (:enabled :json-false)
						    :autopep8 (:enabled :json-false)
						    :black (:enabled t :line_length 88 :cache_config t)))))))

;; Open python files in tree-sitter mode.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
	      ("C-c C-d" . eldoc)
	      ("C-c C-e" . eglot-rename)
	      ("C-c C-o" . python-sort-imports)
	      ("C-c C-f" . eglot-format-buffer))

  :hook (javascript-mode . eglot-ensure)
  :config (configure-lsp))

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . eglot-ensure))

(use-package python-mode
  :ensure t
  :hook ((python-ts-mode . eglot-ensure)
	 (python-ts-mode . flyspell-prog-mode)
         (python-ts-mode . hs-minor-mode)
         (python-ts-mode . (lambda () (set-fill-column 88))))
  :config
  (setq paredit-space-for-delimiter-predicates nil))

(use-package cider
  :after clojure-mode
  :ensure t
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (remove-hook 'eldoc-documentation-functions #'cider-eldoc)
  (cider-auto-test-mode 1))

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
  :hook (org-mode . org-bullets-mode))

(use-package zig-mode
  :ensure t
  :hook (zig-mode . eglot-ensure)
  :config
  (setq paredit-space-for-delimiter-predicates nil))

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t
  :after geiser)

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode 1))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package ediprolog
  :ensure t)

(use-package denote
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :hook (org-mode . writeroom-mode))

(use-package emacs
  :ensure nil
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)
  (load-theme 'modus-operandi-tinted)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq make-backup-files nil)		; stop creating ~ files
  ;; custom font
  (add-to-list 'default-frame-alist '(font . "Hack-14"))

  (setq indent-tabs-mode nil)
  (setq c-default-style "bsd"
        c-basic-offset 4)
  (setq js-indent-level 2)
  (setq-default css-indent-offset 2)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-display-line-numbers-mode)
  (enable-paredit-mode)

  (global-set-key (kbd "C-c s") 'g-eshell)
  (global-set-key (kbd "C-c i") 'run-erc)

  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  )

(defun get-local-secret (name)
  (substring
   (shell-command-to-string (format "pass %s" name))
   0 -1))

(setq erc-email-userid "fpsd_codes/irc.libera.chat") ;; Example with Libera.Chat

(defun run-erc ()
  (interactive)
  (erc-tls :server "chat.sr.ht"
           :port 6697
           :nick "fpsd_codes"
           :password (get-local-secret "srht/fpsd_codes/access_token")))
