;;; package --- Summary
;;; Commentary:
;;; customized by Francesco Pischedda
(require 'package)

;;; Code:
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)

(show-paren-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; This is only needed once, near the top of the file
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(use-package zenburn-theme
  :ensure t
  :init (load-theme 'zenburn t))

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config (evil-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :config (evilnc-default-hotkeys))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package wakatime-mode
  :ensure t
  :init (global-wakatime-mode))

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

(use-package ranger :ensure t
  :commands (ranger)
  :bind (("C-x d" . deer))
  :config
  (setq ranger-cleanup-eagerly t)
  )

(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status))

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

					; powerline - now disabled while testing doom-modeline
(use-package powerline
  :ensure t
  :config (powerline-default-theme))

(use-package all-the-icons
  :ensure t)

; testing doom-modeline
(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(use-package avy
  :ensure t)

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
         ("C-'" . ivy-avy)) ; C-' to ivy-avy
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

(use-package counsel :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . counsel-ag)        ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate))   ; search for files or else using locate
  )

(use-package jedi
  :ensure t
  :init (setq jedi:complete-on-dot t))

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-backend "jedi")
  (setq python-check-command "flake8")
  :bind
  (("s-." . elpy-goto-definition)
   ("s->" . pop-tag-mark)
   )
  :config
  (elpy-enable)
  (yas-minor-mode)
  (jedi:setup))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-mode t))

(use-package ox-reveal
  :ensure t
  :init
  (setq org-reveal-root "file:///Users/francescopischedda/reveal.js/"))

;; run multi-term pressing F1
(global-set-key [f1] 'term)

(use-package rainbow-delimiters
  :ensure t
  :config (rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :config (paredit-mode))

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t
  :init
;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)
;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)
  )

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljc.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; A little more syntax highlighting
(use-package clojure-mode-extra-font-locking
  :ensure t)

;; Elixir related pachages
(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t)

(use-package flycheck-mix
  :ensure t)

;; org mode
(use-package org
  :ensure t)

(use-package ox-reveal
  :ensure t
  :init (setq org-reveal-root "file:///~/reveal.js"))

; ORG mode customizations
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

; redifine some modeline
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")
(rename-modeline "python-mode" python-mode "Py")

; a fix needed to run ipython4 as a shell inside emacs
; link: https://www.reddit.com/r/Python/comments/4w5d4e/psa_ipython_5_will_break_emacs_heres_how_to_fix_it/
(setq python-shell-interpreter "ipython"
  python-shell-interpreter-args "--simple-prompt")

(use-package neotree
  :ensure t
  :commands (neotree-toggle)
  :bind (("M-n" . neotree-toggle))
)

(use-package yaml-mode
  :ensure t)

(use-package ansible
  :ensure t
  :hook (yaml-mode . (lambda () (ansible 1)))
  :commands (ansible))

(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cider paredit-mode zenburn-theme web-mode tagedit slime-clj slime rainbow-delimiters pylint projectile powerline-evil ox-reveal org-bullets multi-term magit-popup jedi-direx ivy helm golint go-complete go-autocomplete go git-commit flycheck-pyflakes exec-path-from-shell evil-surround erlang elpy elixir-yasnippets elixir-mix django-mode darkokai-theme cython-mode column-marker column-enforce-mode clojure-mode-extra-font-locking clj-refactor calfw-gcal calfw android-mode alchemist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
