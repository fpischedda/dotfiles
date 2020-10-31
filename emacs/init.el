;;; package --- Summary
;;; Commentary:
;;; customized by Francesco Pischedda
(require 'package)

;;; Code:

;; next two forms should speed up startup time according to this post
;; https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/fhicvbj?utm_source=share&utm_medium=web2x
(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)

;; these are take from the following article about how doom emacs optimizes startup time
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
(defvar doom-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6) ;; this will be reset at the end of the config

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; <-- end of startup time "optimizations"

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

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))

;; custom font
(set-frame-font "Hack-13" nil t)

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns x))
	    (exec-path-from-shell-initialize)))

(use-package vterm
  :ensure t)

(use-package modus-operandi-theme
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t)

(use-package material-theme
  :ensure t)

(use-package nord-theme
  :ensure t)

(use-package monochrome-theme
  :ensure t)

(use-package acme-theme
  :ensure t)

;; load selected theme
(load-theme 'modus-operandi t)

(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
	  insert-directory-program "/usr/local/bin/gls"))
  :custom
  (dired-listing-switches "-aBhl --group-directories-first"))

(use-package restart-emacs
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; run multi-term pressing F1
(global-set-key [f1] 'term)

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
  :config (paredit-mode))

(use-package ranger :ensure t
  :commands (ranger)
  :bind (("C-x d" . deer))
  :config
  (setq ranger-cleanup-eagerly t)
  )

(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status))

(global-set-key (kbd "C->") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-<") 'paredit-backward-barf-sexp)
(global-set-key (kbd "C-M->") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "C-M-<") 'paredit-backward-slurp-sexp)

(use-package plantuml-mode
  :ensure t)

(use-package deadgrep
  :ensure t
  :bind (("<f5>" . deadgrep)))

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

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

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

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (setq projectile-globally-ignored-files-suffixes '("pyc" "class" "obj"))
  (setq projectile-globally-ignored-directories '(".git" ".projectile" "build" "env" "env2" ".shadow-cljs"))
  (setq projectile-sort-order 'modification-time)
  (setq projectile-project-search-path '("~/work/"))
  (projectile-mode +1))

(use-package flx-ido
  :ensure t)

(use-package company
  :config
  (setq company-idle-delay 0.3)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package flycheck
    :ensure t
    :init
    (add-hook 'prog-mode-hook #'flycheck-mode)
    :config
    (setq-default
     flycheck-display-errors-delay        0.3
     flycheck-check-syntax-automatically  '(mode-enabled save)
     flycheck-indication-mode             'right-fringe
     )
    (setq-default flycheck-gcc-language-standard "c++17")
    (setq-default flycheck-clang-language-standard "c++17")
    )

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))
  (add-to-list 'auto-mode-alist '("\\.rest-client$" . restclient-mode)))

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-mode t))

(use-package cider
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
  (setq cider-lein-parameters "repl :headless :host 127.0.0.1")
  :bind (("C-." . cider-docview-source)))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

;; Use clojure mode for other file extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; A little more syntax highlighting
(use-package clojure-mode-extra-font-locking
  :ensure t)

;; Clojure refactoring module
(use-package clj-refactor
  :ensure t
  :config
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  (cljr-add-keybindings-with-prefix "C-c C-m")
  )

;; Elixir related pachages - temporarily disabled
;; (use-package elixir-mode
;;   :ensure t)

;; (use-package alchemist
;;   :ensure t)

;; (use-package flycheck-mix
;;   :ensure t)

;; org mode
(use-package org
  :ensure t
  :config
  (setf org-src-fontify-natively t)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (lisp . t)
   (emacs-lisp . t)
   (plantuml . t)
   (python . t)))

;; used to render code snippets when exporting org files
(use-package htmlize
  :ensure t)

(use-package ox-reveal
  :ensure t
  :init (setq org-reveal-root "file:///~/reveal.js"))

; ORG mode customizations
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ +\\([-*]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(setq org-agenda-files (list "~/org/work.org"
                             "~/org/agenda.org"
                             "~/org/home.org"))

;; shortcut to open org index
(set-register ?o '(file . "~/org/index.org"))

;; custom TODO workflow
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "WIP(w)" "|" "DONE(d)")
        (sequence "REPORT(R)" "BUG(B)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "BLOCKED(b)")
        (sequence "|" "RECURRING(r)")
        (sequence "|" "CANCELED(c)")))

;; trying org-roam
(use-package org-roam
  :ensure t
  :init (add-hook 'after-init-hook 'org-roam-mode)
  :config (setq org-roam-directory "~/org"))

; redifine some modeline
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(setq js-indent-level 2)
(setq css-indent-offset 2)

(rename-modeline "clojure-mode" clojure-mode "Clj")
(rename-modeline "python-mode" python-mode "Py")

(use-package pyvenv
  :ensure t)

; a fix needed to run ipython4 as a shell inside emacs
; link: https://www.reddit.com/r/Python/comments/4w5d4e/psa_ipython_5_will_break_emacs_heres_how_to_fix_it/
(setq python-shell-interpreter "ipython"
  python-shell-interpreter-args "--simple-prompt")

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

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Rust support
(use-package rust-mode
  :ensure t)

(use-package flycheck-rust
  :ensure t)

(use-package toml-mode
  :ensure t
  :mode (("\\.tscn\\'" . toml-mode)))

;; c/c++ & lsp-mode
(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

(use-package rtags
  :ensure t
  :hook (c++-mode . rtags-start-process-unless-running)
  :config (setq rtags-completions-enabled t
		rtags-path "~/.emacs.d/rtags/src/rtags.el"
		rtags-rc-binary-name "~/bin/rc"
		rtags-use-company t
		rtags-rdm-binary-name "~/bin/rdm")
  :bind (("C-c E" . rtags-find-symbol)
  	 ("C-c e" . rtags-find-symbol-at-point)
  	 ("C-c O" . rtags-find-references)
  	 ("C-c o" . rtags-find-references-at-point)
  	 ("C-c s" . rtags-find-file)
  	 ("C-c v" . rtags-find-virtuals-at-point)
  	 ("C-c F" . rtags-fixit)
  	 ("C-c f" . rtags-location-stack-forward)
  	 ("C-c b" . rtags-location-stack-back)
  	 ("C-c n" . rtags-next-match)
  	 ("C-c p" . rtags-previous-match)
  	 ("C-c P" . rtags-preprocess-file)
  	 ("C-c R" . rtags-rename-symbol)
  	 ("C-c x" . rtags-show-rtags-buffer)
  	 ("C-c T" . rtags-print-symbol-info)
  	 ("C-c t" . rtags-symbol-type)
  	 ("C-c I" . rtags-include-file)
  	 ("C-c i" . rtags-get-include-file-for-symbol)))

(use-package flycheck-rtags
    :ensure t
    :after flycheck rtags
    :config
    (defun my-flycheck-rtags-setup ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil)
      )
    (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
    (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
    (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)
    )

(use-package ivy-rtags
  :ensure t
  :config
  (setq rtags-display-result-backend 'ivy))

(use-package company-rtags
  :ensure t
  :config (push 'company-rtags company-backends))


;;; .emacs ends here

;;set GC to "sane" default again, it has been disabled to make startup faster
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)
    (setq file-name-handler-alist doom--file-name-handler-alist)))

;; minibuffer related optimizations take from doom emacs
(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold doom-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-lein-parameters "repl :headless :host 0.0.0.0")
 '(clojure-indent-style :always-indent)
 '(custom-safe-themes
   '("6339e18e32734507d5b70817fbd490cdf1761826d7445153215ad7ee63ee3931" "afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" "befd48e22121985f7bdbb188704bd3fdb12697d5031c0f0c4d4cb34f8fa3024c" "248bdabe33857dd0f9700a4630526f5a0f4867afab418188f8f2eaca73dfc6c8" "18cc5dd284685a224ab24873911cfb467a06c1fe45b121c8aa073920179ce070" "aec089c0a0e043589bf8d9d99fe2e330073a3a1e75c7eb51262835d6046a9e22" "41fa754692f20e6ed36b6038f710ff56c247f05de6edb6d5680215ffabfb88a2" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "d6c5b8dc6049f2e9dabdfcafa9ef2079352640e80dffe3e6cc07c0f89cbf9748" "28caf31770f88ffaac6363acfda5627019cac57ea252ceb2d41d98df6d87e240" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "37ba833442e0c5155a46df21446cadbe623440ccb6bbd61382eb869a2b9e9bf9" "abdb1863bc138f43c29ddb84f614b14e3819982936c43b974224641b0b6b8ba4" default))
 '(dired-listing-switches "-aBhl --group-directories-first")
 '(initial-frame-alist '((fullscreen . maximized)))
 '(org-agenda-files '("~/org/agenda.org" "~/org/home.org"))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(vterm org-roam acme-theme flycheck-clj-kondo pyvenv all-the-icons-ivy-rich modus-operandi-theme modus-vivendi-theme htmlize ivy-rtags flycheck-rtags ccls rtags monochrome-theme nord-theme dracula-theme phps-mode company-lsp lsp-ui lsp-mode use-package flycheck-rust rust-mode material-theme paper-theme auto-org-md markdown-mode cider-eval-sexp-fu flx-ido discover w3m evil-collection-neotree restclient cframe restart-emacs treemacs-projectile treemacs-magit treemacs-evil treemacs mastodon groovy-mode jenkins flycheck-plantuml plantuml-mode all-the-icons-ivy cider paredit-mode zenburn-theme web-mode tagedit slime-clj slime rainbow-delimiters pylint projectile powerline-evil ox-reveal org-bullets multi-term magit-popup jedi-direx ivy helm golint go-complete go-autocomplete go git-commit flycheck-pyflakes exec-path-from-shell evil-surround erlang elpy elixir-yasnippets elixir-mix django-mode darkokai-theme cython-mode column-marker column-enforce-mode clojure-mode-extra-font-locking clj-refactor calfw-gcal calfw android-mode alchemist)))
 ;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
