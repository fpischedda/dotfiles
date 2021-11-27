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

(use-package all-the-icons
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns x))
	    (exec-path-from-shell-initialize)))

(use-package vterm
  :ensure t)

(use-package modus-themes
  :ensure t)

;; load selected theme
(load-theme 'modus-vivendi t)

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
(global-set-key [f1] 'vterm)

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

(use-package ranger :ensure t
  :commands (ranger)
  :bind (("C-x d" . deer))
  :config
  (setq ranger-cleanup-eagerly t)
  )

(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status))

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
  :config
  (evil-mode 1))

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

;; (use-package yasnippet
;;   :ensure t
;;   :init
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

; testing doom-modeline
(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-mode))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-word-1)
	 ("C-;" . avy-goto-word-0-regexp)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
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
  (setq projectile-project-search-path '("~/work/" "~/quicklisp/local-projects"))
  (projectile-mode +1))

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
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode)
	 ("\\.http\\'" . restclient-mode)))

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-mode t))

;; (use-package lsp-ui
;;   :ensure t)

;;;; Clojure
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
  :mode
;; Use clojure mode for other file extensions
  (("\\.edn$" . clojure-mode)
   ("\\.boot$" . clojure-mode)
   ("lein-env" . enh-ruby-mode))
  :config
  (require 'flycheck-clj-kondo)
  :hook
  ((clojure-mode . lsp)
   (clojurescript . lsp)
   (clojurec-mode . lsp))
  )

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

;; Clojure lsp settings
(setq lsp-clojure-custom-server-command '("bash" "-c" "~/bin/clojure-lsp"))


;; ;; Scheme
;; (use-package geiser-chez
;;   :ensure t)

;; org mode
(use-package org
  :ensure t
  :config
  (setf org-src-fontify-natively t)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t))

(use-package ob-restclient
  :ensure t)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (lisp . t)
   (clojure . t)
   (emacs-lisp . t)
   (plantuml . t)
   (restclient . t)
   (python . t)))

;; used to render code snippets when exporting org files
(use-package htmlize
  :ensure t)

(use-package ox-reveal
  :ensure t
  :init (setq org-reveal-root "file:///~/reveal.js"))

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

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
  :after org
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/org")
  :config (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
           ("C-c n r" . org-roam-node-random)
           (:map org-mode-map
                 (("C-c n i" . org-roam-node-insert)
                  ("C-c n o" . org-id-get-create)
                  ("C-c n t" . org-roam-tag-add)
                  ("C-c n a" . org-roam-alias-add)
                  ("C-c n l" . org-roam-buffer-toggle)))))

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

;; Python

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp))))  ; or lsp-deferred

(use-package pyvenv
  :ensure t)

(use-package yaml-mode
  :ensure t)

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

;; ;; Rust support
;; (use-package rust-mode
;;   :ensure t)

;; (use-package flycheck-rust
;;   :ensure t)

;; (use-package toml-mode
;;   :ensure t
;;   :mode (("\\.tscn\\'" . toml-mode)))

;; ;; c/c++ & lsp-mode
;; (setq c-default-style "linux"
;;       c-basic-offset 4)

;; (use-package rtags
;;   :ensure t
;;   :hook
;;   ((c++-mode . rtags-start-process-unless-running)
;;    (c-mode . rtags-start-process-unless-running))
;;   :config (setq rtags-completions-enabled t
;; 		rtags-path "~/.emacs.d/rtags/src/rtags.el"
;; 		rtags-rc-binary-name "~/bin/rc"
;; 		rtags-use-company t
;; 		rtags-rdm-binary-name "~/bin/rdm")
;;   :bind (("C-c E" . rtags-find-symbol)
;;   	 ("C-c e" . rtags-find-symbol-at-point)
;;   	 ("C-c O" . rtags-find-references)
;;   	 ("C-c o" . rtags-find-references-at-point)
;;   	 ("C-c s" . rtags-find-file)
;;   	 ("C-c v" . rtags-find-virtuals-at-point)
;;   	 ("C-c F" . rtags-fixit)
;;   	 ("C-c f" . rtags-location-stack-forward)
;;   	 ("C-c b" . rtags-location-stack-back)
;;   	 ("C-c n" . rtags-next-match)
;;   	 ("C-c p" . rtags-previous-match)
;;   	 ("C-c P" . rtags-preprocess-file)
;;   	 ("C-c R" . rtags-rename-symbol)
;;   	 ("C-c x" . rtags-show-rtags-buffer)
;;   	 ("C-c T" . rtags-print-symbol-info)
;;   	 ("C-c t" . rtags-symbol-type)
;;   	 ("C-c I" . rtags-include-file)
;;   	 ("C-c i" . rtags-get-include-file-for-symbol)))

;; (use-package flycheck-rtags
;;     :ensure t
;;     :after flycheck rtags
;;     :config
;;     (defun my-flycheck-rtags-setup ()
;;       (flycheck-select-checker 'rtags)
;;       (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;       (setq-local flycheck-check-syntax-automatically nil)
;;       )
;;     (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
;;     (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
;;     (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)
;;     )

;; (use-package ivy-rtags
;;   :ensure t
;;   :config
;;   (setq rtags-display-result-backend 'ivy))

;; (use-package company-rtags
;;   :ensure t
;;   :config (push 'company-rtags company-backends))


;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; the next are relative to the root maildir
;; (see `mu info`).
;; instead of strings, they can be functions too, see
;; their docstring or the chapter 'Dynamic folders'
(setq mu4e-sent-folder   "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder  "/Trash")

;; the maildirs you use frequently; access them with 'j' ('jump')
(setq   mu4e-maildir-shortcuts
    '((:maildir "/gmail/inbox" :key ?g)
      (:maildir "/yahoo/inbox"   :key ?y)
      (:maildir "/yandex/inbox"    :key ?x)))

;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
    '( (:date          .  25)    ;; alternatively, use :human-date
       (:flags         .   6)
       (:from          .  22)
       (:subject       .  nil))) ;; alternatively, use :thread-subject

;; ;; program to get mail; alternatives are 'fetchmail', 'getmail'
;; ;; isync or your own shellscript. called when 'U' is pressed in
;; ;; main view.

;; ;; If you get your mail without an explicit command,
;; ;; use "true" for the command (this is the default)
;; (setq mu4e-get-mail-command "mbsync -a")

;; ;; general emacs mail settings; used when composing e-mail
;; ;; the non-mu4e-* stuff is inherited from emacs/message-mode
;; (setq mu4e-compose-reply-to-address "francesco@pischedda.info"
;;       user-mail-address "francesco@pischedda.info"
;;       user-full-name  "Francesco Pischedda")

;; (setq mu4e-compose-signature
;;    "Francesco Pischedda\nhttps://francesco.pischedda.info")

;; ;; smtp mail setting
;; ;; MAYBE REFINE LATER
;; (setq sendmail-program "/usr/bin/msmtp"
;;       message-sendmail-f-is-evil t
;;       message-sendmail-extra-arguments '("--read-envelope-from")
;;       send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'message-send-mail-with-sendmail)

;; ;;    ;; if you need offline mode, set these -- and create the queue dir
;; ;;    ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
;; ;;    smtpmail-queue-mail  nil
;; ;;    smtpmail-queue-dir  "/home/user/Maildir/queue/cur")

;; ;; don't keep message buffers around
;; (setq message-kill-buffer-on-exit t)
;; ;; end of mu4e config

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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ob-restclient which-key vterm visual-fill use-package toml-mode smartparens slime rust-mode restclient restart-emacs ranger rainbow-delimiters pyvenv projectile plantuml-mode ox-reveal org-roam org-bullets nov modus-vivendi-theme modus-operandi-theme magit lsp-ui lsp-python-ms ivy-rtags htmlize geiser-chez flycheck-rust flycheck-rtags flycheck-clj-kondo exec-path-from-shell evil-surround evil-nerd-commenter evil-collection doom-modeline dockerfile-mode docker-compose-mode deadgrep counsel company-rtags clojure-mode-extra-font-locking clj-refactor avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
