;;; package --- Summary
;;; Commentary:
;;; Completion and project management
;;; Code:

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
  (setq projectile-globally-ignored-directories '("*.git" "*.projectile" "*build" "*env" "*env2" "*.shadow-cljs" "*.cache" "*__pycache__" "*.cpcache" "*.clj-kondo"))
  (setq projectile-sort-order 'modification-time)
  (setq projectile-project-search-path '("~/work/" "~/works/" "~/quicklisp/local-projects"))
  (projectile-mode +1))

(use-package flycheck
    :ensure t
    :hook
    ((prog-mode . flycheck-mode))
    :config
    (setq-default
     flycheck-display-errors-delay        0.3
     flycheck-check-syntax-automatically  '(mode-enabled save)
     flycheck-indication-mode             'right-fringe
     )
    (setq-default flycheck-gcc-language-standard "c++21")
    (setq-default flycheck-clang-language-standard "c++21")
    )

;;; completion-and-projects.el ends here
