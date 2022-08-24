;;; package --- Summary
;;; Commentary:
;;; customized by Francesco Pischedda
;;; Code:

(load-file "~/.emacs.d/pre-startup-optimizations.el")

(load-file "~/.emacs.d/startup.el")

(load-file "~/.emacs.d/essential-packages.el")

(load-file "~/.emacs.d/structural-edit.el")

(load-file "~/.emacs.d/evil.el")

(load-file "~/.emacs.d/modeline.el")

(load-file "~/.emacs.d/clojure.el")

(load-file "~/.emacs.d/lsp.el")

(load-file "~/.emacs.d/treemacs.el")

(load-file "~/.emacs.d/org.el")

(load-file "~/.emacs.d/completion-and-projects.el")

;;; Languages tweaks and extra modes
;; JS
(setq-default js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq-default css-indent-offset 2)

;; Python
(use-package pyvenv
  :ensure t)

(use-package restclient
  :ensure t
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode)
	 ("\\.http\\'" . restclient-mode)))

(use-package plantuml-mode
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

(use-package toml-mode
  :ensure t
  :mode (("\\.tscn\\'" . toml-mode)))

;; scheme
(use-package geiser-guile
  :ensure t)

;;;; EPUB Reader
;; For now used only by nov mode
(use-package visual-fill
  :ensure t)

(use-package nov
  :ensure t
  :config
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook
  ((nov-mode . visual-line-mode)
   (nov-mode . visual-fill-column-mode)))

(load-file "~/.emacs.d/post-startup-cleanup.el")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs-projectile treemacs-evil treemacs which-key vterm visual-fill use-package toml-mode smartparens slime restclient restart-emacs ranger rainbow-delimiters pyvenv projectile-ripgrep plantuml-mode ox-reveal org-roam org-bullets nov modus-themes magit lsp-ui lsp-python-ms htmlize geiser-guile flycheck exec-path-from-shell evil-surround evil-nerd-commenter evil-collection doom-modeline dockerfile-mode docker-compose-mode deadgrep counsel clojure-mode-extra-font-locking clj-refactor chess all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
