;;; package --- Summary
;;; Commentary:
;;; Clojure related setup
;;; Code:

(use-package cider
  :ensure t
  :hook ((cider-mode . sb/unload-cider-jumps)
	 (cider-repl-mode . sb/unload-cider-jumps))
  :config
  (defun sb/unload-cider-jumps ()
    ;; I prefer lsp's jumps, so kindly don't steal them
    (define-key cider-mode-map (kbd "M-.") nil)
    (define-key cider-mode-map (kbd "M-,") nil))
  :custom
  (cider-prompt-for-symbol nil)
  (cider-redirect-server-output-to-repl nil)
  (cider-prefer-local-resources t)
  (cider-auto-track-ns-form-changes t)
  (cider-repl-pop-to-buffer-on-connect t)
  (cider-eldoc-display-context-dependent-info t)
  (cider-font-lock-dynamically '(macro core function var))
  (cider-auto-select-error-buffer t)
  :init
;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)
  (setq ;; optional
   cljr-add-ns-to-blank-clj-files t
   cider-eldoc-display-for-symbol-at-point nil)
  :bind (("C-." . cider-docview-source)))

;; (use-package flycheck-clj-kondo
;;   :ensure t)

(use-package clojure-mode
  :ensure t
  :mode
;; Use clojure mode for other file extensions
  (("\\.edn$" . clojure-mode)
   ("\\.bb$" . clojure-mode)
   ("\\.boot$" . clojure-mode)))

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

;;; clojure.el ends here
