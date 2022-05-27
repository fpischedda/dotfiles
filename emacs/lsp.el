;;; package --- Summary
;;; Commentary:
;;; lsp related setup
;;; Code:

(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")

  :config
  (setq ;; recommended
        gc-cons-threshold (* 100 1024 1024)
	read-process-output-max (* 1024 1024))
  (setq ;; optional
   lsp-lens-enable t
   lsp-semantic-tokens-enable t
   cljr-add-ns-to-blank-clj-files nil
   cider-eldoc-display-for-symbol-at-point nil)

  :hook ((python-mode . lsp-deferred)
	 (clojure-mode . lsp-deferred)
	 (clojurescript-mode . lsp-deferred)
	 (clojurec-mpde . lsp-deferred)))

;; Provides visual help in the buffer
;; For example definitions on hover.
;; The `imenu` lets me browse definitions quickly.
(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-sideline-enable nil
	lsp-ui-doc-delay 2)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ("C-c i" . lsp-ui-imenu)))

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp))))  ; or lsp-deferred

;;; lsp.el ends here
