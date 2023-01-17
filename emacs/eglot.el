;;; package --- Installs eglot if missing (will be builtin in next emacs versions)
;;; Commentary: hooks commonly used languages to eglot

(use-package eglot
  :ensure t
  :defer t

  :hook ((python-mode . eglot-ensure)
	 (clojure-mode . eglot-ensure)
	 (clojurescript-mode . eglot-ensure)
	 (clojurec-mpde . eglot-ensure)))

;;; eglot.el ends here
