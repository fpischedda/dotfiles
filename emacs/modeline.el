;;; package --- Summary
;;; Commentary:
;;; setting a modeline
;;; Code:

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

; redifine some modeline
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; rename modeline defined in startup.el
(rename-modeline "clojure-mode" clojure-mode "Clj")
(rename-modeline "python-mode" python-mode "Py")
(rename-modeline "js2-mode" js2-mode "JS2")

;;; modeline.el ends here
