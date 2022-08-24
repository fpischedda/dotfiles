;;; package --- Summary
;;; Commentary:
;;; Org mode setup
;;; Code:

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
   (clojure . t)
   (python . t)))

;; used to render code snippets when exporting org files
(use-package htmlize
  :ensure t)

(use-package ox-reveal
  :ensure t
  :init (setq org-reveal-root "file:///~/reveal.js"))

(setq org-agenda-files (list "~/org/work.org"
                             "~/org/agenda.org"
                             "~/org/home.org"))

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
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org"))

;;; org.el ends here
