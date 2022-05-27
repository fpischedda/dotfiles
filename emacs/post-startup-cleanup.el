;;; package --- Summary
;;; Commentary:
;;; Startup optimizations cleanup
;;; Code:

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

;;; post-startup-cleanup.el ends here
