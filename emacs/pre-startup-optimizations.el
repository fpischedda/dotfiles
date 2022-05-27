;;; package --- Summary
;;; Commentary:
;;; Some startup optimizations taken from doom
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

;;; pre-startup-optimizations.el ends here
