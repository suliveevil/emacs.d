(setq debug-on-error t)
;; (setq byte-compile-warnings not free-vars unresolved obsolete)
(setq byte-compile-warnings nil)

(setq confirm-kill-emacs (lambda (prompt) (y-or-n-p-with-timeout "确认退出？" 10 "y")))
;; (setq confirm-kill-emacs 'yes-or-no-p)
(setq use-short-answers t) ;; use y/n instead of yes/no

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 1024 1024 500)) ;; 500 MiB

;; Don't collect garbage when init
(setq gc-cons-threshold most-positive-fixnum) ;; 2^61 on my device

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(setq vc-follow-symlinks t)
