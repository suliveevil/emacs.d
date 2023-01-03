;;; terminal-focus-reporting-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from terminal-focus-reporting.el

(defvar terminal-focus-reporting-mode nil "\
Non-nil if Terminal-Focus-Reporting mode is enabled.
See the `terminal-focus-reporting-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `terminal-focus-reporting-mode'.")
(custom-autoload 'terminal-focus-reporting-mode "terminal-focus-reporting" nil)
(autoload 'terminal-focus-reporting-mode "terminal-focus-reporting" "\
Minor mode for terminal focus reporting integration.

This is a global minor mode.  If called interactively, toggle the
`Terminal-Focus-Reporting mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='terminal-focus-reporting-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "terminal-focus-reporting" '("terminal-focus-reporting-"))

;;; End of scraped data

(provide 'terminal-focus-reporting-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; terminal-focus-reporting-autoloads.el ends here