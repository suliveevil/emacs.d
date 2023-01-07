;;; parrot-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from parrot.el

(defvar parrot-mode nil "\
Non-nil if Parrot mode is enabled.
See the `parrot-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `parrot-mode'.")
(custom-autoload 'parrot-mode "parrot" nil)
(autoload 'parrot-mode "parrot" "\
Use Parrot to show when you're rotating.

You can customize this minor mode, see option `parrot-mode'.

This is a global minor mode.  If called interactively, toggle the
`Parrot mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='parrot-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "parrot" '("parrot-"))


;;; Generated autoloads from parrot-rotate.el

(register-definition-prefixes "parrot-rotate" '("parrot-rotate-" "pulse-flag"))

;;; End of scraped data

(provide 'parrot-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; parrot-autoloads.el ends here
