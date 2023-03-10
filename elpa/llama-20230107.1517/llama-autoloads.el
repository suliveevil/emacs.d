;;; llama-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from llama.el

(autoload '## "llama" "\
Expand to a `lambda' expression that wraps around FN and ARGS.

This `lambda' expression calls the function FN with arguments
ARGS and returns its value.  Its own arguments are derived from
symbols found in ARGS.  Each symbol from `%1' through `%9', which
appears in ARGS, is treated as a positional argument.  Missing
arguments are named `_%N', which keeps the byte-compiler quiet.
In place of `%1' the shorthand `%' can be used, but only one of
these two can appear in ARGS.  `%*' represents extra `&rest'
arguments.

Instead of:

  (lambda (a _ c &rest d)
    (foo a (bar c) d))

you can use this macro and write:

  (##foo % (bar %3) %*)

which expands to:

  (lambda (% _%2 %3 &rest %*)
    (foo % (bar %3) %*))

The name `##' was choosen because that allows (optionally)
omitting the whitespace between it and the following symbol.
It also looks a bit like #\\='function.

(fn FN &rest ARGS)" nil t)
(register-definition-prefixes "llama" '("llama--"))

;;; End of scraped data

(provide 'llama-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; llama-autoloads.el ends here
