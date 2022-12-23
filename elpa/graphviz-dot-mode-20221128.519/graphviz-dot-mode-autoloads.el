;;; graphviz-dot-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from company-graphviz-dot.el

(autoload 'company-graphviz-dot-backend "company-graphviz-dot" "\
Company backend for `graphviz-dot-mode'.
In the signature, COMMAND, ARG and IGNORED are mandated by `company-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t)
(register-definition-prefixes "company-graphviz-dot" '("company-g"))


;;; Generated autoloads from graphviz-dot-mode.el

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "\
Major mode for the dot language. \\<graphviz-dot-mode-map>
TAB indents for graph lines.

\\[graphviz-dot-indent-graph]	- Indentation function.
\\[graphviz-dot-preview]	- Previews graph in a buffer.
\\[graphviz-dot-view]	- Views graph in an external viewer.
\\[graphviz-dot-indent-line]	- Indents current line of code.

Variables specific to this mode:

  `graphviz-dot-dot-program'                   (default `dot')
       Program used to compile the graphs.
  `graphviz-dot-preview-extension'             (default `png')
       File type to use for output.
  `graphviz-dot-view-command'                  (default `dotty %s')
       Command to run when `graphviz-dot-view' is executed.
  `graphviz-dot-view-edit-command'             (default nil)
       If the user should be asked to edit the view command.
  `graphviz-dot-save-before-view'              (default t)
       Automatically save current buffer berore `graphviz-dot-view'.

(fn)" t)
(autoload 'graphviz-dot-preview "graphviz-dot-mode" "\
Compile the graph between BEGIN and END and preview it in an other buffer.

BEGIN (resp. END) is a number defaulting to
`point-min' (resp. `point-max') representing the current buffer's
point where the graph definition starts (resp. stops).

(fn &optional BEGIN END)" t)
(autoload 'graphviz-turn-on-live-preview "graphviz-dot-mode" "\
Turn on live preview.
This will update the preview on every save." t)
(autoload 'graphviz-turn-off-live-preview "graphviz-dot-mode" "\
Turn off live preview.
Saving the file will no longer also update the preview." t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))
(register-definition-prefixes "graphviz-dot-mode" '("dot-menu" "graphviz-"))

;;; End of scraped data

(provide 'graphviz-dot-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; graphviz-dot-mode-autoloads.el ends here
