;;; init-lib.el
;; packages (not in elpa/melpa/gnu/nongnu mirror) configuration

;; keyfreq: Track Emacs commands frequency
;; {{{
;; keyfreq fork: keyfreq-html-v2 show keyboard heat map
(require 'keyfreq) ;; 导入插件包
(setq keyfreq-folder "~/.config/emacs/lib/keyfreq")
(keyfreq-mode 1)          ;; 启动插件包
(keyfreq-autosave-mode 1) ;; 自动保存模式
(setq-default keyfreq-file "~/.config/emacs/assets/keyfreq-log")
;; (defun turnon-keyfreq-mode ()
;;   "Turn on keyfreq."
;;   (interactive)
;;   (my-run-with-idle-timer 4 (lambda () ;; ;; Fire up keyfreq a few seconds later to start up emacs faster
;;                               (keyfreq-mode 1)
;;                               (keyfreq-autosave-mode 1))))
;;
;; }}}

;; keyferq: 排除命令: exclude commands
;; {{{
(with-eval-after-load 'keyfreq
  (setq keyfreq-excluded-commands
        '(
          ;; abort-recursive-edit
          ;; ace-window
          ;; avy-goto-line
          ;; backward-char
          ;; clipboard-kill-ring-save
          ;; comint-previous-input
          ;; comint-send-input
          ;; delete-backward-char
          ;; describe-variable
          ;; electric-pair-delete-pair
          ;; eval-buffer
          ;; exit-minibuffer
          ;; ffip
          ;; forward-char
          ;; goto-line
          ;; hippie-expand
          ;; indent-new-comment-line
          ;; ispell-minor-check
          ;; js-mode
          ;; js2-line-break
          ;; kill-sentence
          ;; left-char
          ;; mac-mwheel-scroll
          ;; magit-next-line
          ;; magit-previous-line
          ;; markdown-exdent-or-delete
          ;; markdown-outdent-or-delete
          ;; minibuffer-complete
          ;; minibuffer-complete-and-exit
          ;; minibuffer-keyboard-quit
          ;; mouse-drag-region
          ;; mouse-set-point
          ;; move-beginning-of-line
          ;; move-end-of-line
          ;; mwheel-scroll
          ;; my-company-number
          ;; my-setup-develop-environment
          ;; newline-and-indent
          ;; next-history-element
          ;; next-line
          ;; package-menu-execute
          ;; pcomplete
          ;; previous-history-element
          ;; previous-line
          ;; push-button
          ;; pwd
          ;; quit-window
          ;; recenter-top-bottom
          ;; right-char
          ;; rjsx-electric-gt
          ;; rjsx-electric-lt
          ;; self-insert-command
          ;; shellcop-erase-buffer
          ;; smarter-move-beginning-of-line
          ;; suspend-frame
          ;; term-send-raw
          ;; turnon-keyfreq-mode
          ;; typescript-insert-and-indent
          ;; undefined ;; lambda function
          ;; wgrep-finish-edit
          ;; xterm-paste
          ;; yank
          )) )
;; }}}

;; keyfreq: 正则表达式排除模式, excluded regexp
;; {{{
;; (with-eval-after-load 'keyfreq
;; (setq keyfreq-excluded-regexp
;;       '(
;;         "^ace-jump-"
;;         "^backward-"
;;         "^company-"
;;         "^dired"
;;         "^evil-"
;;         "^forward-"
;;         "^general-dispatch-self-insert-command-"
;;         "^gnus-"
;;         "^ido-"
;;         "^isearch-"
;;         "^ivy-"
;;         "^keyboard-"
;;         "^keyfreq-"
;;         "^my-hydra-.*/body"
;;         "^next-"
;;         "^org-"
;;         "^paredit-"
;;         "^save-"
;;         "^scroll-"
;;         "^select-window-"
;;         "^undo-"
;;         "^w3m-"
;;         "^web-mode"
;;         "^y-or-n-"
;;         "^yas-"
;;         "emms-"
;;        )))
;; }}}

;; elisp-demos
;; {{{
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
;; }}}

;; fuck
;; {{{
(use-package fuck
  ;; :defer 2
  )
;; }}}

;; unicode
;; {{{
(require 'modeline-char)
(add-hook 'after-init-hook 'mlc-char-in-mode-line-mode-global)
;; }}}


;; org-auto-tangle
;; {{{
(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  )
;; }}}


;; lsp-bridge
;; {{{
;; (use-package lsp-bridge
;;   :after (yasnippet)
;;   ;; :hook (prog-mode . lsp-bridge-mode)
;;   :config
;;   (global-lsp-bridge-mode)
;;   (add-to-list 'lsp-bridge-org-babel-lang-list "emacs-lisp")
;;   (add-to-list 'lsp-bridge-org-babel-lang-list "shell")
;;   )
(run-with-idle-timer
 1 nil
 #'(lambda ()
     (require 'yasnippet)
     (yas-global-mode 1)
     (require 'lsp-bridge)
     (global-lsp-bridge-mode)
     ))
;; }}}

;; D2 Mode
;; {{{
(add-to-list 'auto-mode-alist '("\\.d2" . d2-mode))
(defvar d2-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'d2-compile)
    (keymap-set map "C-c C-f" #'d2-compile-file)
    (keymap-set map "C-c C-b" #'d2-compile-buffer)
    (keymap-set map "C-c C-r" #'d2-compile-region)
    (keymap-set map "C-c C-h" #'d2-compile-file-and-browse)
    (keymap-set map "C-c C-j" #'d2-compile-buffer-and-browse)
    (keymap-set map "C-c C-k" #'d2-compile-region-and-browse)
    (keymap-set map "C-c C-o" #'d2-open-browser)
    (keymap-set map "C-x C-o" #'d2-view-current-svg)
    (keymap-set map "C-c C-d" #'d2-open-doc)
    map))
;; (org-babel-do-load-languages
;;     'org-babel-load-languages
;;     '(
;;       ;; (mermaid . t)
;;       (scheme . t)
;;       (d2 . t)))
;; }}}

;; subed: subtitle edit
;; {{{
(use-package subed
  ;; :ensure t
  :config
  ;; Disable automatic movement of point by default
  (add-hook 'subed-mode-hook 'subed-disable-sync-point-to-player)
  ;; Remember cursor position between sessions
  (add-hook 'subed-mode-hook 'save-place-local-mode)
  ;; Break lines automatically while typing
  (add-hook 'subed-mode-hook 'turn-on-auto-fill)
   ;; Break lines at 40 characters
  (add-hook 'subed-mode-hook (lambda () (setq-local fill-column 40))))
;; }}}

(provide 'init-lib)

;;; init-lib.el ends here
