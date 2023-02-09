;; [[file:README.org::*File Header][File Header:1]]
;; -*- coding: utf-8; lexical-binding: t; -*-
;; -*- origami-fold-style: triple-braces -*-
;;; init.el

;; Date: 2023-02-08T23:45:08+0800
;; File Header:1 ends here

;; [[file:README.org::*describe-random-interactive-function][describe-random-interactive-function:1]]
;; random function
;; {{{
(defun my/describe-random-interactive-function ()
  "Show the documentation for a random interactive function.
Consider only documented, non-obsolete functions."
  (interactive)
  (let (result)
    (mapatoms
     (lambda (s)
       (when (and (commandp s)
                  (documentation s t)
                  (null (get s 'byte-obsolete-info)))
         (setq result (cons s result)))))
    (describe-function (elt result (random (length result))))))
;; }}}
;; describe-random-interactive-function:1 ends here

;; [[file:README.org::*package initialize][package initialize:1]]
;; package.el: mirror 插件镜像
;; {{{
;; GitHub connection: https://github.com/hedzr/mirror-list
;; (require 'package)
;; 代理
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (setq url-proxy-services '(("no_proxy" . "^\\(192\\.168\\..*\\)")
;;                            ("http" . "<代理 IP>:<代理端口号>")
;;                            ("https" . "<代理 IP>:<代理端口号>")))
;;
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;
;; emacs-eask/archives: Magic to prevent refreshing package archives failure
;; https://github.com/emacs-eask/archives
;;
(package-initialize) ;; pair with (setq package-enable-at-startup nil) ;; early-init
;;
;; 防止反复调用 package-refresh-contents 影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))
;;
;; https://emacs-china.org/t/topic/2671/13
;; 把刷新的代码放到对包检查的代码里，避免不缺包的情况下联网查询的操作。
;; (dolist (package my-package-list)
;;   (unless (package-installed-p package)
;;     (unless package-archive-contents
;;       (package-refresh-contents))
;;     (package-install package)))

;; }}}
;; package initialize:1 ends here

;; [[file:README.org::*package list][package list:1]]
(setq package-list-unversioned t)
;; package list:1 ends here

;; [[file:README.org::*package dependency graph][package dependency graph:1]]
;; package dependency graph (Graphviz)
;; {{{
;; https://emacs-china.org/t/package/22775/2?u=suliveevil
;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html#Loop-Facility
;; (defun get-pkg-reqs-alist ()
(defun my/emacs-package-dependency ()
  (interactive)
  (cl-loop for pkg-and-desc in package-alist
           for pkg = (car pkg-and-desc)
           for desc = (cadr pkg-and-desc)
           for req-names = (cl-loop for it in (package-desc-reqs desc)
                                    collect (car it))
           collect (cons pkg req-names)))
;; (setq info (get-pkg-reqs-alist))

(setq info (my/emacs-package-dependency))

;; (with-temp-file "/tmp/g.dot"
(with-temp-file (expand-file-name
                 "assets/emacs-package-dependency.dot"
                 (concat user-emacs-directory)
                 )
  (insert "digraph G {")
  (insert (mapconcat #'identity
                     (cl-loop for pkg-reqs in info
                              for pkg = (car pkg-reqs)
                              for reqs = (cdr pkg-reqs)
                              nconcing (cl-loop for req in reqs
                                                collect
                                                (format
                                                 "\"%s\" -> \"%s\";\n"
                                                 pkg
                                                 req)))))
  (insert "}"))
;; }}}
;; package dependency graph:1 ends here

;; [[file:README.org::*use-package][use-package:1]]
;; use-package
;; {{{
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)
(setq use-package-expand-minimally t)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package)
;;   (eval-when-compile (require 'use-package)))
(use-package package
  :ensure nil
  :config
  (setq package-quickstart t)
  ;; post-command-hook package-update list-packages
  )
;; }}}
;; use-package:1 ends here

;; [[file:README.org::*keybinding][keybinding:1]]
;; https://emacs.stackexchange.com/a/654
(defun key-binding-at-point (key)
  (mapcar (lambda (keymap) (when (keymapp keymap)
                             (lookup-key keymap key)))
          (list
           ;; More likely
           (get-text-property (point) 'keymap)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'keymap))
                   (overlays-at (point)))
           ;; Less likely
           (get-text-property (point) 'local-map)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'local-map))
                   (overlays-at (point))))))

(defun my/keymaps-at-point ()
  "List entire keymaps present at point."
  (interactive)
  (let ((map-list
         (list
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'keymap))
                  (overlays-at (point)))
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'local-map))
                  (overlays-at (point)))
          (get-text-property (point) 'keymap)
          (get-text-property (point) 'local-map))))
    (apply #'message
           (concat
            "Overlay keymap: %s\n"
            "Overlay local-map: %s\n"
            "Text-property keymap: %s\n"
            "Text-property local-map: %s")
           map-list)))

(defun my/locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kLocate keyBinding: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))
;; keybinding:1 ends here

;; [[file:README.org::*keymap][keymap:1]]
(use-package emacs
  :ensure nil
  :bind
  (
   ("C-s-j" . end-of-buffer)
   ("C-s-k" . beginning-of-buffer)
   )
  )
;; keymap:1 ends here

;; [[file:README.org::*repeat-mode][repeat-mode:1]]
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :bind
  ("M-o" . other-window)
  :init
  (setq repeat-echo-function 'repeat-echo-message)
  (put 'other-window 'repeat-map nil)
  )
;; repeat-mode:1 ends here

;; [[file:README.org::*repeat-mode][repeat-mode:2]]
(defvar isearch-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "s" #'isearch-repeat-forward)
    (keymap-set map "r" #'isearch-repeat-backward)
    map))

(dolist (cmd '(isearch-repeat-forward isearch-repeat-backward))
  (put cmd 'repeat-map 'isearch-repeat-map))
;; repeat-mode:2 ends here

;; [[file:README.org::*触控板和鼠标 touchpad/trackpad & mouse][触控板和鼠标 touchpad/trackpad & mouse:1]]
;; touchpad/trackpad & mouse
;; {{{
(setq mouse-wheel-tilt-scroll t) ; Make the direction sane on an apple trackpad
(setq mouse-wheel-flip-direction t)
;;
;; (defun mouse-hover-tooltip (&optional arg)
;;   "Show mouse hover help info using pos-tip-show."
;;   (interactive)
;;   (let ((help (help-at-pt-kbd-string)))
;;     (if help
;;         (pos-tip-show help nil nil nil 0)
;;       (if (not arg) (message "No local help at point"))))
;;   (unwind-protect
;;       (push (read-event) unread-command-events)
;;     (pos-tip-hide)))
;; }}}
;; 触控板和鼠标 touchpad/trackpad & mouse:1 ends here

;; [[file:README.org::*触控板和鼠标 touchpad/trackpad & mouse][触控板和鼠标 touchpad/trackpad & mouse:2]]
(xterm-mouse-mode 1)
;; menu-bar-mode

;; (add-hook 'after-make-frame-functions
;;   (lambda ()
;;     ;; we do something only in terminal Emacs
;;     (unless (display-graphics-p)
;;       (xterm-mouse-mode 1)))
;; 触控板和鼠标 touchpad/trackpad & mouse:2 ends here

;; [[file:README.org::*光标和选区 cursor/region/selection][光标和选区 cursor/region/selection:2]]
;; cursor
;; {{{
;; cursor move
;; Emacs 一行内移动 cursor 的最佳方案是什么？ - Emacs China
;; https://emacs-china.org/t/emacs-cursor/6753/12
;; make cursor the width of the character it is under i.e. full width of a TAB
(setq x-stretch-cursor t) ;; When on a tab, make the cursor the tab length.
;; cursor line: 光标所在行显示/高亮
;; (global-hl-line-mode t) ;; highlight current line
(custom-set-faces '(hl-line ((t (:background "grey")))))
(delete-selection-mode t) ;; 删除选中的文字或选中文字后输入时替换选中的文字
;; }}}
;; 光标和选区 cursor/region/selection:2 ends here

;; [[file:README.org::*光标和选区 cursor/region/selection][光标和选区 cursor/region/selection:3]]
(put 'narrow-to-region 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
;; (put 'list-timers 'disabled nil)
;; 光标和选区 cursor/region/selection:3 ends here

;; [[file:README.org::*光标和选区 cursor/region/selection][光标和选区 cursor/region/selection:4]]
(setq
 scroll-step 1
 scroll-margin 15
 scroll-conservatively 10000
 )
;; 光标和选区 cursor/region/selection:4 ends here

;; [[file:README.org::*Emacs Lisp][Emacs Lisp:1]]
(use-package emacs
  :ensure nil
  :bind
  (
   :map emacs-lisp-mode-map
   ("C-c M-e" . my/eval-current-elisp-func-only)
   ("C-c M-r" . my/eval-current-elisp-func-and-run)
   )
  :config
  (defun my/eval-current-elisp-func (&optional run)
    " eval-last-sexp 当前光标处的 function
根据 run 来决定是否要运行"
    (interactive)
    (let* ((current-pos (point)) func-start-pos func-end-pos fun-name)
      (when (string= major-mode "emacs-lisp-mode")
        (save-excursion
          (beginning-of-defun)
          (setq func-start-pos (point))
          (end-of-defun)
          (setq func-end-pos (point)))
        (when (and (>= current-pos func-start-pos) (< current-pos func-end-pos))
          (evil-backward-section-begin)
          (evil-jump-item)
          (eval-last-sexp nil)   ;; 不把执行的结果插入到当前 buffer 中
          (goto-char current-pos)
          (when run
            (setq fun-name (format "(%s)" (lisp-current-defun-name)))
            (eval (read fun-name)))))))

  (defun my/eval-current-elisp-func-only ()
    " eval-last-sexp 当前光标处的 function "
    (interactive)
    (my/eval-current-elisp-func))

  (defun my/eval-current-elisp-func-and-run ()
    " eval-last-sexp 当前光标处的 function 并运行"
    (interactive)
    (my/eval-current-elisp-func t))
  )
;; Emacs Lisp:1 ends here

;; [[file:README.org::*Emacs Lisp][Emacs Lisp:2]]

;; Emacs Lisp:2 ends here

;; [[file:README.org::*帮助文档 eldoc help info man-pages...][帮助文档 eldoc help info man-pages...:2]]
(use-package eldoc
  :ensure nil
  ;; :after flymake
  :hook (emacs-lisp-mode . eldoc-mode)
  )
;; 帮助文档 eldoc help info man-pages...:2 ends here

;; [[file:README.org::*时间与日期 date & time][时间与日期 date & time:1]]
;; time
;; {{{
(use-package iso8601
  :ensure nil
  :defer t
  :hook (kill-emacs . my/log-emacs-uptime)
  :bind ("C-c d t" . my/date-and-time-iso8601)
  :config
  (defun my/date-and-time-iso8601 ()
    (interactive)
    (insert (format-time-string "%FT%T%z")))

  ;; https://emacsredux.com/blog/2014/12/23/uptime/
  (defvar my/emacs-uptime-log
    ;; (locate-user-emacs-file "uptime.log")
    (expand-file-name "assets/uptime.org" user-emacs-directory)
    "Log file for `my/log-emacs-uptime'.")
  (defun my/log-emacs-uptime ()
    "Write emacs uptime to `my/emacs-uptime-log'. Use with `kill-emacs-hook'."
    (with-temp-buffer
      (insert
       "|"
       (format-time-string "%FT%T%z" before-init-time)
       " | "
       (format-time-string "%FT%T%z" (current-time))
       " | "
       (emacs-uptime)
       " |"
       "\n")
      (append-to-file nil nil my/emacs-uptime-log)))
  )
;; }}}
;; 时间与日期 date & time:1 ends here

;; [[file:README.org::*剪贴板与寄存器 clipboard & register][剪贴板与寄存器 clipboard & register:2]]
(use-package emacs
  :ensure nil
  :defer t
  :bind
  ;; ("C-c H-k" . yank-from-kill-ring)
  ("M-z" . zap-up-to-char)
  )
;; 剪贴板与寄存器 clipboard & register:2 ends here

;; [[file:README.org::*剪贴板与寄存器 clipboard & register][剪贴板与寄存器 clipboard & register:3]]
(use-package register
  :ensure nil
  :defer t
  )
;; 剪贴板与寄存器 clipboard & register:3 ends here

;; [[file:README.org::*注释 comment][注释 comment:1]]
;; comment
;; {{{
(use-package emacs
  :ensure nil
  :defer t
  :bind
  ("H-/" . comment-current-line-dwim)
  :config
  (defun comment-current-line-dwim ()
    "Comment or uncomment the current line/region."
    (interactive)
    (save-excursion
      (if (use-region-p)
          (comment-or-uncomment-region (region-beginning) (region-end))
        (push-mark (beginning-of-line) t t)
        (end-of-line)
        (comment-dwim nil)
        )
      )
    )
  )
;; }}}
;; 注释 comment:1 ends here

;; [[file:README.org::*命令历史][命令历史:3]]
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-autosave-interval 300)
  (setq enable-recursive-minibuffers t)
  (setq history-length 1024)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(
          (comint-input-ring        . 50)
          (dired-regexp-history     . 20)
          (face-name-history        . 20)
          (kill-ring                . 20)
          (regexp-search-ring       . 20)
          (search-ring              . 20)
          extended-command-history
          global-mark-ring
          mark-ring
          regexp-search-ring
          register-alist
          ))
  )
;; 命令历史:3 ends here

;; [[file:README.org::*文件历史][文件历史:1]]
(use-package recentf
  :ensure nil
  :defer 1
  ;; :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 256)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup 'never)
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-exclude
   `(,@(cl-loop for f in `(,package-user-dir
                           ;; ,no-littering-var-directory
                           ;; ,no-littering-etc-directory
                           )
                collect
                (abbreviate-file-name f))
     `("/tmp/" "/ssh:"
       ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'"))
     ;; Folders on macOS start
     ".cache"
     ".cask"
     ".elfeed"
     "/TAGS\\'"
     "/ssh\\(x\\)?:"
     "/su\\(do\\)?:"
     "COMMIT_EDITMSG\\'"
     "\\*crontab\\*"
     "^/private/tmp/"
     "^/tmp/"
     "^/usr/include/"
     "^/var/folders/"
     "bookmarks"
     "cache"
     "elfeed"
     "ido.*"
     "persp-confs"
     "recentf"
     "undo-tree-hist"
     "url"
     ;; Folders on macOS end
     ))
  :config
  (recentf-mode +1)
  (defun my/recentf-ido-find-file ()
    "Find a recent file using ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))
  )
;; 文件历史:1 ends here

;; [[file:README.org::*光标位置历史][光标位置历史:1]]
;; 自动记住每个文件的最后一次访问的光标位置
(use-package saveplace
  :ensure nil
  ;; :defer 1
  :hook (after-init . save-place-mode)
  )
;; 光标位置历史:1 ends here

;; [[file:README.org::*缓冲区 buffer][缓冲区 buffer:1]]
(use-package emacs
  :ensure nil
  :bind
  (("C-c b f" . next-buffer)
   ("C-c b b" . previous-buffer) ("C-c b l" . list-buffers)
   ;; esc-map
   ;; ("M-b f" . next-buffer)
   ;; ("M-b b" . previous-buffer)
   ;; ("M-b l" . list-buffers)
   )
  :custom
  ;; (async-shell-command-buffer 'new-buffer)
  (shell-command-dont-erase-buffer 'end-last-out)
  (tab-always-indent 'complete) ; free the M-TAB keybinding
  :init
  (add-to-list
   'display-buffer-alist
   (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))))
;; 缓冲区 buffer:1 ends here

;; [[file:README.org::*行 line][行 line:1]]
;; line
;; {{{
(use-package display-line-numbers
  :ensure nil
  :hook
  ;; (prog-mode . display-line-numbers-mode)
  (after-init . global-display-line-numbers-mode)
  :bind
  ("C-c M-o"   . open-newline-above)
  :config
  (setq-default display-line-numbers-widen t) ; Keep line numbers inside a narrow
  (setq display-line-numbers-width-start t)
  (setq display-line-numbers-grow-only t)    ;; do not shrink line number width
  (setq display-line-numbers-type 'relative) ;; 相对行号

  ;; new line
  ;; https://github.com/manateelazycat/open-newline

  (defun open-newline-above (arg)
    "Move to the previous line (like vi) and then opens a line."
    (interactive "p")
    (beginning-of-line)
    (open-line arg)
    (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
        (indent-according-to-mode)
      (beginning-of-line)))

  (defun open-newline-below (arg)
    "Move to the next line (like vi) and then opens a line."
    (interactive "p")
    (end-of-line)
    (open-line arg)
    (call-interactively 'next-line arg)
    (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
        (indent-according-to-mode)
      (beginning-of-line)))
  )
;; }}}
;; 行 line:1 ends here

;; [[file:README.org::*行 line][行 line:2]]
(use-package emacs
  :ensure nil
  ;; :bind
  ;; (
  ;;  fill-paragraph
  ;;  )
  :init
  ;; wrap/truncate: word-wrap-mode
  (setq-default truncate-lines nil) ; nil equals wrap
  (setq word-wrap-by-category t) ;; improves CJK + Latin word-wrapping
  )

;; file head: # -*- truncate-lines: nil -*-

;; (use-package simple
;;   :ensure nil
;;   :bind
;;   (
;;    ;; toggle-word-wrap
;;    )
;;   )
;; 行 line:2 ends here

;; [[file:README.org::*列 column][列 column:1]]
(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (after-init . global-display-fill-column-indicator-mode)
  ;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
  )
;; 列 column:1 ends here

;; [[file:README.org::*kill buffer][kill buffer:1]]
;; kill buffer
;; {{{
(use-package emacs
  :ensure nil
  :bind
  ("C-c K" . my/kill-all-other-buffers)
  :config
  (setq confirm-kill-processes nil)
  (defun my/kill-all-other-buffers ()
    (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
    )
  )
;; }}}
;; kill buffer:1 ends here

;; [[file:README.org::*side-buffer][side-buffer:1]]
;; side buffer
;; {{{
(use-package emacs
  :ensure nil
  :bind
  ("C-c B" . my/side-buffer)
  :config
  (defun my/side-buffer ()
    (interactive)
    (let ((other (buffer-name (window-buffer (next-window)))))
      (delete-other-windows)
      (set-frame-width (selected-frame)
                       (+ (frame-width (selected-frame)) (window-width)))
      (split-window-horizontally)
      (split-window-vertically)
      (with-selected-window (next-window)
        (set-window-buffer (selected-window) other))
      (with-selected-window (previous-window)
        (set-window-buffer (selected-window) "*Scratch*")))
    )
  )
;; }}}
;; side-buffer:1 ends here

;; [[file:README.org::*ibuffer][ibuffer:1]]
;; ibuffer
;; {{{
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  ;; :custom
  ;; (ibuffer-formats
  ;;  '((mark modified read-only locked " "
  ;;          (name 35 35 :left :elide)
  ;;          " "
  ;;          (size 9 -1 :right)
  ;;          " "
  ;;          (mode 16 16 :left :elide)
  ;;          " " filename-and-process)
  ;;    (mark " "
  ;;          (name 16 -1)
  ;;          " " filename)))
  :config
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("emacs" (or
                           (mode . emacs-lisp-mode)
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           ))
                 ("org" (or (mode . org-mode)
                            (mode . org-agenda-mode)
                            (mode . org-src-mode)
                            ))
                 ;;               ("erc" (mode . erc-mode))

                 ("planner" (or
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
                             (mode . muse-mode)))
                 ("PDF"    (mode . pdf-view-mode))
                 ("python" (mode . python-mode))
                 ;; ("gnus" (or
                 ;;          (mode . message-mode)
                 ;;          (mode . bbdb-mode)
                 ;;          (mode . mail-mode)
                 ;;          (mode . gnus-group-mode)
                 ;;          (mode . gnus-summary-mode)
                 ;;          (mode . gnus-article-mode)
                 ;;          (name . "^\\.bbdb$")
                 ;;          (name . "^\\.newsrc-dribble")))
                 ))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  )
;; }}}
;; ibuffer:1 ends here

;; [[file:README.org::*文件 file][文件 file:1]]
(use-package emacs
  :ensure nil
  :bind ("C-c C-q" . my/sudo-edit)
  :init
  (setq default-directory "~/")
  (setq command-line-default-directory "~/")
  (setq find-file-visit-truename t)
  ;; warn when opening files bigger than 100 MB
  (setq large-file-warning-threshold (* 100 1000 1000))
  :config
  ;; https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
  (defun my/sudo-edit (&optional arg)
    "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file
         (concat "/sudo:root@localhost:" (ido-read-file-name "File(root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (defun my/sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user writeable, aborting sudo"))
    (find-file
     (if (file-remote-p file)
         (concat
          "/"
          (file-remote-p file 'method)
          ":"
          (file-remote-p file 'user)
          "@"
          (file-remote-p file 'host)
          "|sudo:root@"
          (file-remote-p file 'host)
          ":"
          (file-remote-p file 'localname))
       (concat "/sudo:root@localhost:" file)))))
;; 文件 file:1 ends here

;; [[file:README.org::*文件 file][文件 file:2]]
;; 使 Emacs 自动加载外部修改过的文件
(use-package autorevert
  :ensure nil
  :hook
  (on-first-file . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  ;; :config (global-auto-revert-mode 1)
  )

;; Open file system read-only files as read-only in Emacs as well.
(setq view-read-only t)
;; 文件 file:2 ends here

;; [[file:README.org::*chunk][chunk:1]]
;; chunk
;; {{{
;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000) ;; 64kb
;; }}}
;; chunk:1 ends here

;; [[file:README.org::*auto-save][auto-save:1]]
;; auto-save: 定期预存，防止停电、系统崩溃等原因造成的数据损失
;; {{{
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save file when buffer/focus change 自动保存
(add-hook 'after-init-hook 'auto-save-visited-mode)
(setq
 auto-save-default t ; auto-save every buffer that visits a file
 auto-save-timeout 20 ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
 )

(defun my/save-all-file-buffers ()
  "Saves every buffer associated with a file."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (buffer-modified-p))
        (save-buffer)))))
;; }}}
;; auto-save:1 ends here

;; [[file:README.org::*backup][backup:1]]
;; backup file: 备份
;; {{{
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;;
;; (defvar --backup-directory (concat user-emacs-directory "backups"))
;; (if (not (file-exists-p --backup-directory))
;;         (make-directory --backup-directory t))
;; (setq backup-directory-alist `(("." . ,--backup-directory)))
;; (setq backup-directory-alist `((".*" . ,(expand-file-name "backup" user-emacs-directory))))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq make-backup-files t         ; backup of a file the first time it is saved.
      backup-by-copying t         ; don't clobber symlinks
      version-control t           ; version numbers for backup files
      delete-old-versions t       ; delete excess backup files silently
      delete-by-moving-to-trash t
      dired-kept-versions 2
      kept-old-versions 6 ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9 ; newest versions to keep when a new numbered backup is made (default: 2)
      )
;; }}}
;; backup:1 ends here

;; [[file:README.org::*lockfile][lockfile:1]]
;; lockfile: 不同进程修改同一文件
;; {{{
(setq create-lockfiles t)
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
;; }}}
;; lockfile:1 ends here

;; [[file:README.org::*find file/grep file][find file/grep file:2]]
(use-package ffap
  :ensure nil
  :defer t
  :bind
  (
   ([remap find-file] . my/find-file-at-point)
   ([remap ffap]      . my/find-file-at-point)
   )
  :config
  ;; https://emacstalk.codeberg.page/post/021/
  (defun my/find-file-at-point ()
    "Enhanced version of `find-file-at-point'.
First attempt to open file specified by `symbol-at-point', and fallback to normal one."
    (interactive)
    (condition-case nil
        (thread-last (thing-at-point 'symbol t)
                     (intern)
                     (symbol-value)
                     (find-file-noselect)
                     (switch-to-buffer))
      (t (call-interactively 'find-file-at-point))))
  )
;; find file/grep file:2 ends here

;; [[file:README.org::*file name and file extension][file name and file extension:1]]
;; file name and file extension
;; {{{
;;
;; https://github.com/chyla/kill-file-path
;;
;; 如何在文件夹层次结构中找到所有不同的文件扩展名？
;; https://qa.1r1g.com/sf/ask/128957811/#
;;

;; file name only
(defun my/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (kill-new filename))
    (message filename)))

;; file name with file path
(defun my/copy-file-name-full ()
  "Copy the current buffer file name (with full path) to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
;; }}}
;; file name and file extension:1 ends here

;; [[file:README.org::*file path][file path:1]]
;; file path
;; {{{
(defun my/copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the current or marked files.

If a buffer is not file and not dired, copy value of `default-directory'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version 2018-06-18 2021-09-30"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result
                      (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))
;; }}}
;; file path:1 ends here

;; [[file:README.org::*rename file][rename file:1]]
;; rename file
;; {{{
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-simple.el

;;;###autoload
(defun my/rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t))
  )
;; }}}
;; rename file:1 ends here

;; [[file:README.org::*delete file][delete file:1]]
;; delete buffer file
;; {{{
(defun my/delete-current-file ()
  "Delete the file associated with the current buffer.
Delete the current buffer too.
If no file is associated, just close buffer without prompt for save."
  (interactive)
  (let ((currentFile (buffer-file-name)))
    (when (yes-or-no-p (concat "Delete file?: " currentFile))
      (kill-buffer (current-buffer))
      (when currentFile
        (delete-file currentFile)))))
;; }}}
;; delete file:1 ends here

;; [[file:README.org::*垃圾筒][垃圾筒:1]]
;; move file to trash when delete
;; {{{
;;; macOS
(when (eq system-type 'darwin)
  (setq trash-directory "~/.Trash/")
  (setq delete-by-moving-to-trash t))
;; }}}
;; 垃圾筒:1 ends here

;; [[file:README.org::*文件夹][文件夹:1]]
(use-package emacs
  :ensure nil
  :init
  ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
  (defun my/auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))

  (add-to-list 'find-file-not-found-functions #'my/auto-create-missing-dirs)
  )
;; 文件夹:1 ends here

;; [[file:README.org::*dired & wdired][dired & wdired:1]]
(use-package dired
  :ensure nil
  ;; :demand t
  ;; :defer 1.5
  :hook
  ((dired-mode-hook . my/dired-setup-view)
   (dired-mode-hook . my/dired-disable-line-wrapping)
   (dired-after-readin-hook . my/dired-postprocess-ls-output))
  :bind
  (:map
   dired-mode-map ("RET" . my/dired-open-dwim) ("<tab>" . my/dired-switch-view))
  :config
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq
   dired-recursive-deletes 'always
   dired-recursive-copies 'always) ; 全部递归拷贝、删除文件夹中的文件
  (setq dired-use-ls-dired t)
  (setq dired-auto-revert-buffer t)
  ;; (dired-listing-switches "-alGh")
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  (setq
   dired-listing-switches "-alh --time-style=long-iso"
   ;; "-l --almost-all --human-readable --group-directories-first --no-group"
   )

  (defun my/dired-open-dwim ()
    (interactive)
    (if (file-directory-p (dired-file-name-at-point))
        (dired-find-file)
      (dired-find-file-other-window)))

  ;; https://www.n16f.net/blog/decluttering-dired-for-peace-of-mind/
  (setq my/dired-minimal-view t)

  (defun my/dired-setup-view ()
    (dired-hide-details-mode
     (if my/dired-minimal-view
         1
       -1)))

  (defun my/dired-switch-view ()
    (interactive)
    (setq my/dired-minimal-view (not my/dired-minimal-view))
    (my/dired-setup-view))

  (defun my/dired-postprocess-ls-output ()
    "Postprocess the list of files printed by the ls program when
executed by Dired."
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        ;; Go to the beginning of the next line representing a file
        (while (null (dired-get-filename nil t))
          (dired-next-line 1))
        (beginning-of-line)
        ;; Narrow to the line and process it
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (save-restriction
            (narrow-to-region start end)
            (setq inhibit-read-only t)
            (unwind-protect
                (my/dired-postprocess-ls-line)
              (setq inhibit-read-only nil))))
        ;; Next line
        (dired-next-line 1))))

  (defun my/dired-disable-line-wrapping ()
    (setq truncate-lines t))

  (defun my/dired-postprocess-ls-line ()
    "Postprocess a single line in the ls output, i.e. the information
about a single file. This function is called with the buffer
narrowed to the line."
    ;; Highlight everything but the filename
    (when (re-search-forward directory-listing-before-filename-regexp nil t 1)
      (add-text-properties (point-min) (match-end 0) '(font-lock-face shadow)))
    ;; Hide the link count
    (beginning-of-line)
    (when (re-search-forward " +[0-9]+" nil t 1)
      (add-text-properties (match-beginning 0) (match-end 0) '(invisible t))))
  )
;; dired & wdired:1 ends here

;; [[file:README.org::*project][project:1]]
;; project
;; {{{
(use-package project
  :ensure nil
  :defer 2
  :bind
  (
   ("C-x s" . my/eshell)
   ;; ("C-c p" . project-prefix-map)
   )
  :config
  ;; (setq project-switch-commands 'project-dired)
  (defun my/eshell ()
    "Start eshell at the root of the current project, or in the
current directory if the current buffer is not part of a
project."
    (interactive)
    (if (project-current)
        (project-eshell)
      (eshell)))

  ;; https://emacstalk.codeberg.page/post/010/
  ;; (defun my/project-try-local (dir)
  ;;   "Determine if DIR is a non-Git project."
  ;;   (catch 'ret
  ;;     (let ((pr-flags '((".project")
  ;;                       ("go.mod"
  ;;                        "Cargo.toml"
  ;;                        "project.clj"
  ;;                        "pom.xml"
  ;;                        "package.json")
  ;;                       ("Makefile"
  ;;                        "README.org"
  ;;                        "README.md"))))
  ;;       (dolist (current-level pr-flags)
  ;;         (dolist (f current-level)
  ;;           (when-let ((root (locate-dominating-file dir f)))
  ;;             (throw 'ret (cons 'local root))))))))

  ;; (setq project-find-functions
  ;;       '(my/project-try-local project-try-vc))
  )
;; }}}
;; project:1 ends here

;; [[file:README.org::*mibuffer][mibuffer:1]]
(use-package minibuffer
  :ensure nil
  :defer t
  :bind
  (
   ;; ("TAB" . minibuffer-complete)
   ("M-SPC" . execute-extended-command) ; replace part of [cycle-spacing] keymap
   :map minibuffer-mode-map ; alias of minibuffer-local-map
   ("H-j" . next-line-or-history-element)
   ("H-k" . previous-line-or-history-element)
   ;;
   ("C-n" . next-line-or-history-element)
   ("C-p" . previous-line-or-history-element)
   ;;
   :map completion-in-region-mode-map
   ("C-n" . minibuffer-previous-completion)
   ("C-p" . minibuffer-next-completion)
   )
  :init
  (setq history-delete-duplicates t)
  ;; :config
  ;; (setq completions-detailed t)
  )

;; https://emacs-china.org/t/emacs-28-1-fido-vertical-mode/20474/5
;; mibuffer:1 ends here

;; [[file:README.org::*mibuffer][mibuffer:2]]
;; completion window
(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)))
;; case: ignore case
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t    ;; default nil
      read-file-name-completion-ignore-case t ;; default t
      )
;; completion style, fido-mode override completion-styles
(setq completion-styles '(basic substring initials partial-completion flex))
(setq completion-cycle-threshold 15)
(setq completions-format 'vertical) ; one-column
;; (setq completions-header-format nil)
(setq completions-max-height 18)
(setq completion-auto-help 'always)
(setq completion-auto-select 'second-tab)
(setq enable-recursive-minibuffers t)
;; }}}
;; mibuffer:2 ends here

;; [[file:README.org::*mibuffer][mibuffer:3]]
(defun my/sort-by-alpha-length (elems)
  "Sort ELEMS first alphabetically, then by length."
  (sort elems (lambda (c1 c2)
                (or (string-version-lessp c1 c2)
                    (< (length c1) (length c2))))))

(defun my/sort-by-history (elems)
  "Sort ELEMS by minibuffer history.
Use `mct-sort-sort-by-alpha-length' if no history is available."
  (if-let ((hist (and (not (eq minibuffer-history-variable t))
                      (symbol-value minibuffer-history-variable))))
      (minibuffer--sort-by-position hist elems)
    (my/sort-by-alpha-length elems)))

(defun my/completion-category ()
  "Return completion category."
  (when-let ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
                             (max (minibuffer-prompt-end) (point)))
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
       'category))))

(defun my/sort-multi-category (elems)
  "Sort ELEMS per completion category."
  (pcase (my/completion-category)
    ('nil elems) ; no sorting
    ('kill-ring elems)
    ('project-file (my/sort-by-alpha-length elems))
    (_ (my/sort-by-history elems))))

(setq completions-sort #'my/sort-multi-category)
;; mibuffer:3 ends here

;; [[file:README.org::*ido & fido][ido & fido:1]]
(use-package ido
  :ensure nil
  :defer 1
  :bind ("C-c p" . ido-switch-buffer)
  :config
  (setq ido-vertical-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-decorations
        ;; order matters
        '(
          "{"                 ; "\n=> "
          "}"                 ; ""
          " | "               ; "\n"
          " | ..."            ; ""
          "["
          "]"
          " [No match]"
          " [Matched]"
          " [Not readable]"
          " [Too big]"
          " [Confirm]"
          ))
  (setq ido-ignore-buffers
        '("\\` "
          "^ "
          "*Completions*"
          "*Shell Command Output*"
          "*Messages*"
          "Async Shell Command"
          ))
  )
;; ido & fido:1 ends here

;; [[file:README.org::*括号自动补全][括号自动补全:1]]
;; pair completion
(use-package electric-pair-mode ; elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  )
;; 括号自动补全:1 ends here

;; [[file:README.org::*abbrev/dabbrev Completion][abbrev/dabbrev Completion:1]]
(use-package abbrev
  :ensure nil
  :defer 1
  :config
  (setq abbrev-suggest t)
  )
;; abbrev/dabbrev Completion:1 ends here

;; [[file:README.org::*abbrev/dabbrev Completion][abbrev/dabbrev Completion:2]]
;; abbrev/dabbrev: dynamic abbreviation expand
;; {{{
(use-package dabbrev
  :ensure nil
  :bind
  (
   ("C-<tab>" . dabbrev-expand)
   ("H-<tab>" . dabbrev-expand)
   )
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  ;; :config
  )
;; }}}
;; abbrev/dabbrev Completion:2 ends here

;; [[file:README.org::*hippie-expand][hippie-expand:1]]
;; hippie-expand
;; {{{
(use-package hippie-exp
  :ensure nil
  :bind
  ([remap dabbrev-expand] . hippie-expand​​)
  ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          ))
  )
;; }}}
;; hippie-expand:1 ends here

;; [[file:README.org::*tree-sitter: treesit][tree-sitter: treesit:1]]
;; tree-sitter
;; {{{
;; Use the built-in treesit and load all language grammars
(use-package treesit
  :ensure nil
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :defer 1
  :hook
  (
   ;; (tree-sitter-after-on . tree-sitter-hl-mode)
   (emacs-lisp-mode . (lambda () (treesit-parser-create 'elisp))))
  :init
  ;; Load languages directly from the repository after making them
  ;; (setq treesit-extra-load-path
  ;;       (expand-file-name "tree-sitter" user-emacs-directory))
  :custom
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (csharp-mode . csharp-ts-mode)
     (conf-toml-mode . toml-ts-mode)
     (css-mode . css-ts-mode)
     ;; (emacs-lisp-mode . elisp-ts-mode)
     (java-mode . java-ts-mode)
     (js-mode . js-ts-mode)
     (js-json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (ruby-mode . ruby-ts-mode)
     (sh-mode . bash-ts-mode)))
  :config

  ;; (add-hook 'emacs-lisp-mode-hook
  ;;   #'(lambda () (treesit-parser-create 'elisp)))

  ;; (add-to-list 'auto-mode-alist '("\\.el\\'" . elisp-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (add-to-list
   'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)))
;; }}}
;; tree-sitter: treesit:1 ends here

;; [[file:README.org::*基本编辑][基本编辑:1]]
;; https://github.com/Eason0210/.emacs.d/
(use-package emacs
  :ensure nil
  :hook ((prog-mode text-mode) . indicate-buffer-boundaries-left)
  :custom
  ;; (mouse-yank-at-point t)
  (scroll-preserve-screen-position 'always)
  (truncate-partial-width-windows nil)
  (tooltip-delay 1.5)
  ;; (use-short-answers t)
  (frame-resize-pixelwise t)
  ;; :custom-face
  ;; (fixed-pitch ((t (:family ,(face-attribute 'default :family) :height 1.0))))
  ;; (variable-pitch ((t (:family "Bookerly" :height 1.0))))
  ;; (mode-line ((t (:inherit variable-pitch :height 1.0))))
  ;; (mode-line-inactive ((t (:inherit variable-pitch :height 1.0))))
  :config
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  )
;; 基本编辑:1 ends here

;; [[file:README.org::*基本编辑][基本编辑:2]]
(use-package simple
  :ensure nil
  :hook (emacs-lisp-mode . turn-off-auto-fill)
  :bind
  (
   ("M-j" . join-line) ; M-^ is inconvenient
   ("C-x k" . kill-current-buffer)
   ("C-x x p" . pop-to-mark-command)
   ("C-x C-." . pop-global-mark)
   )
  ;; :custom
  ;; (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (setq set-mark-command-repeat-pop t)
  (setq save-interprogram-paste-before-kill t)
  (setq indent-tabs-mode nil)
  (setq column-number-mode t)
  ;; (setq tab-always-indent 'complete) ; free the M-TAB keybinding
  ;; electric-indent-mode
  )
;; 基本编辑:2 ends here

;; [[file:README.org::*unicode][unicode:1]]
;; unicode
;; {{{
;; https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
(when (file-exists-p
       (expand-file-name "assets/unicode/UnicodeData.txt"
                         user-emacs-directory))
  (setq describe-char-unicodedata-file
        (expand-file-name "assets/unicode/UnicodeData.txt"
                          user-emacs-directory)))
;; }}}
;; unicode:1 ends here

;; [[file:README.org::*空白字符/不可见字符][空白字符/不可见字符:1]]
;; 不可见字符: zero-width characters ->​<-
;; {{{
;; http://xahlee.info/emacs/emacs/elisp_unicode_replace_invisible_chars.html
(defun xah-replace-invisible-char ()
  "Query replace some invisible Unicode chars.
The chars replaced are:
 LEFT-TO-RIGHT MARK           (8206, #x200e)
 OBJECT REPLACEMENT CHARACTER (65532, #xfffc)
 RIGHT-TO-LEFT MARK           (8207, #x200f)
 RIGHT-TO-LEFT OVERRIDE       (8238, #x202e)
 ZERO WIDTH NO-BREAK SPACE    (65279, #xfeff)
 ZERO WIDTH SPACE             (codepoint 8203, #x200b)

Search begins at buffer beginning. (respects `narrow-to-region')

URL `http://xahlee.info/emacs/emacs/elisp_unicode_replace_invisible_chars.html'
Version: 2018-09-07 2022-09-13"
  (interactive)
  (let ((case-replace nil)
        (case-fold-search nil)
        ($p0 (point)))
    (goto-char (point-min))
    (while (re-search-forward
            "\ufeff\\|\u200b\\|\u200f\\|\u202e\\|\u200e\\|\ufffc"
            nil t)
      (replace-match ""))
    (goto-char $p0))
  )
;; }}}
;; 空白字符/不可见字符:1 ends here

;; [[file:README.org::*空白字符/不可见字符][空白字符/不可见字符:2]]
;; see invisible chars
;; {{{
;; https://emacs-china.org/t/topic/19557
(defun my/see-invisible-chars ()
  "Highlight ZERO WIDTH chars in all buffers."
  (interactive)
  (let ((charnames (list
                    "BYTE ORDER MARK"
                    "LEFT-TO-RIGHT EMBEDDING"
                    "LEFT-TO-RIGHT MARK"
                    "OBJECT REPLACEMENT CHARACTER"
                    "RIGHT-TO-LEFT MARK"
                    "RIGHT-TO-LEFT OVERRIDE"
                    "ZERO WIDTH JOINER"
                    "ZERO WIDTH NO-BREAK SPACE"
                    "ZERO WIDTH NON-JOINER"
                    "ZERO WIDTH SPACE"
                    )))
    (set-face-background 'glyphless-char "RoyalBlue1")
    (dolist (name charnames)
      ;; see info node "info:elisp#Glyphless Chars" for available values
      (set-char-table-range glyphless-char-display
                            (char-from-name name) "fuck"))
    )
  )
;; }}}
;; 空白字符/不可见字符:2 ends here

;; [[file:README.org::*空白字符/不可见字符][空白字符/不可见字符:3]]
(use-package emacs
  :ensure nil
  :bind
  ("H-SPC H-SPC" . my/insert-zero-width-space-200b)
  :config
  (defun my/insert-zero-width-space-200b ()
    (insert "\u200b")
    (interactive))
  )
;; 空白字符/不可见字符:3 ends here

;; [[file:README.org::*单词 Word][单词 Word:1]]
(use-package subword
  ;; camelCase and superword-mode
  :ensure nil
  :defer 1
  :config
  (global-subword-mode)
  )
;; 单词 Word:1 ends here

;; [[file:README.org::*句子、段落 sentence paragraph][句子、段落 sentence paragraph:1]]
(use-package emacs
 :ensure nil
 :bind (([remap fill-paragraph] . my/toggle-fill-unfill))
 :init
 ;; (setq sentence-end-double-space nil)
 (setq-default fill-column 80) ; M-x set-fill-column RET
 :config
 ;; sentence: 断句
 (setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"))
;; paragraph: 段落
(defun my/toggle-fill-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'my-fill-or-unfill)
             (progn
               (setq this-command nil)
               (point-max))
           fill-column)))
    (call-interactively 'fill-paragraph nil (vector nil t))))

;; https://www.emacswiki.org/emacs/UnfillParagraph
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
;; 句子、段落 sentence paragraph:1 ends here

;; [[file:README.org::*Title Capitalization][Title Capitalization:1]]
;; additionally to the list defined in title-capitalization:
(defvar my/do-not-capitalize-words
  '("suliveevil")
  "Personal list of words that doesn't get capitalized in titles.")

(defun my/text-case-title-capitalization (beg end)
  "Proper English title capitalization of a marked region"
  ;; - before: the presentation of this heading of my own from my keyboard and yet
  ;; - after:  The Presentation of This Heading of My Own from My Keyboard and Yet
  ;; - before: a a a a a a a a
  ;; - after:  A a a a a a a A
  (interactive "r")
  (save-excursion
    (let* (
           ;; basic list of words which don't get capitalized according to simplified rules:
           ;; http://karl-voit.at/2015/05/25/elisp-title-capitalization/
           (do-not-capitalize-basic-words '(
                                            "a"
                                            "ago"
                                            "an"
                                            "and"
                                            "as"
                                            "at"
                                            "but"
                                            "by"
                                            "es"
                                            "for"
                                            "from"
                                            "in"
                                            "into"
                                            "it"
                                            "n"
                                            "next"
                                            "nor"
                                            "of"
                                            "off"
                                            "on"
                                            "onto"
                                            "or"
                                            "over"
                                            "past"
                                            "s"
                                            "so"
                                            "t"
                                            "the"
                                            "till"
                                            "to"
                                            "up"
                                            "yet"))
           ;; if user has defined 'my/do-not-capitalize-words, append to basic list:
           (do-not-capitalize-words (if (boundp 'my/do-not-capitalize-words)
                                        (append do-not-capitalize-basic-words my/do-not-capitalize-words )
                                      do-not-capitalize-basic-words
                                      )
                                    )
           )
      ;; go to begin of first word:
      (goto-char beg)
      (capitalize-word 1)
      ;; go through the region, word by word:
      (while (< (point) end)
        (skip-syntax-forward "^w" end)
        (let ((word (thing-at-point 'word)))
          (if (stringp word)
              ;; capitalize current word except it is list member:
              (if (member (downcase word) do-not-capitalize-words)
                  (downcase-word 1)
                (capitalize-word 1)))))
      ;; capitalize last word in any case:
      (backward-word 1)
      (if (and (>= (point) beg)
               (not (member (or (thing-at-point 'word) "s")
                            '("n" "t" "es" "s"))))
          (capitalize-word 1))))
  )
;; Title Capitalization:1 ends here

;; [[file:README.org::*upcase word][upcase word:1]]
;; https://emacstalk.codeberg.page/post/023/
;;;###autoload
(defun my/upcase-backwards ()
  "Upcase word in reverse direction, back until the first space char or beginning-of-line"
  (interactive)
  (save-excursion
    ;; move to first non-space char
    (skip-syntax-backward " " (line-beginning-position))
    (push-mark)
    (let ((beginning (or (re-search-backward "[[:space:]]" (line-beginning-position) t)
                         (line-beginning-position)))
          (end (mark)))
      (unless (= beginning end)
        (upcase-region beginning end)))))

;; (keymap-global-set "M-o" #'my/upcase-backwards)
;; upcase word:1 ends here

;; [[file:README.org::*跳转][跳转:1]]
;; goto-char by Oliver Scholz
;; {{{
(use-package emacs
  :ensure nil
  :bind
  ([remap goto-char] . my/goto-char)
  :config
  (defun my/goto-char (n char)
    "Move forward to Nth occurence of CHAR.
Typing `my/goto-char-key' again will move forwad to the next Nth
occurence of CHAR."
    (interactive "p\ncGo to char: ")
    (search-forward (string char) nil nil n)
    (while (char-equal (read-char)
                       char)
      (search-forward (string char) nil nil n))
    (setq unread-command-events (list last-input-event)))
  )

;; similar work
;; https://www.emacswiki.org/emacs/go-to-char.el
;; https://www.emacswiki.org/emacs/joseph-go-to-char
;; doitian/iy-go-to-char: Go to next CHAR which is similar to "f" and "t" in vim
;; https://github.com/doitian/iy-go-to-char
;; }}}
;; 跳转:1 ends here

;; [[file:README.org::*跳转][跳转:2]]
(defun my/occur-mode-hook-fn ()
  "HELP customizations."
  (interactive)
  (turn-on-stripe-buffer-mode)
  (occur-rename-buffer))

(add-hook 'occur-mode-hook #'my/occur-mode-hook-fn)

(define-key occur-mode-map (kbd "n") #'next-logical-line)
(define-key occur-mode-map (kbd "p") #'previous-logical-line)

(defun my/recenter-line-near-top-fn ()
  "Move current line near top"
  (interactive)
  (let ((recenter-positions '(5)))
    (recenter-top-bottom)))

(add-hook 'occur-mode-find-occurrence-hook #'my/recenter-line-near-top-fn)
;; 跳转:2 ends here

;; [[file:README.org::*isearch][isearch:1]]
;; isearch
;; {{{
;; M-<: first match
;; M->: last  match
(use-package isearch
  :ensure nil
  :defer t
  ;; :bind
  ;; (
  ;; :map isearch-mode-map
  ;; ("C-c" . isearch-cancel)
  ;; ("DEL" . isearch-del-char)
  ;; ("s-v" . isearch-yank-kill)
  ;; :map minibuffer-local-isearch-map
  ;; )
  :config
  (setq isearch-lazy-count t) ; anzu
  (setq isearch-allow-motion t)
  ;; 这样可以在 literal 的 isearch 中，把空格直接当成正则里面的 .* 匹配
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace t)
  (setq search-whitespace-regexp ".*")
  (setq isearch-regexp-lax-whitespace nil) ; 正则搜索时不开启这个功能，空格就是空格
  (setq isearch-motion-changes-direction t)
  ;;
  ;; 自动 wrap
  (defadvice isearch-search (after isearch-no-fail activate)
    (unless isearch-success
      (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
      (ad-activate 'isearch-search)
      (isearch-repeat (if isearch-forward 'forward))
      (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
      (ad-activate 'isearch-search)))
  ;;
  ;; 重新输入并搜索
  (defmacro isearch-quit-and-run (&rest body)
    "Quit the minibuffer and run BODY afterwards."
    (declare (indent 0))
    `(progn
       (put 'quit 'error-message "")
       (run-at-time nil nil
                    (lambda ()
                      (put 'quit 'error-message "Quit")
                      (with-demoted-errors "Error: %S"
                        ,@body)))
       (isearch-cancel)))

  (defun my/rerun-isearch ()
    "rerun isearch from the original place."
    (interactive)
    (isearch-quit-and-run
      (isearch-forward)))
  )
;; }}}
;; isearch:1 ends here

;; [[file:README.org::*对齐缩进格式化 align indent format][对齐缩进格式化 align indent format:3]]
(use-package simple
  :ensure nil
  :defer t
  :bind ("C-c H-i" . my/indent-buffer)
  :config
  ;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/extensions/lazycat/basic-toolkit.el
  (defun my/refresh-file ()
    "Automatic reload current file."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (my/indent-buffer)
      (my/indent-comment-buffer)
      (save-buffer)
      (load-file (buffer-file-name)))
     ((member major-mode '(lisp-mode c-mode perl-mode))
      (my/indent-buffer)
      (my/indent-comment-buffer)
      (save-buffer))
     ((member major-mode '(haskell-mode sh-mode))
      (my/indent-comment-buffer)
      (save-buffer))
     ((derived-mode-p 'scss-mode)
      (require 'css-sort)
      (css-sort))
     (t
      (message "Current mode is not supported, so didn't reload"))))

  (defun my/indent-buffer ()
    "Automatic format current buffer."
    (interactive)
    (if (derived-mode-p 'python-mode)
        (message "Don't indent python buffer, it will mess up the code syntax.")
      (save-excursion
        (indent-region (point-min) (point-max) nil)
        (delete-trailing-whitespace)
        (untabify (point-min) (point-max)))))

  (defun my/indent-comment-buffer ()
    "Indent comment of buffer."
    (interactive)
    (my/indent-comment-region (point-min) (point-max)))

  (defun my/indent-comment-region (start end)
    "Indent region."
    (interactive "r")
    (save-excursion
      (setq end (copy-marker end))
      (goto-char start)
      (while (< (point) end)
        (if (comment-search-forward end t)
            (comment-indent)
          (goto-char end)))))
  )
;; 对齐缩进格式化 align indent format:3 ends here

;; [[file:README.org::*文本对比与合并 Diff & Merge][文本对比与合并 Diff & Merge:2]]
(use-package diff-mode
  :ensure nil
  :defer t
  )
;; 文本对比与合并 Diff & Merge:2 ends here

;; [[file:README.org::*大纲与折叠 outline & fold][大纲与折叠 outline & fold:2]]
;; fold
;; {{{
(use-package hideshow
  :ensure nil
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("C-c TAB" . hs-toggle-hiding)
  ("M-+" . hs-show-all)
  :config
  (add-to-list 'hs-special-modes-alist
               '(emacs-lisp-mode "{" "}" ";;" nil nil))
  )
;; }}}
;; 大纲与折叠 outline & fold:2 ends here

;; [[file:README.org::*imenu][imenu:1]]
(use-package imenu
  :ensure nil
  :hook (font-lock-mode . my/try-to-add-imenu)
  :config
  (defun my/try-to-add-imenu ()
    (interactive)
    (condition-case nil
        (imenu-add-to-menubar "Imenu")
      (error nil)))
  )

;; (setq sql-imenu-generic-expression
;;       '(("Comments" "^-- \\(.+\\)" 1)
;;         ("Function DeFinitions"
;;          "^\\s-*\\(function\\|procedure\\)[ \n\t]+\\([a-z0-9_]+\\)\
;;  [ \n\t]*([a-z0-9 _,\n\t]*)[ \n\t]*\\(return[ \n\t]+[a-z0-9_]+[ \n\t]+\\)?[ai]s\\b"
;;          2)
;;         ("Function Prototypes"
;;          "^\\s-*\\(function\\|procedure\\)[ \n\t]+\\([a-z0-9_]+\\)\
;;  [ \n\t]*([a-z0-9 _,\n\t]*)[ \n\t]*\\(return[ \n\t]+[a-z0-9_]+[ \n\t]*\\)?;"
;;          2)
;;         ("Indexes" "^\\s-*create\\s-+index\\s-+\\(\\w+\\)" 1)
;;         ("Tables" "^\\s-*create\\s-+table\\s-+\\(\\w+\\)" 1)))

;; (add-hook
;;  'sql-mode-hook
;;  (lambda () (setq imenu-generic-expression sql-imenu-generic-expression)))
;; imenu:1 ends here

;; [[file:README.org::*narrow][narrow:1]]
(use-package emacs
 :ensure nil
 :bind ("C-c n n" . my/narrow-or-widen-dwim)
 :config
 ;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
 (defun my/narrow-or-widen-dwim (p)
   "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.
With prefix P, don't widen, just narrow even if buffer is already
narrowed."
   (interactive "P")
   (declare (interactive-only))
   (cond
    ((and (buffer-narrowed-p) (not p))
     (widen))
    ((region-active-p)
     (narrow-to-region (region-beginning) (region-end)))
    ((derived-mode-p 'org-mode)
     ;; `org-edit-src-code' is not a real narrowing command.
     ;; Remove this first conditional if you don't want it.
     (cond
      ((ignore-errors
         (org-edit-src-code))
       (delete-other-windows))
      ((org-at-block-p)
       (org-narrow-to-block))
      (t
       (org-narrow-to-subtree))))
    (t
     (narrow-to-defun))))
 )
;; narrow:1 ends here

;; [[file:README.org::*Eshell][Eshell:1]]
;; https://www.n16f.net/blog/eshell-key-bindings-and-completion/
(use-package eshell
  :ensure nil
  :bind
  (
   ("C-x s" . eshell)
   ;; :map eshell-mode-map
   ;; (
   ;;("C-l" . eshell-clear)
   ;; ("C-r" . eshell-history)
   ;; ("<tab>" . company-complete)
   ;; )
   )
  :config
  (require 'esh-mode) ; eshell-mode-map
  )
;; Eshell:1 ends here

;; [[file:README.org::*python][python:1]]
(use-package python
  :ensure nil
  :mode
  ("\\.py\\'" . python-mode)
  ("\\.wsgi$" . python-mode)
  :interpreter
  ("python3" . python-mode)
  :config
  (setq python-indent-offset 4)
  )
;; python:1 ends here

;; [[file:README.org::*frame][frame:1]]
;; frame
;; {{{
(setq frame-size-history t)

;; (setq frame-title-format
;;       '(buffer-file-name
;;         (:eval (abbreviate-file-name buffer-file-name))
;;         (dired-directory dired-directory "%b")))

;; https://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))
;; }}}
;; frame:1 ends here

;; [[file:README.org::*window/layout][window/layout:2]]
(use-package windmove
  :ensure nil
  :bind
  (
   ("C-c <left>" . windmove-left)
   ("C-c <right>" . windmove-right)
   ("C-c <up>" . windmove-up)
   ("C-c <down>" . windmove-down)
   )
  )
;; window/layout:2 ends here

;; [[file:README.org::*toggle fullscreen][toggle fullscreen:1]]
(defun my/toggle-fullscreen ()
  (interactive)
  (set-frame-parameter
   nil
   'fullscreen
   (if (frame-parameter nil 'fullscreen)
       nil
     'fullboth))
  )
;; toggle fullscreen:1 ends here

;; [[file:README.org::*toggle one window][toggle one window:1]]
;; window
;; {{{
(use-package emacs
  :ensure nil
  :bind
  (
   ("H-w H-t" . my/toggle-one-window)
   ("C-c C-w" . my/toggle-one-window)
   )
  :config
  ;; toggle one window
  ;; https://github.com/manateelazycat/toggle-one-window
  (defvar toggle-one-window-window-configuration nil
    "The window configuration use for `toggle-one-window'.")

  (defun my/toggle-one-window ()
    "Toggle between window layout and one window."
    (interactive)
    (if (equal (length (cl-remove-if #'window-dedicated-p (window-list))) 1)
        (if toggle-one-window-window-configuration
            (progn
              (set-window-configuration toggle-one-window-window-configuration)
              (setq toggle-one-window-window-configuration nil))
          (message "No other windows exist."))
      (setq toggle-one-window-window-configuration
            (current-window-configuration))
      (delete-other-windows)))
  )
;; }}}
;; toggle one window:1 ends here

;; [[file:README.org::*toggle vertical horizontal split][toggle vertical horizontal split:1]]
(use-package emacs
  :ensure nil
  :bind
  ("H-w H-w" . my/toggle-vertical-horizontal-split)
  :config
  (defun my/toggle-vertical-horizontal-split ()
    "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
    (interactive)
    (require 'windmove)
    (let ((done))
      (dolist (dirs '((right . down) (down . right)))
        (unless done
          (let* ((win (selected-window))
                 (nextdir (car dirs))
                 (neighbour-dir (cdr dirs))
                 (next-win (windmove-find-other-window nextdir win))
                 (neighbour1 (windmove-find-other-window neighbour-dir win))
                 (neighbour2
                  (if next-win
                      (with-selected-window next-win
                        (windmove-find-other-window neighbour-dir next-win)))))
            ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
            (setq done
                  (and (eq neighbour1 neighbour2)
                       (not (eq (minibuffer-window) next-win))))
            (if done
                (let* ((other-buf (window-buffer next-win)))
                  (delete-window next-win)
                  (if (eq nextdir 'right)
                      (split-window-vertically)
                    (split-window-horizontally))
                  (set-window-buffer
                   (windmove-find-other-window neighbour-dir) other-buf)))))))))
;; toggle vertical horizontal split:1 ends here

;; [[file:README.org::*字体 face/font][字体 face/font:1]]
;; font and syntax
;; {{{
(set-face-attribute 'default nil
                    :family "Sarasa Mono SC Nerd"
                    :height 140 ; 更改显示字体大小
                    )
(global-font-lock-mode t) ;; turn on syntax highlighting for all buffers
;; }}}
;; 字体 face/font:1 ends here

;; [[file:README.org::*pretty-symbols][pretty-symbols:1]]
;; pretty-symbols
;; {{{
(setq-default prettify-symbols-alist
              '(
                ("lambda" . ?λ)
                ("function" . ?𝑓)
                )
              )
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
;; }}}
;; pretty-symbols:1 ends here

;; [[file:README.org::*mode-line][mode-line:1]]
;; mode-line
;; {{{
(use-package battery
  :ensure nil
  :defer 2
  :config
  (setq column-number-mode t) ;; 在 mode line 数字形式显示光标所在列
  (display-battery-mode +1)
  )
;; }}}
;; mode-line:1 ends here

;; [[file:README.org::*数据库][数据库:2]]
(defun pgformatter-on-region ()
  "A function to invoke pgFormatter as an external program."
  (interactive)
  (let ((b
         (if mark-active
             (min (point) (mark))
           (point-min)))
        (e
         (if mark-active
             (max (point) (mark))
           (point-max)))
        (pgfrm "/opt/homebrew/bin/pg_format"))
    (shell-command-on-region b e pgfrm (current-buffer) 1)))
;; 数据库:2 ends here

;; [[file:README.org::*eww][eww:1]]
(use-package eww
  :ensure nil
  :defer t
  :hook (eww-after-render . my/eww-render-hook)
  :bind ("C-c d B" . my/eww-browse-bing-dict)
  :config
  ;; (setq eww-retrieve-command '("/opt/homebrew/bin/readable"))

  ;; https://emacstalk.codeberg.page/post/018/
  (setq my/url-redirect-list
        `(("^https://github.com/\\(.+\\)/commit/\\(\\w+\\)$" .
           ;; 针对单个 commit
           (lambda (url)
             (format "https://github.com/%s/commit/%s.patch"
                     (match-string 1 url)
                     (match-string 2 url))))
          ("^https://github.com/\\(.+\\)/pull/\\([[:digit:]]+\\)$" .
           ;; 针对单个 Pull Request
           (lambda (url)
             (format "https://github.com/%s/pull/%s.patch"
                     (match-string 1 url)
                     (match-string 2 url))))
          ("^https://github.com/\\(.+\\)/blob/\\(.+\\)" .
           ;; 针对单个文件
           (lambda (url)
             (format "https://github.com/%s/raw/%s"
                     (match-string 1 url)
                     (match-string 2 url))))))

  (defun my/url-redirect (fn url &rest args)
    (catch 'ret
      (dolist (redirect-rule my/url-redirect-list)
        (let* ((regexp (car redirect-rule))
               (redirect-fn (cdr redirect-rule))
               (inhibit-message t))
          (when-let* ((matched-groups (string-match regexp url)))
            (setq url (funcall redirect-fn url))
            (message "Redirect URL to %s" url)
            (throw 'ret url)))))
    (apply fn url args))

  (advice-add 'eww :around 'my/url-redirect)

  (defun my/eww-render-hook ()
    (let ((url (plist-get eww-data :url)))
      (cond
       ((string-suffix-p ".patch" url)
        (diff-mode))
       ((string-suffix-p ".el" url)
        (emacs-lisp-mode))
       ((string-suffix-p ".rs" url)
        (rust-mode))
       ((string-suffix-p ".go" url)
        (go-mode))
       (t
        (when (and (plist-get eww-data :source)
                   ;; 排除微信公众号内的文章
                   (not (string-match-p "weixin\\.qq\\.com" url)))
          (eww-readable))))))

  (defun my/eww-browse-bing-dict ()
    (interactive)
    (switch-to-buffer-other-window
     (eww-browse-url
      (concat
       "http://www.bing.com/dict/search?mkt=zh-cn&q="
       (url-hexify-string (read-string "Query: "))))))
  )
;; eww:1 ends here

;; [[file:README.org::*联网工具][联网工具:1]]
(defun xwidget-webkit-search-forward (text)
  "Search forward of `text'"
  (interactive "sSearch: " xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session) (format "window.find(\"%s\");" text)))

(defun my/webkit-open-local-file (fpath)
  (interactive "fEnter file path: ")
  (when (member (substring fpath -4 nil) '("html" ".pdf" ".mp4"))
    (xwidget-webkit-browse-url (concat "file://" (expand-file-name fpath)))))
;; 联网工具:1 ends here

;; [[file:README.org::*Open App][Open App:1]]
;; open app
;; {{{
(defun mac-launchpad/string-ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
         (let ((elength (length ending)))
           (string= (substring s (- 0 elength)) ending)))
        (t nil))
  )

(defun mac-launchpad/find-mac-apps (folder)
  (let* ((files (directory-files folder))
         (without-dots (cl-delete-if
                        (lambda (f)
                          (or (string= "." f)
                              (string= ".." f)))
                        files))
         (all-files (mapcar (lambda (f)
                              (file-name-as-directory
                               (concat (file-name-as-directory folder)
                                       f)))
                            without-dots))
         (result (cl-delete-if-not
                  (lambda (s) (mac-launchpad/string-ends-with s ".app/"))
                  all-files)))
    result)
  )

(defun mac-launchpad ()
  (interactive)
  (let* ((apps (mac-launchpad/find-mac-apps "/Applications"))
         (to-launch (completing-read "launch: " apps)))
    (shell-command
     (format
      "defaults read \"%s\"Contents/Info.plist CFBundleIdentifier | xargs open -b"
      to-launch)))
  )
;; }}}
;; Open App:1 ends here

;; [[file:README.org::*Open in System file manager][Open in System file manager:1]]
(use-package dired
  :ensure nil
  :bind
  (
   :map dired-mode-map
   ("e" . my/dired-open-in-file-manager)
   )
  :config
  (defun my/dired-open-in-file-manager ()
    "Show current file in desktop.
This command can be called when in a file buffer or in `dired'."
    (interactive)
    (let (($path (if (buffer-file-name) (buffer-file-name) default-directory)))
      (cond
       ((string-equal system-type "windows-nt")
        (shell-command
         (format "PowerShell -Command Start-Process Explorer -FilePath %s"
                 (shell-quote-argument default-directory))))
       ((string-equal system-type "darwin")
        (if (eq major-mode 'dired-mode)
            (let (($files (dired-get-marked-files )))
              (if (eq (length $files) 0)
                  (shell-command
                   (concat "open "
                           (shell-quote-argument
                            (expand-file-name default-directory ))))
                (shell-command
                 (concat "open -R "
                         (shell-quote-argument
                          (car (dired-get-marked-files )))))))
          (shell-command
           (concat "open -R " (shell-quote-argument $path)))))
       ((string-equal system-type "gnu/linux")
        (let ((process-connection-type nil)
              (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                   "/usr/bin/gvfs-open"
                                 "/usr/bin/xdg-open")))
          (start-process ""
                         nil
                         openFileProgram
                         (shell-quote-argument $path)))))))
  )
;; Open in System file manager:1 ends here

;; [[file:README.org::*Open in Default app][Open in Default app:1]]
;; open in default app
;; {{{
;; https://emacs-china.org/t/pdf/14954/5
(defun my/open-with (arg)
  "使用外部程序打开浏览的文件或者当前光标下的链接.
处于 dired mode 时, 打开当前光标下的文件;
若当前光标下存在链接，使用外部程序打开链接;
使用 prefix ARG 时指定使用的外部程序."
  (interactive "P")
  (let ((current-file-name
         (cond ((eq major-mode 'dired-mode) (dired-get-file-for-visit))
               ((help-at-pt-string)
                (pcase (cdr (split-string (help-at-pt-string) ":" t " "))
                  ((or `(,path) `(,(pred (string= "file")) ,path) `(,_ ,path ,_))
                   (expand-file-name path))
                  (`(,proto ,path) (concat proto ":" path))))
               (t (or (thing-at-point 'url) buffer-file-name))))
        (program (if arg
                     (read-shell-command "Open current file with: ")
                   "open")))
    (call-process program nil 0 nil current-file-name))
  )
;; }}}
;; Open in Default app:1 ends here

;; [[file:README.org::*Open in Neovide][Open in Neovide:1]]
;; Neovide
;; {{{
(defun my/open-in-neovide ()
  (interactive)
  (start-process-shell-command "neovide"
                               nil
                               (concat "neovide "
                                       "+"
                                       (int-to-string (line-number-at-pos))
                                       " "
                                       (buffer-file-name)
                                       )))
;; }}}
;; Open in Neovide:1 ends here

;; [[file:README.org::*Open in Obsidian][Open in Obsidian:1]]
;; Obsidian
;; {{{
;; https://emacs-china.org/t/emacs-obsidian/22504/11?u=suliveevil
(defun my/open-in-obsidian () ;; 在 Obsidian 中打开当前 Emacs 正在编辑的文件
  (interactive)
  (browse-url-xdg-open
   (concat "obsidian://open?path=" (url-hexify-string (buffer-file-name)))))
;; }}}
;; Open in Obsidian:1 ends here

;; [[file:README.org::*Open in VSCode][Open in VSCode:1]]
;; Visual Studio Code
;; {{{
;; https://github.com/pietroiusti/.emacs.d/blob/master/custom-functions.el
(defun my/open-in-vscode ()
  (interactive)
  (start-process-shell-command
   "code" nil
   (concat
    "code --goto "
    (buffer-file-name)
    ":"
    (int-to-string (line-number-at-pos))
    ":"
    (int-to-string (current-column)))))
;; (w32-shell-execute "open" "vscode-path" (format "-g %s:%s:%s" (buffer-file-name) (int-to-string (line-number-at-pos)) (int-to-string (current-column))))
;; better solution
;; https://emacs-china.org/t/leader-vscode/19166/29
;; (defun my/open-in-vscode ()
;;   "Open current file with vscode."
;;   (interactive)
;;   (let ((line (number-to-string (line-number-at-pos)))
;;         (column (number-to-string (current-column))))
;;     (apply 'call-process "code" nil nil nil (list (concat buffer-file-name ":" line ":" column) "--goto"))))
;; (keymap-set global-map "C-c C" #'my/open-in-vscode)
;; }}}
;; Open in VSCode:1 ends here

;; [[file:README.org::*basic][basic:1]]
(use-package org
  :ensure nil
  :defer 1
  :bind
  (
   :map org-mode-map
   ("C-c l"   . org-store-link) ; C-c C-l org-insert-link
   ("C-c n o" . org-id-get-create)
   ("C-c t t" . org-insert-structure-template)
   ("C-c H-t" . my/sparse-tree-with-tag-filter)
   )
  :config
  (setq org-insert-heading-respect-content t) ; for C-<return>
  (setq org-directory "~/org-roam")
  ;; (setq org-fold-core-style "overlays")
  ;; (setq org-startup-indented t)
  (setq org-image-actual-width nil)
  (add-to-list 'auto-mode-alist
               '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  (add-to-list 'org-file-apps '("\\.odp" . "open %s"))
  ;; 标签 tag
  (defun my/sparse-tree-with-tag-filter()
    "asks for a tag and generates sparse tree for
all open tasks in current Org buffer
  that are associated with this tag"
    (interactive "*")
    (setq tag-for-filter
          (org-trim
           (org-icompleting-read "Tags: "
                                 'org-tags-completion-function
                                 nil nil nil 'org-tags-history))
          )
    (org-occur
     (concat "^\\*+ \\(NEXT\\|TODO\\|WAITING\\|STARTED\\) .+:"
             tag-for-filter
             ":")
     )
    )
  )
;; basic:1 ends here

;; [[file:README.org::*UI][UI:1]]
;; (setq org-hide-leading-stars t) ; Omit headline-asterisks except the last one
(setq org-src-fontify-natively t)  ; code block syntax highlight
(setq org-fontify-todo-headline nil)
(setq org-fontify-done-headline nil)

;; org-mode Face for org-id links.                                      ; FIXME
;; (defface my-org-id-link
;;   '((t
;;      :inherit org-link
;;      :underline nil
;;      ;; :foreground "#009600"
;;      :group 'org-faces
;;      ))
;;   :group 'org-faces)
;; (with-eval-after-load 'ol
;;   (org-link-set-parameters "id" :face 'my-org-id-link))
;; }}}
;; UI:1 ends here

;; [[file:README.org::*narrow][narrow:1]]
;; org-mode: head/title
;; (org-in-src-block-p)
;; {{{
;; 显示当前 heading 内容并折叠其他
;; https://emacstil.com/til/2021/09/09/fold-heading/
(defun my/org-show-current-heading-tidily ()
  (interactive)
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (save-excursion
      (outline-back-to-heading)
      (unless (and (bolp) (org-on-heading-p))
        (org-up-heading-safe)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children))
    )
  )

(keymap-global-set "C-c H-n" #'my/org-show-current-heading-tidily)

;; (defun my/org-narrow-heading-or-code-block ()
;;   (interactive)
;;   (cond ((org-in-src-block-p)
;;               (org-src-mode)
;;                t)
;;         (org-edit-src-exit)
;;         (org-show-current-heading-tidily)
;;         )
;;   (cond (eq (progn (eq (org-in-src-block-p) t)
;;                    (eq (org-src-mode) nil)) t)
;;         (org-edit-special)
;;         (delete-other-windows)
;;         )

;;   (cond (eq (progn  (eq (org-in-src-block-p) nil)
;;                     (eq (org-src-mode) nil)) t)
;;         (org-show-current-heading-tidily)
;;         )
;;   nil
;;   )
;; (keymap-global-set "C-c H-n" #'my/org-narrow-heading-or-code-block)
;; }}}
;; narrow:1 ends here

;; [[file:README.org::*code block: org-babel org-src][code block: org-babel org-src:1]]
(defconst my/org-special-pre "^\s*#[+]")
(defun my/org-2every-src-block (fn)
  "Visit every Source-Block and evaluate `FN'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward (concat my/org-special-pre "BEGIN_SRC") nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (funcall fn element)))))
    (save-buffer)))

;; https://www.wisdomandwonder.com/link/9008/a-progress-indicator-for-code-blocks-in-org-mode
(defadvice org-babel-execute-src-block (around progress nil activate)
  (set-face-attribute
   'org-block-background nil :background "LightSteelBlue")
  (message "Running your code block")
  ad-do-it
  (set-face-attribute 'org-block-background nil :background "gray")
  (message "Done with code block"))
;; code block: org-babel org-src:1 ends here

;; [[file:README.org::*code block: org-babel org-src][code block: org-babel org-src:2]]
(use-package ob ; org-src
  :ensure nil
  :defer t
  :bind
  (
   :map emacs-lisp-mode-map
   ("C-c M-o" . org-babel-tangle-jump-to-org)
   :map org-mode-map
   ;; ("s-]" . (lambda () (interactive)
   ;;            (my/org-2every-src-block
   ;;             'org-babel-remove-result)))
   ("C-c e" . org-edit-special)
   ("s-l" . org-edit-special)
   ("H-l" . org-edit-special)
   ("s-j" . org-babel-next-src-block)
   ("s-k" . org-babel-previous-src-block)
   :map org-src-mode-map
   ("C-c e" . org-edit-src-exit)
   ("s-l" . org-edit-src-exit)
   ("s-s" . org-edit-src-exit)
   )
  :init
  (setq org-src-fontify-natively 1)         ; 代码块语法高亮
  ;; (setq org-src-tab-acts-natively t)       ; 开启代码块语法缩进/格式化
  (setq org-edit-src-content-indentation 0) ; 代码块初始缩进范围
  (setq org-src-window-setup 'current-window)
  :config
  (setq org-src-lang-modes
        '(
          ("C" . c)
          ("C++" . c++)
          ("asymptote" . asy)
          ("bash" . sh)
          ("beamer" . latex)
          ("calc" . fundamental)
          ("cpp" . c++)
          ("desktop" . conf-desktop)
          ("ditaa" . artist)
          ("dot"  . graphviz-dot)
          ("elisp" . emacs-lisp)
          ("json"  . json-ts)
          ("ocaml" . tuareg)
          ("screen" . shell-script)
          ("shell" . sh)
          ("sqlite" . sql)
          ("toml" . conf-toml)
          ))
  )
;; code block: org-babel org-src:2 ends here

;; [[file:README.org::*code block: org-babel org-src][code block: org-babel org-src:3]]
(use-package org-src
  :ensure nil
  ;; :defer t
  )
;; code block: org-babel org-src:3 ends here

;; [[file:README.org::*Link][Link:1]]
(use-package org
  :ensure nil
  :defer t
  :config
  (setq my-linkcolor-org "wheat3")
  (setq my-linkcolor-file "MediumSeaGreen")
  (setq my-linkcolor-web "DeepSkyBlue")

  (defun my-set-linkcolors ()
    "Defines the colors of various link colors"
    (interactive)

    ;; Org links ---------------------------------------------------------------

    (org-link-set-parameters "id"
                             :face
                             `(:foreground ,my-linkcolor-org :underline t))
    (org-link-set-parameters "contact"
                             :face
                             `(:foreground ,my-linkcolor-org :underline t))

    ;; File links --------------------------------------------------------------

    (org-link-set-parameters "file"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))
    ;; defined elsewhere;; (org-link-set-parameters "tsfile" :face '`(:foreground "DarkSeaGreen" :underline t))
    (org-link-set-parameters "pdf"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))

    (org-link-set-parameters "EPA"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))
    (org-link-set-parameters "EPAAFO"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))
    (org-link-set-parameters "JAFO"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))
    (org-link-set-parameters "DAKEPA"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))
    (org-link-set-parameters "BMTSK"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))
    (org-link-set-parameters "ISO"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))

    (org-link-set-parameters "gemSpec_DS_Anbieter"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))
    (org-link-set-parameters "gemSpec_Net"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))
    (org-link-set-parameters "gemSpec_PKI"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))
    (org-link-set-parameters "gemSpec_IDP_Dienst"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))

    (org-link-set-parameters "messageid"
                             :face
                             `(:foreground ,my-linkcolor-file :underline t))

    ;; Web links ---------------------------------------------------------------

    (org-link-set-parameters "http"
                             :face
                             `(:foreground ,my-linkcolor-web :underline t))
    (org-link-set-parameters "https"
                             :face
                             `(:foreground ,my-linkcolor-web :underline t))
    )

  (defun my-set-linkcolors ()
    "Defines the colors of various link colors"
    (interactive)

    ;; Org links ---------------------------------------------------------------

    (org-link-set-parameters "id"
                             :face
                             '(:foreground "wheat3" :underline t))
    (org-link-set-parameters "contact"
                             :face
                             '(:foreground "wheat3" :underline t))

    ;; File links --------------------------------------------------------------

    (org-link-set-parameters "file"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    ;; defined elsewhere;; (org-link-set-parameters "tsfile" :face ''(:foreground "DarkSeaGreen" :underline t))
    (org-link-set-parameters "pdf"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))

    (org-link-set-parameters "EPA"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    (org-link-set-parameters "EPAAFO"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    (org-link-set-parameters "JAFO"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    (org-link-set-parameters "DAKEPA"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    (org-link-set-parameters "BMTSK"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    (org-link-set-parameters "gemSpec_DS_Anbieter"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    (org-link-set-parameters "gemSpec_Net"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    (org-link-set-parameters "gemSpec_PKI"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    (org-link-set-parameters "gemSpec_IDP_Dienst"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))
    (org-link-set-parameters "messageid"
                             :face
                             '(:foreground "MediumSeaGreen" :underline t))

    ;; Web links ---------------------------------------------------------------

    (org-link-set-parameters "http"
                             :face
                             '(:foreground "DeepSkyBlue" :underline t))
    (org-link-set-parameters "https"
                             :face
                             '(:foreground "DeepSkyBlue" :underline t))

    )

  (my-set-linkcolors) ;; set colors when loading
  )
;; Link:1 ends here

;; [[file:README.org::*图像][图像:1]]
(defadvice org-toggle-inline-images (after org-open-at-point activate)
  (if smooth-scrolling-mode (smooth-scrolling-mode -1)
    (smooth-scrolling-mode 1)))
;; 图像:1 ends here

;; [[file:README.org::*PDF][PDF:1]]
(use-package org
  :ensure nil
  :defer t
  :config
  (add-to-list
   'org-file-apps
   '("\\.[pP][dD][fF]\\'" . "open %s") ; 使用默认 app 打开 PDF
   ;; '("\\.pdf\\'" . "open -b com.apple.Safari %s")
   )
  )
;; PDF:1 ends here

;; [[file:README.org::*org-agenda][org-agenda:1]]
(use-package org-agenda
  :ensure nil
  :defer t
  :bind ("ESC M-RET" . org-agenda)
  )
;; org-agenda:1 ends here

;; [[file:README.org::*org-capture][org-capture:1]]
(use-package org-capture
  :ensure nil
  :defer t
  :bind
  ("ESC M-c". org-capture)
  :hook ((org-capture-mode . (lambda ()
                               (setq-local
                                org-complete-tags-always-offer-all-agenda-tags
                                t)))
         (org-capture-mode . delete-other-windows))
  :config
  (setq org-default-notes-file "~/org-roam/notes/inbox.org")
  :custom
  (org-capture-use-agenda-date nil)
  ;; define common template
  (org-capture-templates `(
                           ("t" "Tasks" entry
                            (file+headline "task.org" "Reminders")
                            "* TODO %i%?"
                            :empty-lines-after 1
                            :prepend t)
                           ("n" "Notes" entry
                            (file+headline "capture.org" "Notes")
                            "* %? %^g\n%i\n"
                            :empty-lines-after 1)
                           ;; For EWW
                           ("b" "Bookmarks"
                            entry (file+headline "capture.org" "Bookmarks")
                            "* %:description\n\n%a%?"
                            :empty-lines 1
                            :immediate-finish t)
                           ("d" "Diary")
                           ("dt" "Today's TODO list"
                            entry (file+olp+datetree "diary.org")
                            "* Today's TODO list [/]\n%T\n\n* TODO %?"
                            :empty-lines 1
                            :jump-to-captured t)
                           ("do" "Other stuff"
                            entry (file+olp+datetree "diary.org")
                            "* %?\n%T\n\n%i"
                            :empty-lines 1
                            :jump-to-captured t)
                           ))
  )
;; org-capture:1 ends here

;; [[file:README.org::*网络搜索][网络搜索:1]]
(use-package xwidget
  :ensure nil
  :bind
  (
   ("M-s e" . my/open-microsoft-bing) ; open search engine
   :map xwidget-webkit-mode-map
   ("C-h b" . describe-bindings)
   ;; :map xwidget-webkit-edit-mode-map
   )
  :config
  ;; bing search
  (defun my/open-microsoft-bing ()
    (interactive)
    (xwidget-webkit-browse-url "https://www.bing.com" t)
    )
  )
;; 网络搜索:1 ends here

;; [[file:README.org::*OCR][OCR:1]]
(use-package simple
  :ensure nil
  :bind
  ("C-c H-o" . my/siri-ocr)
  :config
  ;; Siri Shortcuts: OCR
  (defun my/siri-ocr ()
    (interactive)
    (shell-command "shortcuts run \"OCR Selected Area\"")
    (do-applescript "tell application id \"org.gnu.Emacs\" to activate")
    (shell-command "pbpaste")
    )
  )
;; OCR:1 ends here

;; [[file:README.org::*Translate][Translate:1]]
(use-package simple
  :ensure nil
  :bind ("H-t H-t" . my/translate-language-to-zh-or-zh-to-english)
  :config
  ;; Siri Shortcuts: Translate
  (defun my/siri-translate ()
    (interactive)
    (let ((tempfile (make-temp-file "siri-translate-" nil ".txt")))
      (write-region (format "%s" (thing-at-point 'paragraph)) nil tempfile)
      (end-of-paragraph-text) ; jump to end of paragraph
      (shell-command (format "shortcuts run \"Translate File\" -i %s" tempfile)))
    (shell-command "open -b org.gnu.Emacs")
    ;; (shell-command "pbpaste")
    )

  (defun my/siri-translate2english ()
    (interactive)
    (let ((tempfile (make-temp-file "siri-translate-" nil ".txt")))
      (write-region (format "%s" (thing-at-point 'paragraph)) nil tempfile)
      (end-of-paragraph-text) ; jump to end of paragraph
      (shell-command
       (format "shortcuts run \"Translate File 2 English\" -i %s" tempfile)))
    (shell-command "open -b org.gnu.Emacs")
    ;; (shell-command "pbpaste")
    )

  (defun my/translate-language-to-zh-or-zh-to-english ()
    (interactive) ; 测试
    (let ((string (thing-at-point 'paragraph)))
      (if (eq (string-match "\\cC" string) nil)
          (my/siri-translate)
        (my/siri-translate2english)))
    (shell-command "pbpaste")))
;; Translate:1 ends here

;; [[file:README.org::*Alfred Search][Alfred Search:1]]
;; Alfred
;; {{{
;; https://github.com/xuchunyang/emacs.d/blob/master/lisp/alfred.el
(defun my/alfred-search (b e)
  "Activate Alfred with selected text."
  (interactive "r")
  (do-applescript
   (format
    "tell application id \"com.runningwithcrayons.Alfred\" to search \"%s\""
    (mapconcat ;; In AppleScript String, " and \ are speical characters
     (lambda (char)
       (pcase char
         (?\" (string ?\\ ?\"))
         (?\\ (string ?\\ ?\\))
         (_   (string char)))
       )
     (buffer-substring b e) "")
    )
   )
  )
;; }}}
;; Alfred Search:1 ends here

;; [[file:README.org::*MacVim][MacVim:1]]
;; MacVim
;; {{{
(defun my/open-in-macvim ()
  (interactive)
  (start-process-shell-command "mvim"
                               nil
                               (concat "mvim "
                                       (buffer-file-name)
                                       " -c 'normal "
                                       (int-to-string (line-number-at-pos))
                                       "G"
                                       (int-to-string (current-column))
                                       "|'"
                                       )))
;; }}}
;; MacVim:1 ends here

;; [[file:README.org::*insert key sequence][insert key sequence:1]]
;; https://emacs.stackexchange.com/a/12906
(defun my/insert-key-sequence (key)
  (interactive "kKey Sequence: ")
  (insert (format
           "%S" ; "(kbd %S)"
           (key-description key))))

(keymap-global-set "C-c M-k" #'my/insert-key-sequence)
;; insert key sequence:1 ends here

;; [[file:README.org::*File End][File End:1]]
;;; init.el ends here.
;; File End:1 ends here

;; [[file:README.org::*Emacs 插件配置][Emacs 插件配置:1]]
(add-to-list 'load-path
             (concat user-emacs-directory  "site-lisp"))
(require 'my-package)
;; Emacs 插件配置:1 ends here
