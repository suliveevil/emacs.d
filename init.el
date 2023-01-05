;; -*- coding: utf-8; lexical-binding: t; -*-
;; -*- origami-fold-style: triple-braces -*-
;; init

(let (
      (gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil)
      ))

;; test
;; {{{
(require 'treesit)
(add-to-list 'load-path (expand-file-name "~/.config/emacs/bisec"))
;; }}}

;; warning
;; {{{
;; (add-to-list 'warning-suppress-log-types '((defvaralias))) ; FIXME
;; }}}

;; file/buffer
;; {{{
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
;;
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
      (set-window-buffer (selected-window) "*Scratch*"))))
(keymap-global-set "C-c B" #'my/side-buffer)

;; kill buffer
(defun my/kill-all-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

;; delete buffer file
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

;; chunk
;; {{{
;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000) ;; 64kb
;; }}}

;; cursor move
;; {{{
;; [Emacs 一行内移动 cursor 的最佳方案是什么？ - Emacs China](https://emacs-china.org/t/emacs-cursor/6753/12)
;;
;; char-wise
;; goto-char by Oliver Scholz
(defun my/go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(keymap-set global-map "C-c a" #'my/go-to-char)
;; similar work
;; [go-to-char.el - EmacsWiki](https://www.emacswiki.org/emacs/go-to-char.el)
;; [joseph-go-to-char - EmacsWiki](https://www.emacswiki.org/emacs/joseph-go-to-char)
;; [doitian/iy-go-to-char: Go to next CHAR which is similar to "f" and "t" in vim](https://github.com/doitian/iy-go-to-char)
;;
;; zap-up-to-char
(keymap-global-set "M-z" #'zap-up-to-char)
;; }}}

;; window
;; {{{
;; toggle one window
;; https://github.com/manateelazycat/toggle-one-window
(defvar toggle-one-window-window-configuration nil
  "The window configuration use for `toggle-one-window'.")
;;
(defun my/toggle-one-window ()
  "Toggle between window layout and one window."
  (interactive)
  (if (equal (length (cl-remove-if #'window-dedicated-p (window-list))) 1)
      (if toggle-one-window-window-configuration
          (progn
            (set-window-configuration toggle-one-window-window-configuration)
            (setq toggle-one-window-window-configuration nil))
        (message "No other windows exist."))
    (setq toggle-one-window-window-configuration (current-window-configuration))
    (delete-other-windows)))
(keymap-global-set "C-c C-w" #'my/toggle-one-window)
;; }}}

;; frame
;; {{{
(setq frame-size-history t)
(setq frame-title-format
      '(buffer-file-name (:eval (abbreviate-file-name buffer-file-name))
                         (dired-directory dired-directory "%b")))
;; }}}

;; font and syntax
;; {{{
(set-face-attribute 'default nil
                    :family "Sarasa Mono SC Nerd"
                    :height 140 ;; 更改显示字体大小
                    )
(global-font-lock-mode t) ;; turn on syntax highlighting for all buffers
;; }}}

;; key
;; {{{
;; bind: 全局按键/快捷键 (Global key bindings)
(setq echo-keystrokes 0.1)
(setq mac-command-modifier 'super         ;; s: super(Command/Win)
      mac-control-modifier 'control       ;; C: Ctrl
      mac-right-control-modifier 'control ;; C: Ctrl
      mac-option-modifier  'meta          ;; M: Meta (Option/Alt)
      mac-right-command-modifier 'hyper   ;; H: hyper (reachable for thumb)
      mac-right-option-modifier 'none     ;; macOS style Option
      ;; mac-function-modifier            ;; Function Key
      ;;                                  ;; A: Alt (redundant and not used)
      ;;                                  ;; H: Hyper
      ;;                                  ;; S: Shift
      )
(keymap-global-set "s-a" #'mark-whole-buffer)       ;;        : selection : 全选
(keymap-global-set "s-c" #'kill-ring-save)          ;; M-w    : copy      : 复制
(keymap-global-set "s-q" #'save-buffers-kill-emacs) ;;        : copy      : 复制
(keymap-global-set "s-v" #'yank)                    ;; C-y    : paste/yank: 粘贴
(keymap-global-set "s-w" #'delete-frame)            ;;        :           :
(keymap-global-set "s-s" #'save-buffer)             ;; C-x C-s: save      : 保存
(keymap-global-set "s-x" #'kill-region)             ;; C-w    : cut       : 剪切
(keymap-global-set "s-z" #'undo)                    ;; C-_    : undo      : 撤销
(keymap-global-set "s-Z" #'undo-redo)               ;; C-M-_  : undo-redo : 重做
;;
(keymap-global-set "S-s-<return>" #'toggle-frame-maximized)
(keymap-global-set "C-s-f"        #'toggle-frame-fullscreen) ;; macOS

;; }}}

;; mouse
;; {{{
(setq mouse-wheel-tilt-scroll t)
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

;; pretty-symbols
;; {{{
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(setq-default prettify-symbols-alist
              '(("lambda" . ?λ)
                ("function" . ?𝑓)))
;; }}}

;; cursor and region
;; {{{
;; make cursor the width of the character it is under i.e. full width of a TAB
(setq x-stretch-cursor t) ;; When on a tab, make the cursor the tab length.
;; cursor line: 光标所在行显示/高亮
;; (global-hl-line-mode t) ;; highlight current line
(custom-set-faces '(hl-line ((t (:background "grey")))))
(delete-selection-mode t) ;; 删除选中的文字或选中文字后输入时替换选中的文字
(global-subword-mode)     ;; camelCase and superword-mode
;; }}}

;; line
;; {{{
;; wrap/truncate
(setq word-wrap-by-category t) ;; improves CJK + Latin word-wrapping
(setq scroll-margin 5)
(global-display-line-numbers-mode 1)
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
(keymap-global-set "C-c O" #'open-newline-above)
(defun open-newline-below (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))
(keymap-global-set "C-c C-o" #'open-newline-below)
;; }}}

;; sentence: 断句
;; {{{
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; (setq sentence-end-double-space nil)
;; }}}

;; column
;; {{{
(setq-default fill-column 80) ;; M-x set-fill-column RET
(add-hook 'after-init-hook 'global-display-fill-column-indicator-mode)
;; }}}

;; mode-line
;; {{{
(display-battery-mode t)    ;; display battery status
(setq column-number-mode t) ;; 在 mode line 数字形式显示光标所在列
;; }}}

;; completion: buffer and minibuffer
;; {{{
;; hippie-expand
(keymap-global-set "M-/" #'hippie-expand)
;; window
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
;; style
(setq completion-styles '(substring initials partial-completion flex basic))
(setq completion-cycle-threshold 10)
;; pair
(electric-pair-mode 1)
;; }}}

;; fold
;; {{{
(add-hook 'prog-mode 'hs-minor-mode)
(add-to-list 'hs-special-modes-alist
             '(emacs-lisp-mode "{" "}" ";;" nil nil))
(keymap-global-set "C-c TAB" #'hs-toggle-hiding)
(keymap-global-set "M-+" #'hs-show-all)
;; }}}

;; time
;; {{{
(defun my/date-and-time-iso8601 ()
  (interactive)
  (insert (format-time-string "%FT%T%z"))
  )

(keymap-global-set "C-c D" #'my/date-and-time-iso8601)
;; }}}

;; completion: dabbrev: dynamic abbreviation expand
;; {{{
(keymap-set minibuffer-mode-map "TAB" #'minibuffer-complete)
(keymap-global-set               "C-<tab>" #'dabbrev-expand)
(keymap-set minibuffer-local-map "C-<tab>" #'dabbrev-expand)
;; }}}

(global-auto-revert-mode 1) ;; 使 Emacs 自动加载外部修改过的文件

(add-hook 'after-init-hook 'auto-save-visited-mode) ;; save file when buffer/focus change 自动保存

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
      auto-save-default t ; auto-save every buffer that visits a file
      auto-save-timeout 20 ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
      )
;; auto-save: 定期预存，防止停电、系统崩溃等原因造成的数据损失
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; lockfile: 不同进程修改同一文件
;; {{{
(setq create-lockfiles t)
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
;; }}}

;; move file to trash when delete
;; {{{
;;; macOS
(when (eq system-type 'darwin)
  (setq trash-directory "~/.Trash/")
  (setq delete-by-moving-to-trash t))
;; }}}

;; symlink
;; {{{
(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn
        (setq buffer-read-only t)
        (message "File is a symlink"))))
(add-hook 'find-file-hooks 'read-only-if-symlink)
;; }}}

;; 快速打开文件
;; {{{
(defun my/open-init-file() ;; Emacs init
  (interactive)
  (find-file-other-window user-init-file))
(keymap-global-set "C-c I" #'my/open-init-file)
;;
;; (defun open-goku-file()      ;; Emacs early-init
;;   (interactive)
;;   (find-file "~/.config/karabiner.edn")
;;   (find-file "~/.config/goku/karabiner.edn")
;; )
;; }}}

;; dired
;; {{{
(require 'dired)
(defun dired-open-dwim ()
  (interactive)
  (if (file-directory-p (dired-file-name-at-point))
      (dired-find-file)
    (dired-find-file-other-window)))
(keymap-set dired-mode-map "RET" 'dired-open-dwim)
;; }}}

;; file name/file extension/file path
;; {{{
;; https://github.com/chyla/kill-file-path
;; [如何在文件夹层次结构中找到所有不同的文件扩展名？ |](https://qa.1r1g.com/sf/ask/128957811/#)
;;
;; file name
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
;;
;; file path
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
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
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

;; refresh-file: format/indent elisp file
;; {{{
;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/extensions/lazycat/basic-toolkit.el
(defun refresh-file ()
  "Automatic reload current file."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (indent-buffer)
         (indent-comment-buffer)
         (save-buffer)
         (load-file (buffer-file-name)))
        ((member major-mode '(lisp-mode c-mode perl-mode))
         (indent-buffer)
         (indent-comment-buffer)
         (save-buffer))
        ((member major-mode '(haskell-mode sh-mode))
         (indent-comment-buffer)
         (save-buffer))
        ((derived-mode-p 'scss-mode)
         (require 'css-sort)
         (css-sort))
        (t (message "Current mode is not supported, so not reload"))))
(defun indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (if (derived-mode-p 'python-mode)
      (message "Don't indent python buffer, it will mess up the code syntax.")
    (save-excursion
      (indent-region (point-min) (point-max) nil)
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max)))))
(defun indent-comment-buffer ()
  "Indent comment of buffer."
  (interactive)
  (indent-comment-region (point-min) (point-max)))

(defun indent-comment-region (start end)
  "Indent region."
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (if (comment-search-forward end t)
          (comment-indent)
        (goto-char end)))))
;; }}}

;; ido
;; {{{
(use-package ido
  :config
  (setq ido-vertical-mode t)
  (setq ido-enable-flex-matching t)
  )
;; }}}

;; isearch
;; {{{
;; M-<: first match
;; M->: last  match
(use-package isearch
  :bind
  ([remap yank] . isearch-yank-kill)
  :config
  (setq isearch-lazy-count t) ;; anzu
  )

;; }}}

;; org-mode
;; {{{
;; (setq org-startup-indented t)
(keymap-global-set "C-c l" #'org-store-link)
;;
;; 显示当前 heading 内容并折叠其他
;; https://emacstil.com/til/2021/09/09/fold-heading/
(defun org-show-current-heading-tidily ()
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
    ))
;; code block: TAB 格式化
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
(setq org-src-fontify-natively 1)         ;代码块语法高亮
(setq org-src-tab-acts-natively 1)        ;开启代码块语法缩进
(setq org-edit-src-content-indentation 0) ;代码块初始缩进范围
;;
(setq org-fontify-todo-headline nil)
(setq org-fontify-done-headline nil)
;;
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (awk         .       t)
   ;; (c           .       t)
   (calc        .       t)
   (comint      .       t)
   (css         .       t)
   (dot         .       t)
   (emacs-lisp  .       t)
   (eshell      .       t)
   (haskell     .       t)
   (js          .       t)
   (latex       .       t)
   (lua         .       t)
   (org         .       t)
   (perl        .       t)
   (plantuml    .       t)
   (python      .       t)
   (ruby        .       t)
   (sed         .       t)
   (shell       .       t)
   (sql         .       t)
   (sqlite      .       t)
   ))
;; }}}

;; tree-sitter
;; {{{
(when (treesit-available-p)
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          ;; (c++-mode        . c++-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (css-mode        . css-ts-mode)
          (java-mode       . java-ts-mode)
          (js-mode         . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (sh-mode         . bash-ts-mode))))
;; }}}

;; open app
;; {{{
(defun mac-launchpad/string-ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
         (let ((elength (length ending)))
           (string= (substring s (- 0 elength)) ending)))
        (t nil)))

(defun mac-launchpad/find-mac-apps (folder)
  (let* ((files (directory-files folder))
         (without-dots (cl-delete-if (lambda (f) (or (string= "." f) (string= ".." f))) files))
         (all-files (mapcar (lambda (f) (file-name-as-directory (concat (file-name-as-directory folder) f))) without-dots))
         (result (cl-delete-if-not (lambda (s) (mac-launchpad/string-ends-with s ".app/")) all-files)))
    result))

(defun mac-launchpad ()
  (interactive)
  (let* ((apps (mac-launchpad/find-mac-apps "/Applications"))
         (to-launch (completing-read "launch: " apps)))
    (shell-command (format "defaults read \"%s\"Contents/Info.plist CFBundleIdentifier | xargs open -b" to-launch))))


;; (keymap-global-set "C-c C-l" #'mac-launchpad)
;; }}}

;; open with default app
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
    (call-process program nil 0 nil current-file-name)))
;; }}}

;; Alfred
;; {{{
;; https://github.com/xuchunyang/emacs.d/blob/master/lisp/alfred.el
(defun my/alfred-search (b e)
  "Activate Alfred with selected text."
  (interactive "r")
  (do-applescript
   (format "tell application id \"com.runningwithcrayons.Alfred\" to search \"%s\""
           (mapconcat ;; In AppleScript String, " and \ are speical characters
            (lambda (char)
              (pcase char
                (?\" (string ?\\ ?\"))
                (?\\ (string ?\\ ?\\))
                (_   (string char))))
            (buffer-substring b e) ""))))
;; }}}

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
;; 防止反复调用 package-refresh-contents 影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))
;; }}}

;; profile: benchmark-init
;; {{{
(require 'benchmark-init-modes)
(require 'benchmark-init)
(benchmark-init/activate)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)
;; }}}

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
           for req-names = (cl-loop for it in (package-desc-reqs desc) collect (car it))
           collect (cons pkg req-names)))
;; (setq info (get-pkg-reqs-alist))

(setq info (my/emacs-package-dependency))

;; (with-temp-file "/tmp/g.dot"
(with-temp-file "~/.config/emacs/assets/emacs-package-dependency.dot"
  (insert "digraph G {")
  (insert (mapconcat #'identity
                     (cl-loop for pkg-reqs in info
                              for pkg = (car pkg-reqs)
                              for reqs = (cdr pkg-reqs)
                              nconcing (cl-loop for req in reqs
                                                collect (format "\"%s\" -> \"%s\";\n" pkg req)))))
  (insert "}"))
;; }}}

;; ibuffer
;; {{{
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
  )
;; }}}



;; Siri Shortcuts: OCR
;; {{{
(defun my/siri-ocr ()
  (interactive)
  (shell-command "shortcuts run \"OCR Selected Area\"")
  (do-applescript "tell application id \"org.gnu.Emacs\" to activate")
  )
(keymap-global-set "C-c H-o" #'my/siri-ocr)
;; }}}

;; Siri Shortcuts: Translate
;; {{{
(add-to-list 'display-buffer-alist
             (cons
              "\\*Async Shell Command\\*.*"
              (cons #'display-buffer-no-window nil)))

(defun my/siri-translate ()
  (interactive)
  (let
      ((tempfile
        (make-temp-file "siri-translate-" nil ".txt")
        ))
    (write-region
     (format "%s" (thing-at-point 'paragraph))
     nil
     tempfile)
    (end-of-paragraph-text)             ; jump to end of paragraph
    (shell-command (format "shortcuts run \"Translate File\" -i %s &" tempfile))
    )
  (shell-command "open -b org.gnu.Emacs")
  )

;; (keymap-global-set "C-c C-t" #'my/siri-translate)

(defun my/siri-translate2english ()
  (interactive)
  (let
      ((tempfile
        (make-temp-file "siri-translate-" nil ".txt")
        ))
    (write-region
     (format "%s" (thing-at-point 'paragraph))
     nil
     tempfile)
    (end-of-paragraph-text)             ; jump to end of paragraph
    (shell-command (format "shortcuts run \"Translate File 2 English\" -i %s &" tempfile))
    )
  (shell-command "open -b org.gnu.Emacs")
  )

;; (keymap-global-set "C-c C-e" #'my/siri-translate2english)

(defun language-to-zh-or-zh-to-english ()
  (interactive) ;; 测试
  (let ((string (thing-at-point 'paragraph)))
    (if (eq (string-match "\\cC" string) nil)
        (my/siri-translate)
      (my/siri-translate2english)
      )
    )
  )

(keymap-global-set "H-t H-t" #'language-to-zh-or-zh-to-english)
;; }}}

;; comment
;; {{{
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
(keymap-global-set "H-/" #'comment-current-line-dwim)
;; }}}


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

;; Visual Studio Code
;; {{{
;; https://github.com/pietroiusti/.emacs.d/blob/master/custom-functions.el
(defun my/open-in-vscode ()
  (interactive)
  (start-process-shell-command "code"
                               nil
                               (concat "code --goto "
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

;; Obsidian
;; {{{
;; https://emacs-china.org/t/emacs-obsidian/22504/11?u=suliveevil
(defun my/open-in-obsidian () ;; 在 Obsidian 中打开当前 Emacs 正在编辑的文件
  (interactive)
  (browse-url-xdg-open
   (concat "obsidian://open?path=" (url-hexify-string (buffer-file-name)))))
;; }}}

;; project
;; {{{
(use-package project
  ;; :bind-keymap
  ;; (("C-c p" . project-prefix-map))
  )
;; }}}


;; package config
;; {{{
;; (add-to-list 'load-path (expand-file-name "init-package.el"  (concat user-emacs-directory))) ;; :FIXME:
;; (add-to-list 'load-path "~/.config/emacs/init-package.el")
;; (require 'init-package) ;; packages installed by package.el
;; }}}

;; org-mode Face for org-id links.                                      ; FIXME
;; {{{
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


;; package out of package.el :FIXME:
;; {{{
;; :FIXME:
;; (add-to-list 'load-path (expand-file-name
;; "init-lib.el"
;; user-emacs-directory))
;; (add-to-list 'load-path "~/.config/emacs/init-lib.el")

;; }}}

;; diagram-preview
;; {{{

;; }}}

(require 'init-package) ; packages configuration
(require 'init-lib)     ; packages (out of elpa/melpa) configuration
(require 'init-test)    ; test my little functions
;; init.el
