;; -*- coding: utf-8; lexical-binding: t; -*-
;; -*- origami-fold-style: triple-braces -*-
;;; init.el

;; Date: 2023-01-06T12:52:58+0800

(let (
      (gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil)
      )
  )

;; time
;; {{{
(defun my/date-and-time-iso8601 ()
  (interactive)
  (insert (format-time-string "%FT%T%z"))
  )

(keymap-global-set "C-c D" #'my/date-and-time-iso8601)
;; }}}

;; font and syntax
;; {{{
(set-face-attribute 'default nil
                    :family "Sarasa Mono SC Nerd"
                    :height 140 ;; 更改显示字体大小
                    )
(global-font-lock-mode t) ;; turn on syntax highlighting for all buffers
;; }}}

;; pretty-symbols
;; {{{
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(setq-default prettify-symbols-alist
              '(("lambda" . ?λ)
                ("function" . ?𝑓)))
;; }}}

;; tree-sitter
;; {{{
(require 'treesit)
;; (tree-sitter-load 'elisp "elisp")
;; (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
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

;; chunk
;; {{{
;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000) ;; 64kb
;; }}}

;; zap-up-to-char
(keymap-global-set "M-z" #'zap-up-to-char)

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

;; warn when opening files bigger than 100 MB
(setq large-file-warning-threshold 100000000)

;; 使 Emacs 自动加载外部修改过的文件
(global-auto-revert-mode 1)

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

;; auto-save: 定期预存，防止停电、系统崩溃等原因造成的数据损失
;; {{{
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-hook 'after-init-hook 'auto-save-visited-mode) ;; save file when buffer/focus change 自动保存
(setq
      auto-save-default t ; auto-save every buffer that visits a file
      auto-save-timeout 20 ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
 )
;; }}}

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

;; 快速打开文件
;; {{{
(defun my/open-init-file() ;; Emacs init
  (interactive)
  (find-file-other-window user-init-file))
(keymap-global-set "C-c I" #'my/open-init-file)

(defun my/open-init-org() ;; Emacs init
  (interactive)
  (find-file-other-window
   (expand-file-name
    "init.org"
    (concat user-emacs-directory)
    )
   )
  )
(keymap-global-set "C-c H-i" #'my/open-init-org)
;; (defun open-goku-file()      ;; Emacs early-init
;;   (interactive)
;;   (find-file "~/.config/karabiner.edn")
;;   (find-file "~/.config/goku/karabiner.edn")
;; )

;; }}}

;; side buffer
;; {{{
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
;; }}}

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

;; kill buffer
;; {{{
(defun my/kill-all-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
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

;; project
;; {{{
(use-package project
  ;; :bind-keymap
  ;; (("C-c p" . project-prefix-map))
  )
;; }}}

;; frame
;; {{{
(setq frame-size-history t)
(setq frame-title-format
      '(buffer-file-name (:eval (abbreviate-file-name buffer-file-name))
                         (dired-directory dired-directory "%b")))
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

;; fold
;; {{{
(add-hook 'prog-mode 'hs-minor-mode)
(add-to-list 'hs-special-modes-alist
             '(emacs-lisp-mode "{" "}" ";;" nil nil))
(keymap-global-set "C-c TAB" #'hs-toggle-hiding)
(keymap-global-set "M-+" #'hs-show-all)
;; }}}

;; cursor
;; {{{
;; cursor move
;; [Emacs 一行内移动 cursor 的最佳方案是什么？ - Emacs China](https://emacs-china.org/t/emacs-cursor/6753/12)
;; make cursor the width of the character it is under i.e. full width of a TAB
(setq x-stretch-cursor t) ;; When on a tab, make the cursor the tab length.
;; cursor line: 光标所在行显示/高亮
;; (global-hl-line-mode t) ;; highlight current line
(custom-set-faces '(hl-line ((t (:background "grey")))))
(delete-selection-mode t) ;; 删除选中的文字或选中文字后输入时替换选中的文字
(global-subword-mode)     ;; camelCase and superword-mode
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

;; goto-char by Oliver Scholz
;; {{{
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

;; column
;; {{{
(setq-default fill-column 80) ;; M-x set-fill-column RET
(add-hook 'after-init-hook 'global-display-fill-column-indicator-mode)
;; }}}

;; sentence: 断句
;; {{{
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; (setq sentence-end-double-space nil)
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
(keymap-set isearch-mode-map "s-v" 'isearch-yank-kill)
(setq isearch-lazy-count t) ;; anzu
;; }}}

;; completion: buffer and minibuffer
;; {{{
;; dabbrev: dynamic abbreviation expand
(keymap-set minibuffer-mode-map "TAB" #'minibuffer-complete)
(keymap-global-set               "C-<tab>" #'dabbrev-expand)
(keymap-set minibuffer-local-map "C-<tab>" #'dabbrev-expand)
;; hippie-expand
(keymap-global-set "M-/" #'hippie-expand)
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
;; completion style
(setq completion-styles '(substring initials partial-completion flex basic))
(setq completion-cycle-threshold 10)
;; pair completion
(electric-pair-mode 1)
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
        (goto-char end))
      )
    )
  )
;; }}}

;; org-mode: keymap
;; {{{
;; (setq org-startup-indented t)
;; (keymap-global-set "C-c l"   #'org-store-link) ; C-c C-l org-insert-link
(keymap-global-set "C-c n o" #'org-id-get-create)
;; (keymap-set org-mode-map "C-c n o" #'org-id-get-create) ;FIXME
;; }}}

;; org-mode: head/title
;; {{{
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
;; }}}

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
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (awk         .       t)
   ;; (c           .       t) ; FIXME
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
(setq org-src-fontify-natively 1)         ;代码块语法高亮
(setq org-src-tab-acts-natively 1)        ;开启代码块语法缩进/格式化
(setq org-edit-src-content-indentation 0) ;代码块初始缩进范围

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

;; mode-line
;; {{{
(display-battery-mode t)    ;; display battery status
(setq column-number-mode t) ;; 在 mode line 数字形式显示光标所在列
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
                                                collect (format "\"%s\" -> \"%s\";\n" pkg req)))))
  (insert "}"))
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

;; (defun language-detect-zh ()
;;   (interactive)
;;   (let ((zh-words 0) (en-words 0))
;;     (with-temp-buffer
;;       (insert (format (thing-at-point 'paragraph)))
;;       (goto-char (point-min))
;;       (while (< (point) (point-max))
;;         (let ((ch (char-to-string (char-after))))
;;           (cond
;;            ((string-match "\\cC" ch)
;;             (let ((start-point (point)))
;;               (forward-word)
;;               (setq zh-words (+ zh-words (- (point) start-point)))))
;;            ((string-match "[a-zA-Z]" ch)
;;             (forward-word)
;;             (setq en-words (1+ en-words)))
;;            (t
;;             (forward-char))))))
;;     (if (< en-words zh-words)
;;      (message "中文")
;;       (message "English")
;;         ;; (cons "zh-CN" "en")
;;       ;; (cons "en" "zh-CN")
;;       )
;;     )
;;   )

;; test my little functions

;; test emacs config
;; (require semantic-mode)
;; (semantic-mode 1)
;; (semantic-stickyfunc-mode 1)

;; use-package
;; {{{
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
;; }}}

;; exec-path-from-shell
;; {{{
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))
;; }}}

;; dictionary: Apple 词典: osx-dictionary
;; {{{
;; (require 'osx-dictionary)
(keymap-global-set "C-c d" #'osx-dictionary-search-word-at-point)
;; }}}

;; ace-pinyin
;; {{{
(use-package ace-pinyin
  ;; :defer 1
  :config
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode +1)
  )
;; }}}

;; pyim
;; {{{
(require 'pyim)
(require 'pyim-cregexp-utils)
(require 'pyim-cstring-utils)
;; (require 'pyim-basedict) ; 拼音词库设置
;; (pyim-basedict-enable)   ; 拼音词库
;; (require 'pyim-greatdict)
;; (pyim-greatdict-enable)
(require 'pyim-tsinghua-dict)
(pyim-tsinghua-dict-enable)
(setq default-input-method "pyim")
(setq pyim-page-style 'vertical)
(setq pyim-page-tooltip '(posframe minibuffer popup))
(setq pyim-page-length 9)
(setq-default pyim-punctuation-translate-p '(auto)) ;; 全角半角
(keymap-set pyim-mode-map "-" 'pyim-page-previous-page)
(keymap-set pyim-mode-map "+" 'pyim-page-next-page)
(keymap-global-set "H-e" 'toggle-input-method)
(keymap-global-set "H-b" 'pyim-backward-word)
;; (keymap-set [remap forward-word] 'pyim-forward-word)
;; (keymap-set [remap backward-word] 'pyim-backward-word)
(keymap-global-set "H-f" 'pyim-forward-word)
;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
(keymap-global-set "H-j" #'pyim-convert-string-at-point)
;; 将光标前的 regexp 转换为可以搜索中文的 regexp.
(keymap-set minibuffer-local-map "H-c" #'pyim-cregexp-convert-at-point)
(pyim-default-scheme 'quanpin)
;; (pyim-isearch-mode 1) ;; 开启代码搜索中文功能（比如拼音，五笔码等）
;; cursor
(setq pyim-indicator-list (list #'my-pyim-indicator-with-cursor-color #'pyim-indicator-with-modeline))

(defun my-pyim-indicator-with-cursor-color (input-method chinese-input-p)
  (if (not (equal input-method "pyim"))
      (progn
        ;; 用户在这里定义 pyim 未激活时的光标颜色设置语句
        (set-cursor-color "green"))
    (if chinese-input-p
        (progn
          ;; 用户在这里定义 pyim 输入中文时的光标颜色设置语句
          (set-cursor-color "purple"))
      ;; 用户在这里定义 pyim 输入英文时的光标颜色设置语句
      (set-cursor-color "blue"))))
;; 探针
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                ;; pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))
;; 让 avy 支持拼音搜索
(with-eval-after-load 'avy
  (defun my-avy--regex-candidates (fun regex &optional beg end pred group)
    (let ((regex (pyim-cregexp-build regex)))
      (funcall fun regex beg end pred group)))
  (advice-add 'avy--regex-candidates :around #'my-avy--regex-candidates))
;; 让 vertico, selectrum 等补全框架，通过 orderless 支持拼音搜索候选项功能。
(defun my-orderless-regexp (orig-func component)
  (let ((result (funcall orig-func component)))
    (pyim-cregexp-build result)))
(advice-add 'orderless-regexp :around #'my-orderless-regexp)
;; }}}

;; ;; smart-input-source
;; ;; {{{
;; (use-package sis
;;   :init
;;   ;; `C-s/r' 默认优先使用英文 必须在 sis-global-respect-mode 前配置
;;   (setq sis-respect-go-english-triggers
;;         (list 'isearch-forward 'isearch-backward) ; isearch-forward 时默认进入 en
;;         sis-respect-restore-triggers
;;         (list 'isearch-exit 'isearch-abort))
;;   :config
;;   (sis-ism-lazyman-config
;;    "com.apple.keylayout.ABC"
;;    "com.apple.inputmethod.SCIM.ITABC"
;;    'macism
;;    )
;;   (sis-global-cursor-color-mode t)
;;   (sis-global-respect-mode t)
;;   (sis-global-context-mode t)
;;   (sis-global-inline-mode t)   ; 中文状态下，中文后<spc>切换英文，结束后切回中文
;;   ;; (keymap-global-set "<f9>" #'sis-log-mode) ; 开启日志
;;   ;; 特殊定制
;;   (setq sis-default-cursor-color "green" ;; 英文光标色
;;         sis-other-cursor-color "purple"  ;; 中文光标色 green
;;         ;; sis-inline-tighten-head-rule 'all ; 删除头部空格，默认 1，删除一个空格，1/0/'all
;;         sis-inline-tighten-tail-rule 'all ; 删除尾部空格，默认 1，删除一个空格，1/0/'all
;;         sis-inline-with-english t ; 默认是 t, 中文 context 下输入<spc>进入内联英文
;;         sis-inline-with-other t) ; 默认是 nil，而且 prog-mode 不建议开启, 英文 context 下输入<spc><spc>进行内联中文
;;   ;; 特殊 buffer 禁用 sis 前缀,使用 Emacs 原生快捷键  setqsis-prefix-override-buffer-disable-predicates
;;   (setq sis-prefix-override-buffer-disable-predicates
;;         (list 'minibufferp
;;               (lambda (buffer) ; magit revision magit 的 keymap 是基于 text property 的，优先级比 sis 更高。进入 magit 后，disable sis 的映射
;;                 (sis--string-match-p "^magit-revision:" (buffer-name buffer)))
;;               (lambda (buffer) ; special buffer，所有*打头的 buffer，但是不包括*Scratch* *New, *About GNU 等 buffer
;;                 (and (sis--string-match-p "^\*" (buffer-name buffer))
;;                      (not (sis--string-match-p "^\*About GNU Emacs" (buffer-name buffer))) ; *About GNU Emacs" 仍可使用 C-h/C-x/C-c 前缀
;;                      (not (sis--string-match-p "^\*New" (buffer-name buffer)))
;;                      (not (sis--string-match-p "^\*Scratch" (buffer-name buffer))))))) ; *Scratch*  仍可使用 C-h/C-x/C-c 前缀
;;   )
;; ;; }}}

;; pangu-spacing
;; {{{
(use-package pangu-spacing
  ;; :defer 1
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor t)
  )
;; }}}

;; ispell flyspell: aspell/hunspell
;; (dolist (hook '(text-mode-hook))                          ;; enable in text-mode
;;   (add-hook hook (lambda () (flyspell-mode 1))))
;; (dolist (hook '(change-log-mode-hook log-edit-mode-hook)) ;; disable in change-log-mode
;;   (add-hook hook (lambda () (flyspell-mode -1))))

;; aspell
;; {{{
(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '(
                          "--sug-mode=ultra"
                          "--lang=en_US"
                          "--camel-case"
                          "--run-together"
                          "--run-together-limit=16"
                          ))
;; ispell-personal-dictionary
;; }}}

;; macOS spell
;; {{{
;; ~/Library/Spelling/LocalDictionary
;; }}}

;; wucuo
;; {{{
;; [redguardtoo](https://github.com/redguardtoo/emacs.d/lisp/init-spelling.el)
(defvar my-default-spell-check-language "en_US"
  "Language used by aspell and hunspell CLI.")

(with-eval-after-load 'flyspell
  ;; You can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
  ;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
  (keymap-global-set "C-c s" #'flyspell-auto-correct-word)

  ;; better performance
  (setq flyspell-issue-message-flag nil))

;; flyspell-lazy is outdated and conflicts with latest flyspell

;; Basic Logic Summary:
;; If (aspell is installed) { use aspell}
;; else if (hunspell is installed) { use hunspell }
;; English dictionary is used.
;;
;; I prefer aspell because:
;; - aspell is very stable and easy to install
;; - looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun my-detect-ispell-args (&optional run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words.
RUN-TOGETHER makes aspell less capable to find plain English typo.
So it should be used in `prog-mode-hook' only."
  (let* (args)
    (when ispell-program-name
      (cond
       ;; use aspell
       ((string-match "aspell" ispell-program-name)
        ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
        ;; For aspell's option "--lang", "two letter ISO 3166 country code after a underscore" is OPTIONAL.
        (setq args (list "--sug-mode=ultra" (format "--lang=%s" my-default-spell-check-language)))
        ;; "--run-together-min" could not be 3, see `check` in "speller_impl.cpp".
        ;; The algorithm is not precise.
        ;; Run `echo tasteTableConfig | aspell --lang=en_US -C --run-together-limit=16  --encoding=utf-8 -a` in shell.
        (when run-together
          (cond
           ;; Kevin Atkinson said now aspell supports camel case directly
           ;; https://github.com/redguardtoo/emacs.d/issues/796
           ((string-match "--.*camel-case"
                          (shell-command-to-string (concat ispell-program-name " --help")))
            (setq args (append args '("--camel-case"))))

           ;; old aspell uses "--run-together". Please note we are not dependent on this option
           ;; to check camel case word. wucuo is the final solution. This aspell options is just
           ;; some extra check to speed up the whole process.
           (t
            (setq args (append args '("--run-together" "--run-together-limit=16")))))))

       ;; use hunspell
       ((string-match "hunspell" ispell-program-name)
        (setq args nil))))
    args))

;; Aspell Setup (recommended):
;; It's easy to set up aspell. So the details are skipped.
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/
;;
;; 2. Download openoffice dictionary extension from
;; http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;;
;; 3. Say `dict-en.oxt' is downloaded. Rename it to `dict-en.zip' and unzip
;; the contents to a temporary folder. Got "en_US.dic" and "en_US.aff" in
;; that folder.
;;
;; 4. Hunspell's option "-d en_US" means finding dictionary "en_US"
;; Modify `ispell-local-dictionary-alist' to set that option of hunspell
;;
;; 5. Copy "en_US.dic" and "en_US.aff" from that temporary folder to
;; the place for dictionary files. I use "~/usr_local/share/hunspell/".
;;
;; 6. Add that folder to shell environment variable "DICPATH"
;;
;; 7. Restart emacs so when hunspell is run by ispell/flyspell to make
;; "DICPATH" take effect
;;
;; hunspell searches a dictionary named "en_US" in the path specified by
;; "DICPATH" by default.

(defvar my-force-to-use-hunspell nil
  "Force to use hunspell.  If nil, try to detect aspell&hunspell.")

(defun my-configure-ispell-parameters ()
  "Set `ispell-program-name' and other parameters."
  (cond
   ;; use aspell
   ((and (not my-force-to-use-hunspell) (executable-find "aspell"))
    (setq ispell-program-name "aspell"))

   ;; use hunspell
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary my-default-spell-check-language)
    (setq ispell-local-dictionary-alist
          (list (list my-default-spell-check-language
                      "[[:alpha:]]" "[^[:alpha:]]" "[']"
                      nil
                      (list "-d" my-default-spell-check-language)
                      nil
                      'utf-8)))
    ;; New variable `ispell-hunspell-dictionary-alist' is defined in Emacs
    ;; If it's nil, Emacs tries to automatically set up the dictionaries.
    (when (boundp 'ispell-hunspell-dictionary-alist)
      (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

   (t (setq ispell-program-name nil)
      (message "You need install either aspell or hunspell for ispell"))))

;; You could define your own configuration and call `my-configure-ispell-parameters' in "~/.custom.el"
(my-configure-ispell-parameters)

(defun my-ispell-word-hack (orig-func &rest args)
  "Use Emacs original arguments when calling `ispell-word'.
When fixing a typo, avoid pass camel case option to cli program."
  (let* ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (my-detect-ispell-args))
    (apply orig-func args)
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))
(advice-add 'ispell-word :around #'my-ispell-word-hack)
(advice-add 'flyspell-auto-correct-word :around #'my-ispell-word-hack)

(defvar my-disable-wucuo nil
  "Disable wucuo.")

(defun my-ensure (feature)
  "Make sure FEATURE is required."
  (unless (featurep feature)
    (condition-case nil
        (require feature)
      (error nil))))

(defun text-mode-hook-setup ()
  "Set up text mode."
  ;; Turn off RUN-TOGETHER option when spell check text.
  (unless my-disable-wucuo
    (setq-local ispell-extra-args (my-detect-ispell-args))
    (my-ensure 'wucuo)
    (wucuo-start)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)

(defun my-clean-aspell-dict ()
  "Clean ~/.aspell.pws (dictionary used by aspell)."
  (interactive)
  (let* ((dict (file-truename "~/.aspell.en.pws"))
         (lines (my-read-lines dict))
         ;; sort words
         (aspell-words (sort (cdr lines) 'string<)))
    (save-buffer)
    (sit-for 1)
    (with-temp-file dict
      (insert (format "%s %d\n%s"
                      "personal_ws-1.1 en"
                      (length aspell-words)
                      (mapconcat 'identity aspell-words "\n"))))))
;; }}}

;; wucuo: aspell or hunspell
;; {{{
(with-eval-after-load 'wucuo
  ;; (setq wucuo-aspell-language-to-use "en")
  ;; (setq wucuo-hunspell-dictionary-base-name "en_US")
  ;; do NOT turn on `flyspell-mode' automatically.
  ;; check buffer or visible region only
  ;; spell check buffer every 30 seconds
  (setq wucuo-update-interval 2))

(setq wucuo-spell-check-buffer-predicate
      (lambda ()
        (not (memq
              major-mode
              '(dired-mode
                log-edit-mode
                compilation-mode
                help-mode
                profiler-report-mode
                speedbar-mode
                gud-mode
                calc-mode
                Info-mode)
              )
             )
        )
      )
;; }}}

;; binky-mode
;; {{{

;; }}}

;; yasnippet
;; {{{
(use-package yasnippet
  :config
  (yas-global-mode 1)
  )
;; }}}

;; deadgrep
;; {{{
(use-package deadgrep
  :bind*
  (("C-c r" . deadgrep)
   ("C-c f" . grep-org-files))
  :config
  (defun grep-org-files (words)
    (interactive "sSearch org files: ")
    (let ((default-directory org-roam-directory)
          (deadgrep--file-type '(glob . "*.org"))
          (deadgrep--context '(1 . 1))
          (deadgrep--search-type 'regexp))
      (deadgrep words)
      )
    )
  )
;; }}}

;; khoj
;; {{{
;; Install Khoj Package from MELPA Stable
(use-package khoj
  :bind ("C-c n s" . 'khoj))
;; }}}

;; free-keys
;; {{{
(require 'free-keys)
(setq free-keys-modifiers '(
                            ""
                            ;; "A"
                            "C"
                            "H"
                            "M"
                            "S"
                            "s"
                            ;; "A-C"
                            ;; "A-H"
                            ;; "A-M"
                            ;; "A-S"
                            ;; "A-s"
                            "C-H"
                            "C-M"
                            ;; "C-S"
                            "C-s"
                            ;; "M-S"
                            ;; "M-s"
                            "s-H"
                            ;; "S-s"
                            ;; "C-M-S"
                            ;; "C-M-s"
                            "C-c"
                            "C-x" ))
;; }}}

;; helpful
;; {{{
(use-package helpful
  :bind
  ("C-h f" . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
;; (keymap-global-set "C-h f" #'helpful-callable)
;; (keymap-global-set "C-h v" #'helpful-variable)
;; (keymap-global-set "C-h k" #'helpful-key)
;;
;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
;; (keymap-global-set "C-c C-d" #'helpful-at-point)
;;
;; Look up *F*unctions (excludes macros).
;; (keymap-global-set "C-h F" #'helpful-function)
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
;;
;; Look up *C*ommands.
;; (keymap-global-set "C-h C" #'helpful-command)
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
;;
;; helpful + ivy
;; (setq counsel-describe-function-function #'helpful-callable)
;; (setq counsel-describe-variable-function #'helpful-variable)
;; }}}

;; sticky header: topsy
;; {{{
;; (add-hook 'prog-mode-hook #'topsy-mode)
(use-package topsy
  :hook (prog-mode . topsy-mode)
  )
;; (add-hook 'org-mode-hook #'org-sticky-header-mode)
(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'full)
  )
;; }}}

;; diff-hl
;; {{{
(global-diff-hl-mode)
;; (global-git-gutter-mode +1) ; BUG when deleting folded 17000+lines

;; }}}

;; goggles: visual hint for operations
;; {{{
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing
;; }}}

;; highlight-parentheses
;; {{{
(require 'highlight-parentheses)
;; (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
(global-highlight-parentheses-mode 1)
;; 括号颜色（由内向外）
(setq highlight-parentheses-colors '(
                                     "Green"
                                     "Blue"
                                     "Orange"
                                     "Purple"
                                     "Yellow"
                                     "Red"
                                     ;; "Pink" ;; only six colors supported ?
                                     ))
;; Apple Six Colors
;; (setq highlight-parentheses-colors '("#61BB46" "#FDB827" "#F5821F" "#E03A3E" "#963D97" "#009DDC"))
;;
;; Emacs builtin
;; (setq show-paren-when-point-inside-paren t
;;       show-paren-when-point-in-periphery t)
;; }}}

;; doom-modeline
;; {{{
;; (add-hook 'after-init-hook #'doom-modeline-mode)
;; (setq doom-modeline-support-imenu t)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  :config
  (doom-modeline-mode)
  (setq doom-modeline-height 18)
  (setq doom-modeline-window-width-limit 85)
  (setq find-file-visit-truename t)
  (setq doom-modeline-highlight-modified-buffer-name t)
  (setq doom-modeline-project-detection 'auto) ;auto/project
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  )
;; }}}

;; all-the-icons
;; {{{
;; (use-package all-the-icons :if (display-graphic-p))
(when (display-graphic-p)
  (require 'all-the-icons)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))
;; }}}

;; fold: origami
;; {{{
(add-hook 'prog-mode-hook 'origami-mode)
(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)
  )
;; }}}

;; multiple-cursors
;; {{{
;; multiple-cursors-mode-enabled-hook
;; multiple-cursors-mode-disabled-hook
(use-package multiple-cursors
  :bind (
         ("H-c H-a" . mc/edit-beginnings-of-lines)
         ("H-c H-e" . mc/edit-ends-of-lines)
         ("H-c H-c" . mc/edit-lines)
         ("H-c H-n" . mc/mark-next-like-this)
         ("H-c H-p" . mc/mark-previous-like-this)
         ("H-c H-h" . mc/mark-all-like-this)
         ("H-c H-r" . set-rectangular-region-anchor)
         )
  )

(add-hook 'activate-mark-hook '(lambda ()
                                 (local-set-key
                                  (kbd "C-@")
                                  'set-rectangular-region-anchor)
                                 ))
(add-hook 'deactivate-mark-hook '(lambda ()
                                   (local-unset-key
                                    (kbd "C-@"))
                                   ))
;; }}}

;; expand-region
;; {{{
(keymap-global-unset "C-=")
(keymap-global-unset "C--")
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region))
  )
;; }}}

;; symbol-overlay
;; {{{
(use-package symbol-overlay
  :bind(("M-i"  . symbol-overlay-put)
        ("M-n"  . symbol-overlay-switch-forward)
        ("M-p"  . symbol-overlay-switch-backward)
        ("<f7>" . symbol-overlay-mode)
        ("<f8>" . symbol-overlay-remove-all)
        :map symbol-overlay-map
        ("d" . symbol-overlay-jump-to-definition)
        ("e" . symbol-overlay-echo-mark)
        ("i" . symbol-overlay-put)
        ("n" . symbol-overlay-jump-next)
        ("p" . symbol-overlay-put)
        ("q" . symbol-overlay-query-replace)
        ("r" . symbol-overlay-rename)
        ("s" . symbol-overlay-isearch-literally)
        ("t" . symbol-overlay-toggle-in-scope)
        ("w" . symbol-overlay-save-symbol)
        )
  )
;; (require 'symbol-overlay)
;; (keymap-global-set "M-i"  #'symbol-overlay-put)
;; (keymap-global-set "M-n"  #'symbol-overlay-switch-forward)
;; (keymap-global-set "M-p"  #'symbol-overlay-switch-backward)
;; (keymap-global-set "<f7>" #'symbol-overlay-mode)
;; (keymap-global-set "<f8>" #'symbol-overlay-remove-all)
;; (keymap-set symbol-overlay-mode "d" symbol-overlay-jump-to-definition)
;; (keymap-set symbol-overlay-mode "e" symbol-overlay-echo-mark)
;; (keymap-set symbol-overlay-mode "i" symbol-overlay-put)
;; (keymap-set symbol-overlay-mode "n" symbol-overlay-jump-next)
;; (keymap-set symbol-overlay-mode "p" symbol-overlay-jump-prev)
;; (keymap-set symbol-overlay-mode "q" symbol-overlay-query-replace)
;; (keymap-set symbol-overlay-mode "r" symbol-overlay-rename)
;; (keymap-set symbol-overlay-mode "s" symbol-overlay-isearch-literally)
;; (keymap-set symbol-overlay-mode "t" symbol-overlay-toggle-in-scope)
;; (keymap-set symbol-overlay-mode "w" symbol-overlay-save-symbol)
;; }}}

;; difftastic + magit
;; {{{
;; (with-eval-after-load 'magit
(use-package magit
  ;; :defer 2
  :bind ("C-c v g" . magit-status)
  :config
  (defun my/magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))
      ;; Clear the result buffer (we might regenerate a diff, e.g., for
      ;; the current changes in our working directory).
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))
      ;; Now spawn a process calling the git COMMAND.
      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       ;; Don't query for running processes when emacs is quit.
       :noquery t
       ;; Show the result buffer once the process has finished.
       :sentinel (lambda (proc event)
                   (when (eq (process-status proc) 'exit)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       (ansi-color-apply-on-region (point-min) (point-max))
                       (setq buffer-read-only t)
                       (view-mode)
                       (end-of-line)
                       ;; difftastic diffs are usually 2-column side-by-side,
                       ;; so ensure our window is wide enough.
                       (let ((width (current-column)))
                         (while (zerop (forward-line 1))
                           (end-of-line)
                           (setq width (max (current-column) width)))
                         ;; Add column size of fringes
                         (setq width (+ width
                                        (fringe-columns 'left)
                                        (fringe-columns 'right)))
                         (goto-char (point-min))
                         (pop-to-buffer
                          (current-buffer)
                          `(;; If the buffer is that wide that splitting the frame in
                            ;; two side-by-side windows would result in less than
                            ;; 80 columns left, ensure it's shown at the bottom.
                            ,(when (> 80 (- (frame-width) width))
                               #'display-buffer-at-bottom)
                            (window-width
                             . ,(min width (frame-width))))))))))))
  (defun my/magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If REV is given, just use it.
            (when (boundp 'rev) rev)
            ;; If not invoked with prefix arg, try to guess the REV from
            ;; point's position.
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            ;; Otherwise, query the user.
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (my/magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))
  (defun my/magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If RANGE is given, just use it.
            (when (boundp 'range) range)
            ;; If prefix arg is given, query the user.
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            ;; Otherwise, auto-guess based on position of point, e.g., based on
            ;; if we are in the Staged or Unstaged section.
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(stash . ,value) (error "stash is not yet implemented"))
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (my/magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))
  (transient-define-prefix my/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" my/magit-diff-with-difftastic)
     ("s" "Difftastic Show" my/magit-show-with-difftastic)])
  (transient-append-suffix 'magit-dispatch "!"
    '("#" "My Magit Cmds" my/magit-aux-commands))

  (define-key magit-status-mode-map (kbd "#") #'my/magit-aux-commands)
  )
;; }}}

;; ;; delta + magit + magit-delta
;; ;; {{{
;; ;; https://scripter.co/using-git-delta-with-magit/
;; (use-package magit-delta
;;  :hook (magit-mode . magit-delta-mode)
;;   )
;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
;; ;; }}}

;; package database: epkg + epkgs
;; {{{
(setq epkg-repository "~/Documents/GitHub/epkgs")
(setq package-list-unversioned t) ;; unversioned packages(ibuffer and so on)
;; 怎样快速找到 elpa 目录下那些重复的包 - Emacs China
;; https://emacs-china.org/t/topic/4244
(defun list-packages-and-versions ()
  "Returns a list of all installed packages and their versions"
  (interactive)
  (mapcar
   (lambda (pkg)
     `(,pkg ,(package-desc-version
              (cadr (assq pkg package-alist)))))
   package-activated-list))
;; }}}

;; avy
;; {{{
;; https://karthinks.com/software/avy-can-do-anything
(keymap-global-set "C-;"     #'avy-goto-char)
(keymap-global-set "C-'"     #'avy-goto-char-2)
(keymap-global-set "M-g f"   #'avy-goto-line)
(keymap-global-set "M-g w"   #'avy-goto-word-1)
(keymap-global-set "M-g e"   #'avy-goto-word-0)
(keymap-global-set "C-c C-j" #'avy-resume)
;; }}}

;; consult
;; {{{
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m c" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)           ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)  ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )
;; }}}

;; vertico
;; {{{
(use-package vertico
  :init
  (fido-mode -1)
  (vertico-mode)
  (vertico-mouse-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; ;;  More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))
;; }}}

;; vertico-posframe
;; {{{
(require 'posframe)
(require 'vertico-posframe)
(vertico-posframe-mode 1)
(setq vertico-multiform-commands
      '((consult-line
         posframe
         (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
         (vertico-posframe-border-width . 10)
         ;; NOTE: This is useful when emacs is used in both in X and
         ;; terminal, for posframe do not work well in terminal, so
         ;; vertico-buffer-mode will be used as fallback at the
         ;; moment.
         (vertico-posframe-fallback-mode . vertico-buffer-mode))
        (t posframe)))
(vertico-multiform-mode 1)
(setq vertico-posframe-parameters
      '((left-fringe . 20)
        (right-fringe . 20)))
;; }}}

;; orderless: minibuffer filter, works with icomplete
;; {{{
(use-package orderless
  ;; (setq completion-styles '(orderless basic initials substring partial-completion flex)
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file
                                         (styles basic partial-completion))
                                        )))
;; }}}

;; marginalia: minibuffer annotations
;; {{{
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  ;; :bind (("C-M-a" . marginalia-cycle)
  ;;        :map minibuffer-local-map
  ;;        ("C-M-a" . marginalia-cycle))
  :init ;; The :init configuration is always executed (Not lazy!)
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
;;
;; https://emacs-china.org/t/21-emacs-vertico-orderless-marginalia-embark-consult/19683/
(defun marginalia-annotate-command (cand)
  "Annotate command CAND with its documentation string.
Similar to `marginalia-annotate-symbol', but does not show symbol class."
  (when-let* ((sym (intern-soft cand))
              (mode (if (boundp sym)
                        sym
                      (lookup-minor-mode-from-indicator cand))))
    (concat
     (if (and (boundp mode) (symbol-value mode))
         (propertize " On" 'face 'marginalia-on)
       (propertize " Off" 'face 'marginalia-off))
     (marginalia-annotate-binding cand)
     (marginalia--documentation (marginalia--function-doc sym)))))
;; }}}

;; org-roam: basic config
;; {{{
(use-package org-roam
  ;; :defer 1
  :bind (
         ("C-c n a" . org-roam-alias-add)
         ("C-c n c" . org-roam-capture)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n j" . org-roam-dailies-capture-today) ;; Dailies
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n o" . org-id-get-create)
         ("C-c n t" . org-roam-tag-add)
         )
  :config
  (setq org-roam-completion-everywhere t)
  (setq org-roam-directory "~/org-roam")
  (setq org-roam-db-location "~/org-roam/org-roam.db")
  (setq org-roam-file-extensions '("org" "md")) ;; enable Org-roam for markdown
  ;; (setq org-roam-node-display-template "${title:50} ${tags:30}")
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (require 'org-roam-protocol)  ;; org-roam-protocol
  (org-roam-db-autosync-mode 1) ;; if md-roam installed, move to md-roam config
  )
;; }}}

;; org-roam: filter tags
;; {{{
(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node)))
  )

(defun my/org-roam-list-notes-by-tag (tag-name)
  (interactive)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list)))
  )
;; }}}

;; org-roam: search tag
;; {{{
;; https://emacs-china.org/t/orgmode-tag/21429/15
(defun my/org-roam-node-find-by-tag ()
  (interactive)
  (let ((chosen-tag
         (completing-read "filter by tag: "
                          (seq-uniq
                           (org-roam-db-query
                            [:select [tag]
                                     :from tags ])))))
    (org-roam-node-find
     nil
     nil
     (lambda (node) (member chosen-tag (org-roam-node-tags node))))))
;; }}}

;; org-roam: template,  id (uuid) timestamps and so on
;; {{{
(setq org-roam-capture-templates
      '(
        ;; #+date: %<%Y-%m-%d-%H:%M:%S %Z>\n
        ;; #+date: %<%FT%T%z>\n
        ;; a: audio & music
        ;; A
        ;; B
        ("b" "图书" plain "%?"
         :target (file+head "图书/${slug}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n#+category:\n#+filetags: \n")
         :immediate-finish t
         :unnarrowed  t)
        ;; c:
        ;; C
        ("d" "default" plain "%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n#+category:\n#+filetags:\n")
         :empty-lines 1
         :immediate-finish t
         :unnarrowed  t)
        ;; D
        ("e" "Emacs" plain "%?"
         :target (file+head "Emacs/${slug}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n#+category:\n#+filetags: \n")
         :immediate-finish t
         :unnarrowed  t)
        ;; E
        ;; f:
        ("f" "Emacs Function" plain "%?"
         :target (file+head "Emacs/function/${title}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n#+category:\n#+filetags: \n")
         :immediate-finish t
         :unnarrowed  t)
        ;; F
        ;; g:
        ;; G
        ("h" "人物" plain "%?"
         :target (file+head "topics/人物/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ;; H
        ;; i:
        ;; I
        ;; j:
        ;; J
        ("k" "Emacs keymap" plain "%?"
         :target (file+head "Emacs/keymap/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ;; K
        ;; l:
        ;; L
        ;; m:
        ;; M
        ;; n:
        ;; N
        ;; o:
        ;; O
        ("p" "project" plain "%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n")
         :immediate-finish t
         :unnarrowed t)
        ("P" "Emacs 包/插件" plain "%?"
         :target (file+head "Emacs/package/${title}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n#+filetags: :Emacs:\n")
         :immediate-finish t
         :unnarrowed t)
        ;; q:
        ;; Q
        ("r" "reference" plain "%?"
         :target (file+head "reference/${slug}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n")
         :immediate-finish t
         :unnarrowed t)
        ;; R
        ;; s:
        ("s" "软件" plain "%?"
         :target (file+head "software/${slug}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n")
         :immediate-finish t
         :unnarrowed t)
        ;; S
        ;; t: topic todo
        ("t" "主题" plain "%?"
         :target (file+head "topics/${slug}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n")
         :immediate-finish t
         :unnarrowed t)
        ;; T
        ;; u:
        ;; U
        ("v" "Emacs 变量" plain "%?"
         :target (file+head "Emacs/variable/${title}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n")
         :immediate-finish t
         :unnarrowed t)
        ;; V:
        ;; w:
        ;; W
        ;; x:
        ;; X
        ;; y:
        ;; Y
        ;; z:
        ;; Z
        ))
;; }}}

;; org-roam: UI
;; {{{
;; (custom-set-faces
;;   '((org-roam-link org-roam-link-current)
;;     :foreground "#e24888" :underline t))
;;
;; (defface my-org-id-link '((t :inherit org-link :slant italic))
;;   "Face for org-id links."
;;   :group 'org-faces)
;; }}}

;; org-roam-ui
;; {{{
(use-package org-roam-ui
  :after org-roam
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :bind (("C-c G" . org-roam-ui-open)
         )
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  )
;; }}}

;; little hack
;; {{{
;; brew install --cask db-browser-for-sqlite
(defun my/org-roam-view-db ()
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (shell-command
     ;; net.sourceforge.sqlitebrowser
     (format "open -b \"net.sourceforge.sqlitebrowser\" --args --table nodes %s" org-roam-db-location)))
   (t
    (message "my/org-roam-view-db not yet working on this system-type"))))
;; }}}

;; consult-org-roam
;; {{{
(use-package consult-org-roam
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n F" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n s" . consult-org-roam-search))
;; }}}

;; org-similarity
;; {{{
(use-package org-similarity
  :ensure nil
  :after (org-roam)
  :config
  (with-suppressed-warnings
      (defvaralias
        'org-similarity-directory
        'org-roam-directory)
    )
  (setq org-similarity-language "english")
  (setq org-similarity-number-of-documents 15)
  (setq org-similarity-show-scores t)
  (setq org-similarity-use-id-links t)
  (setq org-similarity-recursive-search t)
  )
;; }}}

;; md-roam
;; {{{
;; https://emacs.stackexchange.com/questions/5465/how-to-migrate-markdown-files-to-emacs-org-mode-format
;; (require 'org-roam)
;; M-x package-vc-install RET https://github.com/nobiot/md-roam.git RET
;; (require 'md-roam)
;; (md-roam-mode 1)           ; md-roam-mode must be active before org-roam-db-sync
;; (setq md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
;; (org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active
;; }}}

;; markdown-mode
;; {{{
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (setq visual-line-column 90)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-wiki-links t) ;; wikilink/backlink
  (setq markdown-wiki-link-search-type "project")
  (setq markdown-enable-math t)
  )
;; }}}

;; RFC
;; {{{
(use-package rfc-mode
  ;; :defer t
  :config
  (setq rfc-mode-directory (expand-file-name "~/Documents/GitHub/RFC-all/txt/"))
  )
;; }}}

;; auto-dark
;; {{{
(use-package auto-dark
  :init (auto-dark-mode t))
;; }}}

;; moom + transient
;; {{{
;; (with-suppressed-warnings '(void-function transient-prefix)) ;FIXME
;; (add-to-list 'warning-suppress-types '((void-function transient-prefix)))
;; (use-package moom
;;   :ensure nil
;;   ;; :if (memq window-system '(mac ns x))
;;   ;; (keymap-global-set "" #'moom-font-increase)
;;   :commands (moom-transient-dispatch moom-transient-config)
;;   :config
;;   (require 'moom-transient)
;;   (setq moom-use-font-module nil)
;;   (moom-mode 1)
;;   (moom-transient-hide-cursor) ;; if needed
;;   ;; (define-key moom-mode-map (kbd "C-c o") #'moom-transient-dispatch)
;;   (moom-recommended-keybindings '(move fit expand fill font reset undo))
;;   )
;;
;; (unless (and (display-graphic-p) (eq system-type 'darwin))
;;   (add-hook 'after-init-hook 'moom-mode)
;;   )
;;
;; (with-eval-after-load "moom"

;;   (when (require 'moom-transient nil t)
;;     (moom-transient-hide-cursor) ;; if needed
;;     (define-key moom-mode-map (kbd "C-c o") #'moom-transient-dispatch)
;;     )
;;   )
;; )
;; }}}

;; {{{ ace-window
;; (require 'ace-window)
(keymap-global-set "H-o" #'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; }}}

;; goto-line-preview
;; {{{
(use-package goto-line-preview
  :bind
  ;; ([remap goto-line] . goto-line-preview)
  ("H-l" . goto-line-preview)
  )
;; }}}

;; graphviz-dot-mode
;; {{{
(setq graphviz-dot-indent-width 4)
(setq graphviz-dot-preview-extension "svg")
;; }}}

;; osm: OpenStreetMap
;; {{{
(use-package osm
  :bind (("C-c m h" . osm-home)
         ("C-c m s" . osm-search)
         ("C-c m v" . osm-server)
         ("C-c m t" . osm-goto)
         ("C-c m x" . osm-gpx-show)
         ("C-c m j" . osm-bookmark-jump))

  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information

  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))
;; }}}

;; elfeed
;; {{{
;; reference: https://github.com/jiacai2050/jiacai2050.github.io/blob/hugo/playground/mu4e-elfeed-config.el
(use-package elfeed
  :defer t
  :ensure nil
  :custom((elfeed-use-curl t)
          (elfeed-db-directory "~/Downloads/elfeed/")
          (elfeed-curl-timeout 20)
          )
  )

;; elfeed-dashboard
(use-package elfeed-dashboard
  :ensure nil
  )

;;elfeed-org
(use-package elfeed-org
  :ensure nil
  :hook (elfeed-dashboard-mode . elfeed-org)
  )
;; }}}

;; shrface eww nov
;; {{{
(use-package shrface
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))
(use-package eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))
(use-package nov
  :defer t
  :mode ("\\.[eE][pP][uU][bB]\\'" . nov-mode)
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  :config
  ((setq nov-text-width 80)
   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
   (add-to-list 'auto-mode-alist '("\\.epub$" . nov-mode))
   (require 'shrface)
   (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
   (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
   )
  )
;; }}}

;; org-mac-link
;; {{{
(use-package org-mac-link
  :ensure nil
  :bind (
         ("H-i" . org-mac-link-get-link)
         )
  )
;; }}}

;; keyfreq: Track Emacs commands frequency
;; {{{
;; keyfreq fork: keyfreq-html-v2 show keyboard heat map
(require 'keyfreq) ;; 导入插件包
(setq keyfreq-folder  (expand-file-name
                       "lib/keyfreq"
                       (concat user-emacs-directory)
                       )
      )
(keyfreq-mode 1)          ;; 启动插件包
(keyfreq-autosave-mode 1) ;; 自动保存模式
(setq-default keyfreq-file (expand-file-name
                       "assets/keyfreq-log"
                       (concat user-emacs-directory)
                       )
	      )
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

;; lsp-bridge
;; {{{
(use-package lsp-bridge
  :ensure nil
  :after (yasnippet)
  ;; :bind
  :hook (prog-mode . lsp-bridge-mode)
  :config
  ;; DON'T setup any of acm keymap                       ; FIXME
  ;; (keymap-set lsp-bridge-mode-map "H-;" 'lsp-bridge-toggle-sdcv-helper)
  ;; (keymap-set lsp-bridge-mode-map "TAB" 'acm-select-next)
  ;; (keymap-set lsp-bridge-mode-map "<backtab>" 'acm-select-prev)
  ;; (keymap-set lsp-bridge-mode-map "SPC" 'acm-insert-common)
  ;; (keymap-set lsp-bridge-mode-map "RET" 'acm-complete)
  ;; (keymap-set lsp-bridge-mode-map "ESC" 'acm-hide )
  ;; (setq acm-quick-access-modifier 'meta)
  ;; (setq acm-enable-quick-access t)
  (setq lsp-bridge-enable-mode-line nil)
  (setq lsp-bridge-use-wenls-in-org-mode t)
  ;; (global-lsp-bridge-mode)
  (add-to-list 'lsp-bridge-org-babel-lang-list "emacs-lisp")
  (add-to-list 'lsp-bridge-org-babel-lang-list "shell")
  )
;; (setq lsp-bridge-enable-mode-line nil)
;; (setq lsp-bridge-use-wenls-in-org-mode t)
;; (run-with-idle-timer
;;  1 nil
;;  #'(lambda ()
;;      (require 'yasnippet)
;;      (yas-global-mode 1)
;;      (require 'lsp-bridge)
;;      (global-lsp-bridge-mode)
;;      ))
;; }}}

;; fuck
;; {{{
(require 'fuck)
(keymap-global-set "H-k" #'fuck)
;; }}}

;; unicode
;; {{{
(require 'modeline-char)
(add-hook 'after-init-hook 'mlc-char-in-mode-line-mode-global)
;; }}}

;; org-auto-tangle
;; {{{
(use-package org-auto-tangle
  :ensure nil
  :hook (org-mode . org-auto-tangle-mode)
  )
;; }}}

;; ts-fold
;; {{{
;; (use-package ts-fold
;;   :ensure nil
;;   )
;; (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode)
;; }}}

;; subed: subtitle edit
;; {{{
(use-package subed
  :ensure nil
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

(use-package nov-xwidget
  :ensure nil
  :demand t
  :after nov
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

;; image-roll
;; {{{
(use-package image-roll
  :ensure nil
  )
;; }}}

;;; init.el ends here
