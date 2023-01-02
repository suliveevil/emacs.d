;; -*- origami-fold-style: triple-braces -*-
;; init

(let (
      (gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil)
      ))

;; test
;; {{{
(add-to-list 'load-path (expand-file-name "~/.config/emacs/bisec"))
;; }}}

;; warning
;; {{{
;; (add-to-list 'warning-suppress-log-types '((defvaralias))) ; FIXME
;; }}}

;; buffer
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

;; chunk
;; {{{
;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000) ;; 64kb
;; }}}

;; cursor move
;; {{{
;; [Emacs一行内移动cursor的最佳方案是什么？ - Emacs China](https://emacs-china.org/t/emacs-cursor/6753/12)
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
      mac-right-command-modifier 'meta    ;; M: Meta (reachable for thumb)
      mac-control-modifier 'control       ;; C: Ctrl
      mac-right-control-modifier 'control ;; C: Ctrl
      mac-option-modifier  'meta          ;; M: Meta (Option/Alt)
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

;; cursor
;; {{{
;; make cursor the width of the character it is under
;; i.e. full width of a TAB
(setq x-stretch-cursor t)
;; cursor line: 光标所在行显示/高亮
(global-hl-line-mode t) ;; highlight current line
(custom-set-faces '(hl-line ((t (:background "grey")))))
;; }}}

;; line: wrap/truncate
;; {{{
(setq word-wrap-by-category t) ;; improves CJK + Latin word-wrapping
(setq scroll-margin 5)
(global-display-line-numbers-mode 1)
(setq global-display-line-numbers-width-start t)
(setq display-line-numbers-grow-only t)    ;; do not shrink line number width
(setq display-line-numbers-type 'relative) ;; 相对行号
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
;; }}}

;; fold
;; {{{
(add-hook 'prog-mode 'hs-minor-mode)
(add-to-list 'hs-special-modes-alist
             '(emacs-lisp-mode "{" "}" ";;" nil nil))
(keymap-global-set "C-c TAB" #'hs-toggle-hiding)
(keymap-global-set "M-+" #'hs-show-all)
;; }}}


;; completion: dabbrev: dynamic abbreviation expand
;; {{{
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
;; (ido-mode 1)
;; (setq ido-vertical-mode t)
;; }}}

;; isearch
;; {{{
;; M-<: first match
;; M->: last  match
(setq isearch-lazy-count t) ;; anzu
;; }}}

;; org-mode
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
;; code block: TAB 格式化
(setq org-src-fontify-natively 1)         ;代码块语法高亮
(setq org-src-tab-acts-natively 1)        ;开启代码块语法缩进
(setq org-edit-src-content-indentation 0) ;代码块初始缩进范围
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (awk . t)
   ;; (c   .  t)
   (calc   .  t)
   (comint   .  t)
   (css   .  t)
   (dot . t)
   (emacs-lisp   .  t)
   (eshell   .  t)
   (haskell . t)
   (js   .  t)
   (latex . t)
   (lua   .  t)
   (org   .  t)
   (perl   .  t)
   (plantuml   .  t)
   (python . t)
   (ruby . t)
   (sed   .  t)
   (shell . t)
   (sql   .  t)
   (sqlite . t)
   ))
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
(setq package-archives '(
                         ("elpa"                . "https://elpa.gnu.org/packages/")
                         ("elpa-devel"          . "https://elpa.gnu.org/devel/")
                         ("jcs-elpa"            . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                         ("gnu"                 . "http://elpa.gnu.org/packages/")
                         ("gnu-devel"           . "https://elpa.gnu.org/devel/")
                         ("gnu-tsinghua"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("gnu-ustc"            . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa"               . "http://melpa.org/packages/")
                         ("melpa-stable"        . "https://stable.melpa.org/packages/")
                         ("melpa-tsinghua"      . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("melpa-ustc"          . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu"              . "https://elpa.nongnu.org/nongnu/")
                         ("nongnu-devel"        . "https://elpa.nongnu.org/devel/")
                         ("nongnu-ustc"         . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ))
(setq package-archive-priorities '(
                                   ("elpa"                       . 22)
                                   ("nongnu"                     . 21)
                                   ("gnu"                        . 17)
                                   ("gnu-devel"                  . 18)
                                   ("gnu-tsinghua"               . 50)
                                   ("gnu-ustc"                   . 49)
                                   ("melpa"                      . 51)
                                   ("melpa-stable"               . 14)
                                   ("melpa-tsinghua"             . 48)
                                   ("melpa-ustc"                 . 47)
                                   ("nongnu"                     . 10)
                                   ("nongnu-devel"               . 11)
                                   ("nongnu-ustc"                . 46)
                                   ("jcs-elpa"                   . 7)
                                   ))
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

;; exec-path-from-shell
;; {{{
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))
;; }}}

;; elisp-demos
;; {{{
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
;; }}}

;; fuck
;; {{{
(use-package fuck
  :defer 2
  )
;; }}}

;; dictionary: Apple 词典: osx-dictionary
;; {{{
;; (require 'osx-dictionary)
(keymap-global-set "C-c d" #'osx-dictionary-search-word-at-point)
;; }}}

;; Siri Shortcuts: OCR
;; {{{
(defun my/siri-ocr ()
  (interactive)
  (shell-command "shortcuts run \"OCR Selected Area\"")
  (do-applescript "tell application id \"org.gnu.Emacs\" to activate")
  )
(keymap-global-set "C-c M-o" #'my/siri-ocr)
;; }}}

;; Siri Shortcuts: Translate
;; {{{
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
(defun my/siri-translate ()
  (interactive)
  (let ((tempfile
         (make-temp-file "siri-translate-" nil ".txt") ; temp file
         ))
    (write-region (format "%s" (thing-at-point 'paragraph)) nil tempfile)
    (end-of-paragraph-text)
    (shell-command (format "shortcuts run \"Translate File\" -i %s &" tempfile))
    )
  (do-applescript "tell application id \"org.gnu.Emacs\" to activate")
  )
(keymap-global-set "C-c t" #'my/siri-translate)
;; }}}

;; khoj
;; {{{
;; Install Khoj Package from MELPA Stable
(use-package khoj
  :ensure t
  :bind ("C-c s" . 'khoj))
;; }}}

;; MacVim
;; {{{
;; }}}

;; Neovide
;; {{{
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
(keymap-set global-map "C-c c" #'my/open-in-vscode)
;; }}}

;; Obsidian
;; {{{
;; https://emacs-china.org/t/emacs-obsidian/22504/11?u=suliveevil
(defun my/open-in-obsidian () ;; 在 Obsidian 中打开当前 Emacs 正在编辑的文件
  (interactive)
  (browse-url-xdg-open
   (concat "obsidian://open?path=" (url-hexify-string (buffer-file-name)))))
;; doom emacs 中的按键绑定， SPC-f-o
;; (map! :leader
;;       :desc "open current file in obsidian"
;;       "f o" #'open-current-file-in-obsidian)
;; }}}

;; pyim
;; {{{
(require 'pyim)
(require 'pyim-cregexp-utils)
(require 'pyim-cstring-utils)
(keymap-global-set "M-f" 'pyim-forward-word)
(keymap-global-set "M-b" 'pyim-backward-word)
;; (require 'pyim-basedict) ; 拼音词库设置
;; (pyim-basedict-enable)   ; 拼音词库
;; (require 'pyim-greatdict)
;; (pyim-greatdict-enable)
(require 'pyim-tsinghua-dict)
(pyim-tsinghua-dict-enable)
(setq default-input-method "pyim")
(setq pyim-page-tooltip '(posframe popup minibuffer))
(setq pyim-page-length 9)
(setq-default pyim-punctuation-translate-p '(auto)) ;; 全角半角
;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
;; (global-set-key (kbd "M-j") 'pyim-convert-string-at-point)
;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
;; (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)
(pyim-default-scheme 'quanpin)
(pyim-isearch-mode 1) ;; 开启代码搜索中文功能（比如拼音，五笔码等）
;; 让 vertico, selectrum 等补全框架，通过 orderless 支持拼音搜索候选项功能。
(defun my-orderless-regexp (orig-func component)
  (let ((result (funcall orig-func component)))
    (pyim-cregexp-build result)))
(advice-add 'orderless-regexp :around #'my-orderless-regexp)
;; }}}

;; ace-pinyin
;; {{{
(require 'ace-pinyin)
(setq ace-pinyin-use-avy t)
(ace-pinyin-global-mode +1)
;; }}}

;; helpful
;; {{{
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(keymap-global-set "C-h f" #'helpful-callable)
(keymap-global-set "C-h v" #'helpful-variable)
(keymap-global-set "C-h k" #'helpful-key)
;;
;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(keymap-global-set "C-c C-d" #'helpful-at-point)
;;
;; Look up *F*unctions (excludes macros).
(keymap-global-set "C-h F" #'helpful-function)
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
;;
;; Look up *C*ommands.
(keymap-global-set "C-h C" #'helpful-command)
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
;;
;; helpful + ivy
;; (setq counsel-describe-function-function #'helpful-callable)
;; (setq counsel-describe-variable-function #'helpful-variable)
;; }}}

;; moom
;; {{{
(add-hook 'after-init-hook 'moom-mode)
;; moom + transient
(with-eval-after-load "moom"
  (setq moom-use-font-module nil)
  (when (require 'moom-transient nil t)
    (moom-transient-hide-cursor) ;; if needed
    (define-key moom-mode-map (kbd "C-c o") #'moom-transient-dispatch)
    )
  )
;; }}}

;; {{{ ace-window
;; (require 'ace-window)
(keymap-global-set "M-o" #'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; }}}

;; org-auto-tangle
;; {{{
(require 'org-auto-tangle)
(add-hook 'org-mode-hook 'org-auto-tangle-mode)
;; }}}

;; magit + git-gutter
;; {{{
(global-git-gutter-mode +1)
;; }}}

;; difftastic + magit
;; {{{
;; (with-eval-after-load 'magit
(use-package magit
  :defer 2
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

;; delta + magit + magit-delta
;; {{{
;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
;; }}}

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

;; package config
;; {{{
;; (add-to-list 'load-path (expand-file-name "init-package.el"  (concat user-emacs-directory))) ;; :FIXME:
;; (add-to-list 'load-path "~/.config/emacs/init-package.el")
;; (require 'init-package) ;; packages installed by package.el
;; }}}

;; vertico
;; {{{
(use-package vertico
  :init
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
(require 'orderless)
(setq completion-styles '(orderless basic initials substring partial-completion flex)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))
;; }}}

;; marginalia: minibuffer annotations
;; {{{
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("C-M-a" . marginalia-cycle)
         :map minibuffer-local-map
         ("C-M-a" . marginalia-cycle))
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

;; org-roam: basic config
;; {{{
(use-package org-roam
  :ensure t
  :defer 1
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
  (setq org-roam-node-display-template "${title:50} ${tags:30}")
  (require 'org-roam-protocol)  ;; org-roam-protocol
  (org-roam-db-autosync-mode 1) ;; if md-roam installed, move to md-roam config
  )
;; }}}

;; org-roam: directory
;; {{{
;; }}}

;; org-roam: node directory                                                       ; FIXME
;; {{{
;; (with-eval-after-load 'org-roam
;;   (cl-defmethod org-roam-node-directories ((node org-roam-node))
;;     (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;         (format "(%s)" (car (split-string dirs "/")))
;;       ""))
;;   (setq org-roam-node-display-template
;;         "${title:30} ${tags:30} ${directories:15}")
;;   )
;; }}}

;; org-roam: backlink count & node hierarchy
;; {{{
;; ;; https://github.com/Jousimies/.emacs.d/blob/master/lisp/init-roam.el
;; (require 'org)
;; (require 'org-roam)
;; ;;
;; (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
;;   (let* ((count (caar (org-roam-db-query
;;                        [:select (funcall count source)
;;                                 :from links
;;                                 :where (= dest $s1)
;;                                 :and (= type "id")]
;;                        (org-roam-node-id node)))))
;;     (format "[%d]" count)))
;; ;;
;; ;;   (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
;; ;;     "Return the file TITLE for the node."
;; ;;     (if-let ((file (org-roam-node-file node)))
;; ;;         (with-temp-buffer
;; ;;           (insert-file-contents file nil 0 1024)
;; ;;           (cadr (assoc "TITLE"
;; ;;                        (org-collect-keywords (list "TITLE")))))
;; ;;       (cadr (assoc "TITLE"
;; ;;                    (org-collect-keywords (list "TITLE"))))))

;; ;; (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
;; ;;   "Return the hierarchy for the node."
;; ;;   (let ((title (org-roam-node-title node))
;; ;;         (olp (org-roam-node-olp node))
;; ;;         (level (org-roam-node-level node))
;; ;;         (filetitle (org-roam-node-filetitle node)))
;; ;;     (concat
;; ;;      (if (> level 0) (concat filetitle " > "))
;; ;;      (if (> level 1) (concat (string-join olp " > ") " > "))
;; ;;      title))
;; ;;   )
;; ;;
;; (setq org-roam-node-display-template
;;       "${title:30} ${backlinkscount:5} ${tags:30} ${directories:15}")
;; }}}

;; org-roam: completion
;; {{{
;;roam links support auto-completion via completion-at-point
;; call M-x completion-at-point within a roam link.
;; Where the | character represents the cursor:
;; [[|]] : completes for a file title
;; [[roam:]] : completes for a file title
;; [[*|]] : completes for a headline within this file
;; [[foo*|]] : completes a headline within the file with title “foo”
;; [[roam:foo*|]] completes a headline within the file with title “foo”
;; }}}

;; org-roam: slug (called by org-roam-capture-templates)
;; {{{
;; (cl-defmethod org-roam-node-slug ((node org-roam-node))
;;   "Return the slug of NODE."
;;   (let ((title (org-roam-node-title node))
;;         (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
;;                            768 ; U+0300 COMBINING GRAVE ACCENT
;;                            769 ; U+0301 COMBINING ACUTE ACCENT
;;                            770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
;;                            771 ; U+0303 COMBINING TILDE
;;                            772 ; U+0304 COMBINING MACRON
;;                            774 ; U+0306 COMBINING BREVE
;;                            775 ; U+0307 COMBINING DOT ABOVE
;;                            776 ; U+0308 COMBINING DIAERESIS
;;                            777 ; U+0309 COMBINING HOOK ABOVE
;;                            778 ; U+030A COMBINING RING ABOVE
;;                            780 ; U+030C COMBINING CARON
;;                            795 ; U+031B COMBINING HORN
;;                            803 ; U+0323 COMBINING DOT BELOW
;;                            804 ; U+0324 COMBINING DIAERESIS BELOW
;;                            805 ; U+0325 COMBINING RING BELOW
;;                            807 ; U+0327 COMBINING CEDILLA
;;                            813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
;;                            814 ; U+032E COMBINING BREVE BELOW
;;                            816 ; U+0330 COMBINING TILDE BELOW
;;                            817 ; U+0331 COMBINING MACRON BELOW
;;                            )))
;;     (cl-flet* ((nonspacing-mark-p (char)
;;                                   (memq char slug-trim-chars))
;;                (strip-nonspacing-marks (s)
;;                                        (ucs-normalize-NFC-string
;;                                         (apply #'string (seq-remove #'nonspacing-mark-p
;;                                                                     (ucs-normalize-NFD-string s)))))
;;                (cl-replace (title pair)
;;                            (replace-regexp-in-string (car pair) (cdr pair) title)))
;;       (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
;;                       ("--*" . "-")                   ;; remove sequential underscores
;;                       ("^-" . "")                     ;; remove starting underscore
;;                       ("-$" . "")))                   ;; remove ending underscore
;;              (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
;;         (downcase slug)))))
;; }}}

;; org-roam: filter tags
;; {{{
(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))
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
        ;; ("bz" "Z综合性图书" plain "%?"
        ;;  :target (file+head "图书/Z综合性图书/${title}.org"
        ;;                     "#+title: ${title}\n#+date: %<%FT%T%z>\n#+category:\n#+filetags: \n")
        ;;  :immediate-finish t
        ;;  :unnarrowed  t)
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
        ;; h: human
        ;; H
        ;; i:
        ;; I
        ;; j:
        ;; J
        ;; k:
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

;; org-similarity
;; {{{
(use-package org-similarity
  :after org-roam
  :config
  (with-suppressed-warnings (defvaralias 'org-similarity-directory 'org-roam-directory))
  (setq org-similarity-language "english")
  (setq org-similarity-number-of-documents 15)
  (setq org-similarity-show-scores t)
  (setq org-similarity-use-id-links t)
  (setq org-similarity-recursive-search t)
  )
;; }}}

;; markdown-mode
;; {{{
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mk/d\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(setq markdown-command "multimarkdown")
(setq markdown-enable-wiki-links t) ;; wikilink/backlink
(setq markdown-wiki-link-search-type "project")
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

;; goggles: visual hint for operations
;; {{{
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing
;; }}}

;; doom-modeline
;; {{{
(add-hook 'after-init-hook #'doom-modeline-mode)
;; }}}

;; package out of package.el :FIXME:
;; {{{
;; (add-to-list 'load-path (expand-file-name "init-lib.el" user-emacs-directory)) ;; :FIXME:
;; (add-to-list 'load-path "~/.config/emacs/init-lib.el")
;; (require 'init-lib)     ;; packages out of package.el
;; }}}

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
                            ;; "C-H"
                            "C-M"
                            ;; "C-S"
                            ;; "C-s"
                            ;; "M-S"
                            ;; "M-s"
                            ;; "s-H"
                            ;; "S-s"
                            ;; "C-M-S"
                            ;; "C-M-s"
                            "C-c C"
                            "C-x C" ))
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

;; sticky header: topsy
;; {{{
;; (require 'topsy)
(add-hook 'prog-mode-hook #'topsy-mode)
;; (require 'org-sticky-header)
(add-hook 'org-mode-hook #'org-sticky-header-mode)
;; }}}

;; graphviz-dot-mode
;; {{{
(setq graphviz-dot-indent-width 4)
(setq graphviz-dot-preview-extension "svg")
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
;; }}}

;; diagram-preview
;; {{{

;; }}}

;; RFC
;; {{{
(use-package rfc-mode
  :defer t
  :config
  (setq rfc-mode-directory (expand-file-name "~/Documents/GitHub/RFC-all/txt/"))
  )
;; }}}

;; unicode
;; {{{
(require 'modeline-char)
(add-hook 'after-init-hook 'mlc-char-in-mode-line-mode-global)
;; }}}

;; all-the-icons
;; {{{
(when (display-graphic-p)
  (require 'all-the-icons))
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
;; }}}

;; olivetti
;; {{{
;; https://emacs-china.org/t/emacs/19797/4
(use-package olivetti
  :diminish
  :bind ("<f8>" . olivetti-mode)
  :init
  (setq olivetti-body-width 0.618)	; default: fill-column+2
  (defun xs-toggle-olivetti-for-org ()
    "if current buffer is org and only one visible buffer
  enable olivetti mode"
    (if (and (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
             (or (eq (length (window-list nil nil nil)) 1)
                 (window-at-side-p (frame-first-window) 'right))) ;; frame-first-window 的 mode 是 org-mode 并且没有右边 window
        (olivetti-mode 1)
      (olivetti-mode 0)
      (when (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
        (visual-line-mode 1))))
  (add-hook 'org-mode-hook #'xs-toggle-olivetti-for-org)
  (add-hook 'window-configuration-change-hook #'xs-toggle-olivetti-for-org))
;; }}}

;; osm
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

;; lsp-bridge
;; {{{
(use-package yasnippet
  :defer 1
  :config
  (yas-global-mode 1)
  )

(use-package lsp-bridge
  :defer 1
  :after yasnippet
  :config
  (global-lsp-bridge-mode)
  )
;; }}}

;; init.el
