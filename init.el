;; -*- coding: utf-8; lexical-binding: t; -*-
;; -*- origami-fold-style: triple-braces -*-
;;; init.el

;; Date: 2023-01-06T12:52:58+0800

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
(global-subword-mode)     ;; camelCase and superword-mode
;; }}}

(put 'narrow-to-region 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
;; (put 'list-timers 'disabled nil)

(setq
 scroll-step 1
 scroll-margin 5
 scroll-conservatively 10000
 )

(add-hook 'emacs-lisp-mode-hook 'turn-off-auto-fill)

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

;; use-package
;; {{{
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package)
;;   (eval-when-compile (require 'use-package)))
;; }}}

(use-package benchmark-init
  :ensure nil
  :init (benchmark-init/activate)
  ;; To disable collection of benchmark data after init is done.
  :hook (after-init . benchmark-init/deactivate)
  ;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
  )

;; (add-hook 'after-init-hook
;;           (lambda () (message "loaded in %s" (emacs-init-time))))

;; tree-sitter
;; {{{
;; Use the built-in treesit and load all language grammars
(use-package treesit
  :ensure nil
  :defer 1
  :custom
  ;; Load languages directly from the repository after making them
  (treesit-extra-load-path '("~/.config/emacs/tree-sitter/"))
  :config
  ;; Replace relevant modes with the treesitter variant
  (dolist (mode
           '(
             (bash-mode       . bash-ts-mode)
             (c-mode          . c-ts-mode)
             (cmake-mode      . cmake-ts-mode)
             (conf-toml-mode  . toml-ts-mode)
             (csharp-mode     . csharp-ts-mode)
             (css-mode        . css-ts-mode)
             (dockerfile-mode . dockerfile-ts-mode)
             (java-mode       . java-ts-mode)
             (javascript-mode . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (js-mode         . js-ts-mode)
             (python-mode     . python-ts-mode)
             (ruby-mode       . ruby-ts-mode)
             (sh-mode         . bash-ts-mode)
             (typescript-mode . typescript-ts-mode)
             ;; (c++-mode        . c++-ts-mode) ; FIXME
             ;; (go-mode         . go-ts-mode) ; FIXME
             ))
    (add-to-list 'major-mode-remap-alist mode)
    )
  )
;; }}}

;; random function
;; {{{
(defun describe-random-interactive-function ()
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

;; time
;; {{{
(use-package iso8601
  :ensure nil
  :defer t
  :bind
  ("C-c D" . my/date-and-time-iso8601)
  :config
  (defun my/date-and-time-iso8601 ()
    (interactive)
    (insert (format-time-string "%FT%T%z"))
    )
  )
;; }}}

;; font and syntax
;; {{{
(set-face-attribute 'default nil
                    :family "Sarasa Mono SC Nerd"
                    :height 140 ; 更改显示字体大小
                    )
(global-font-lock-mode t) ;; turn on syntax highlighting for all buffers
;; }}}

(use-package emacs
  :ensure nil
  :bind
  ("C-c H-k" . yank-from-kill-ring)
  ("M-z" . zap-up-to-char)
  )

;; comment
;; {{{
(use-package emacs
  :ensure nil
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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq enable-recursive-minibuffers t)
  (setq history-length 1024)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables '(
                                        extended-command-history
                                        global-mark-ring
                                        mark-ring
                                        regexp-search-ring
                                        search-ring
                                        ))
  (setq savehist-autosave-interval 300)
  )

(use-package recentf
  :ensure nil
  :defer 1
  ;; :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 256)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup 'never)
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-exclude `(,@(cl-loop for f in `(,package-user-dir
                                           ;; ,no-littering-var-directory
                                           ;; ,no-littering-etc-directory
                                           )
                                collect (abbreviate-file-name f))
                     ;; Folders on macOS start
                     "^/private/tmp/"
                     "^/var/folders/"
                     ;; Folders on macOS end
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "^/tmp/"
                     "/ssh\\(x\\)?:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")
                   )
  )

;; 自动记住每个文件的最后一次访问的光标位置
(use-package saveplace
  :ensure nil
  ;; :defer 1
  :hook (after-init . save-place-mode)
  )

;; warn when opening files bigger than 100 MB
(setq large-file-warning-threshold (* 100 1000 1000))

;; 使 Emacs 自动加载外部修改过的文件
;; (global-auto-revert-mode 1)
(add-hook 'on-first-file-hook 'global-auto-revert-mode)

;; Open file system read-only files as read-only in Emacs as well.
(setq view-read-only t)

;; chunk
;; {{{
;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000) ;; 64kb
;; }}}

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

;; move file to trash when delete
;; {{{
;;; macOS
(when (eq system-type 'darwin)
  (setq trash-directory "~/.Trash/")
  (setq delete-by-moving-to-trash t))
;; }}}

;; dired
;; {{{
(use-package dired
  :ensure nil
  :bind
  (
   :map dired-mode-map
   ("RET" . dired-open-dwim)
   )
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always) ; 全部递归拷贝、删除文件夹中的文件
  (setq dired-use-ls-dired t)
  (setq dired-auto-revert-buffer t)
  ;; (dired-listing-switches "-alGh")
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

  (defun dired-open-dwim ()
    (interactive)
    (if (file-directory-p (dired-file-name-at-point))
        (dired-find-file)
      (dired-find-file-other-window)))
  )
;; }}}

;; project
;; {{{
(use-package project
  :ensure nil
  :defer 2
  ;; :bind
  ;; ("C-c p" . project-prefix-map)
  )
;; }}}

;; kill buffer
;; {{{
(use-package emacs
  :ensure nil
  :bind
  ("C-c K" . my/kill-all-other-buffers)
  :config
  (defun my/kill-all-other-buffers ()
    (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
    )
  )
;; }}}

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

;; line
;; {{{
(use-package display-line-numbers
  :ensure nil
  :hook
  (after-init . global-display-line-numbers-mode)
  :bind
  ("C-c O"   . open-newline-above)
  ("C-c C-o" . open-newline-below)
  :config
  ;; (global-display-line-numbers-mode 1)

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

;; display-fill-column-indicator
;; {{{
(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (after-init . global-display-fill-column-indicator-mode)
  ;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
  :bind
  (
   ([remap fill-paragraph] . my/toggle-fill-unfill)
   )
  :config
  (setq-default fill-column 80) ;; M-x set-fill-column RET

  (defun my/toggle-fill-unfill ()
    "Like `fill-paragraph', but unfill if used twice."
    (interactive)
    (let ((fill-column
           (if (eq last-command 'my-fill-or-unfill)
               (progn (setq this-command nil)
                      (point-max))
             fill-column)))
      (call-interactively 'fill-paragraph nil (vector nil t))))

  ;; https://www.emacswiki.org/emacs/UnfillParagraph
  (defun my/unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region))
    )
  )
;; }}}

;; sentence: 断句
;; {{{
(setq sentence-end
      "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      )
;; (setq sentence-end-double-space nil)
;; }}}

;; minibuffer
;; {{{
(use-package minibuffer
  :ensure nil
  :bind
  (
   :map minibuffer-mode-map
   ("H-j" . next-line)
   ("H-k" . previous-line)
   ;; ("TAB" . minibuffer-complete)
   
   ;; :map minibuffer-local-map
   ("C-n" . minibuffer-previous-completion)
   ("C-p" . minibuffer-next-completion)
   ;; ("C-<tab>" . dabbrev-expand)
   
   :map completion-in-region-mode-map
   ("C-n" . minibuffer-previous-completion)
   ("C-p" . minibuffer-next-completion)
   )
  :config
  (setq history-delete-duplicates t)
  )

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
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 20)
(setq completion-auto-select nil)
(setq enable-recursive-minibuffers t)
(setq completion-auto-help 'always)
(setq completion-auto-select 'second-tab)
;; }}}

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

;; abbrev/dabbrev: dynamic abbreviation expand
;; {{{
(use-package dabbrev
  :ensure nil
  :bind
  ( "C-<tab>" . dabbrev-expand)
  ;; :config
  )
;; }}}

;; hippie-expand
;; {{{
(use-package hippie-exp
  :ensure nil
  :bind
  ([remap dabbrev-expand] . hippie-expand)
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

;; unicode
;; {{{
;; https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
(when (file-exists-p "~/.config/emacs/assets/unicode/UnicodeData.txt")
  (setq describe-char-unicodedata-file
        "~/.config/emacs/assets/unicode/UnicodeData.txt")
  )
;; }}}

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

(use-package emacs
  :ensure nil
  :bind
  ("H-SPC H-SPC" . (lambda () (interactive) (insert "\u200b")))
  )

;; pair completion
(use-package electric-pair-mode ; elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  )

;; additionally to the list defined in title-capitalization:
(defvar my-do-not-capitalize-words '("suliveevil")
  "Personal list of words that doesn't get capitalized in titles.")

(defun text-case-title-capitalization (beg end)
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
           ;; if user has defined 'my-do-not-capitalize-words, append to basic list:
           (do-not-capitalize-words (if (boundp 'my-do-not-capitalize-words)
                                        (append do-not-capitalize-basic-words my-do-not-capitalize-words )
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

;; goto-char by Oliver Scholz
;; {{{
(use-package emacs
  :ensure nil
  :bind
  ("C-c g a" . my/go-to-char)
  :config
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
  )

;; similar work
;; https://www.emacswiki.org/emacs/go-to-char.el
;; https://www.emacswiki.org/emacs/joseph-go-to-char
;; doitian/iy-go-to-char: Go to next CHAR which is similar to "f" and "t" in vim
;; https://github.com/doitian/iy-go-to-char
;; }}}

;; isearch
;; {{{
;; M-<: first match
;; M->: last  match
(use-package isearch
  :ensure nil
  :defer 1
  :bind
  (
   :map isearch-mode-map
   ("C-c" . isearch-cancel)
   ("DEL" . isearch-del-char)
   ("s-v" . isearch-yank-kill)
   )
  :config
  (setq isearch-lazy-count t) ;; anzu
  (setq isearch-allow-motion t)
  ;; 这样可以在 literal 的 isearch 中，把空格直接当成正则里面的 .* 匹配
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace t)
  (setq search-whitespace-regexp ".*")
  (setq isearch-regexp-lax-whitespace nil) ; 正则搜索时不开启这个功能，空格就是空格
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

(use-package diff-mode
  :ensure nil
  :defer t
  )

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

;; ido
;; {{{
(use-package ido
  :ensure nil
  :defer 1
  :config
  (setq ido-vertical-mode t)
  (setq ido-enable-flex-matching t)
  )
;; }}}

(use-package eshell
  :ensure nil
  :bind
  (
   ("C-x s" . eshell)
   ;; :map eshell-mode-map
   ;; (
   ;;("C-l" . eshell-clear)
   ;; ("C-r" . eshell-history)
   ;; )
   )
  :config
  (require 'esh-mode) ; eshell-mode-map
  )

;; frame
;; {{{
(setq frame-size-history t)
(setq frame-title-format
      '(buffer-file-name (:eval (abbreviate-file-name buffer-file-name))
                         (dired-directory dired-directory "%b")))
;; }}}

(defun my/toggle-fullscreen ()
  (interactive)
  (set-frame-parameter
   nil
   'fullscreen
   (if (frame-parameter nil 'fullscreen)
       nil
     'fullboth))
  )

;; window
;; {{{
(use-package emacs
  :ensure nil
  :bind
  ("C-c C-w" . my/toggle-one-window)
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

(use-package emacs
  :ensure nil
  :bind
  ("H-w H-w" . my-toggle-vertical-horizontal-split)
  :config
  (defun my-toggle-vertical-horizontal-split ()
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
                 (neighbour2 (if next-win (with-selected-window
					      next-win
                                            (windmove-find-other-window
					     neighbour-dir
					     next-win))
			       )))
            ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
            (setq done (and (eq neighbour1 neighbour2)
                            (not (eq (minibuffer-window) next-win))))
            (if done
                (let* ((other-buf (window-buffer next-win)))
                  (delete-window next-win)
                  (if (eq nextdir 'right)
                      (split-window-vertically)
                    (split-window-horizontally))
                  (set-window-buffer
		   (windmove-find-other-window neighbour-dir)
		   other-buf)))
	    ))))))

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

(defun my/webkit-open-local-file (fpath)
  (interactive "fEnter file path: ")
  (when (member (substring fpath -4 nil) '("html" ".pdf" ".mp4"))
    (xwidget-webkit-browse-url
     (concat "file://" (expand-file-name fpath)))
    )
  )

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

;; Obsidian
;; {{{
;; https://emacs-china.org/t/emacs-obsidian/22504/11?u=suliveevil
(defun my/open-in-obsidian () ;; 在 Obsidian 中打开当前 Emacs 正在编辑的文件
  (interactive)
  (browse-url-xdg-open
   (concat "obsidian://open?path=" (url-hexify-string (buffer-file-name)))))
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

(use-package org
  :ensure nil
  :defer 1
  :bind
  (
   :map org-mode-map
   ("C-c l"   . org-store-link) ; C-c C-l org-insert-link
   ("C-c n o" . org-id-get-create)
   ("C-c H-i" . org-insert-structure-template)
   ("C-c H-t" . my/sparse-tree-with-tag-filter)
   )
  :config
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

;; (setq org-hide-leading-stars t) ; Omit headline-asterisks except the last one
(setq org-src-fontify-natively t)  ; code block syntax highlight

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

(use-package org-src
  :ensure nil
  :defer t
  :config
  (setq org-src-fontify-natively 1)         ;代码块语法高亮
  (setq org-src-tab-acts-natively 1)        ;开启代码块语法缩进/格式化
  (setq org-edit-src-content-indentation 0) ;代码块初始缩进范围
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

(defadvice org-toggle-inline-images (after org-open-at-point activate)
  (if smooth-scrolling-mode (smooth-scrolling-mode -1)
        (smooth-scrolling-mode 1)))

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

(use-package org-capture
  :ensure nil
  :bind
  ("\e\e c" . (lambda () (interactive) (org-capture)))
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
                            "* Today's TODO list [/]\n%T\n\n** TODO %?"
                            :empty-lines 1
                            :jump-to-captured t)
                           ("do" "Other stuff"
                            entry (file+olp+datetree "diary.org")
                            "* %?\n%T\n\n%i"
                            :empty-lines 1
                            :jump-to-captured t)
                           ))
  )

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
    (shell-command
     (format "shortcuts run \"Translate File\" -i %s &" tempfile))
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
    (shell-command
     (format "shortcuts run \"Translate File 2 English\" -i %s &" tempfile))
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

(defun my/open-microsoft-bing ()
  (interactive)
  (xwidget-webkit-browse-url "https://www.bing.com" t)
  )

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

;; dictionary: Apple 词典: osx-dictionary
;; {{{
(use-package osx-dictionary
  :bind
  ("C-c d d" . osx-dictionary-search-word-at-point)
  )
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

;; exec-path-from-shell
;; {{{
(use-package exec-path-from-shell
  :defer nil
  ;; :if (memq window-system '(mac ns x))
  :when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(windows-nt dos))
              (daemonp)))
  :init (exec-path-from-shell-initialize)
  ;; :config
  ;; (progn
  ;;   (when (memq window-system '(mac ns x))
  ;;     (exec-path-from-shell-initialize))

  ;;   (when (daemonp)
  ;;     (exec-path-from-shell-initialize)
  ;;     )
  ;;   )
  )
;; }}}

(use-package which-key
  :hook (after-init . which-key-mode) ; :init (which-key-mode)
  :config
  ;; (which-key-mode)
  (which-key-posframe-mode)
  (setq
   which-key-idle-delay 0.5
   which-key-idle-secondary-delay 0.5
   which-key-show-operator-state-maps t
   )
  )

;; free-keys
;; {{{
(use-package free-keys
  :ensure nil
  :defer 2
  :config
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
                              "C-c H"
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
         ("H-c H-l" . mc/edit-lines)
         ("H-c H-n" . mc/mark-next-like-this)
         ("H-c H-p" . mc/mark-previous-like-this)
         ("H-c H-h" . mc/mark-all-like-this)
         ("H-c H-r" . set-rectangular-region-anchor)
         )
  :config
  (add-hook 'activate-mark-hook '(lambda ()
                                   (local-set-key
                                    (kbd "C-@")
                                    'set-rectangular-region-anchor)
                                   ))
  
  (add-hook 'deactivate-mark-hook '(lambda ()
                                     (local-unset-key
                                      (kbd "C-@"))
                                     ))
  )
;; }}}

;; avy
;; {{{
;; https://karthinks.com/software/avy-can-do-anything
(use-package avy
  :ensure nil
  :bind
  ;; avy-goto-char-timer
  ("H-j H-j"   . avy-goto-char)
  ("H-j 2"     . avy-goto-char-2)
  ("H-j H-k"   . avy-goto-line)
  ;; ("M-g w"   . avy-goto-word-1)
  ;; ("M-g e"   . avy-goto-word-0)
  ("C-c H-j" . avy-resume)
  :custom
  (avy-background t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?q ?e ?r ?u ?i ?p ?n))
  :config
    (defun avy-action-embark (pt)
	(unwind-protect
		(save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
	t)
  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-embark)
  )
;; }}}

(use-package puni
  :ensure nil
  :defer t
  )

(use-package parrot
  ;; :defer t
  :bind (
         ;;
         ("H-w r" . parrot-rotate-prev-word-at-point)
         ("H-w t" . parrot-rotate-next-word-at-point)
         ;;
         ("H-k H-k" . parrot-rotate-next-word-at-point)
         ("H-k H-j" . parrot-rotate-prev-word-at-point)
         )
  :config
  (parrot-mode)
  (parrot-set-parrot-type 'emacs)
  (setq parrot-rotate-dict
        '(
          ;; personal setting
          (:rot ("¥" "$" "￥"))
          (:rot ("nil" "t"))
          (:rot ("setq" "defvar"))
          ;;
          (:rot ("alpha" "beta") :caps t :lower nil)
          ;; => rotations are "Alpha" "Beta"

          (:rot ("snek" "snake" "stawp"))
          ;; => rotations are "snek" "snake" "stawp"

          (:rot ("yes" "no") :caps t :upcase t)
          ;; => rotations are "yes" "no", "Yes" "No", "YES" "NO"

          (:rot ("&" "|"))
          ;; => rotations are "&" "|"

          ;; default dictionary starts here ('v')
          (:rot ("begin" "end") :caps t :upcase t)
          (:rot ("enable" "disable") :caps t :upcase t)
          (:rot ("enter" "exit") :caps t :upcase t)
          (:rot ("forward" "backward") :caps t :upcase t)
          (:rot ("front" "rear" "back") :caps t :upcase t)
          (:rot ("get" "set") :caps t :upcase t)
          (:rot ("high" "low") :caps t :upcase t)
          (:rot ("in" "out") :caps t :upcase t)
          (:rot ("left" "right") :caps t :upcase t)
          (:rot ("min" "max") :caps t :upcase t)
          (:rot ("on" "off") :caps t :upcase t)
          (:rot ("prev" "next"))
          (:rot ("start" "stop") :caps t :upcase t)
          (:rot ("true" "false") :caps t :upcase t)
          (:rot ("&&" "||"))
          (:rot ("==" "!="))
          (:rot ("." "->"))
          (:rot ("if" "cond" "else" "elif"))
          (:rot ("ifdef" "ifndef"))
          (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
          (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
          (:rot ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
          (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))
          )
        )
  )

(use-package visual-regexp
  :ensure nil
  :defer 1
  :bind
  (
   ("C-c r e" . vr/replace)
   ("C-c q r" . vr/query-replace)
   ("C-c m m" . vr/mc-mark)        ; for multiple-cursors
   )
  )

(use-package visual-regexp-steroids
  :ensure nil
  :defer 1
  :bind
  (
   :map esc-map
   ("C-r" . vr/isearch-backward)
   ("C-s" . vr/isearch-forward)
   )
  ;; :config
  ;;  (setq vr/engine 'pcre2el) ; emacs python
  ;; (defvar vr--command-python-default
  ;;   (format
  ;;    "python3 %s"
  ;;    (expand-file-name "regexp.py" (file-name-directory load-file-name))))
  )

(use-package elisp-depmap
  :ensure nil
  :init
  ;; read-symbol-positions-list is deleted from Emacs 29
  (defvar read-symbol-positions-list nil)
  :bind (
         ("C-c H-d" . elisp-depmap-graphviz-digraph)
         ("C-c H-g" . elisp-depmap-graphviz)
         ("C-c H-s" . elisp-depmap-makesummarytable)
         )
  :config
  (setq elisp-depmap-parse-hashtablesize 1024)
  ;; (elisp-depmap-exec-file "~/.config/emacs/assets/elisp-dep-ana.dot")
  )

(use-package elisp-autofmt
  :commands
  (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode)
  )

(use-package vundo
  :ensure nil
  :bind
  (
   ("C-x u" . vundo)
   :map vundo-mode-map
   ("h" . vundo-backward) ; b
   ("j" . vundo-next)     ; n
   ("k" . vundo-previous) ; p
   ("l" . vundo-forward)  ; f
   ;; ("C-g" . vundo-quit)
   ;; ("RET" . vundo-confirm)
   ;; ("a"   . vundo-stem-root)
   ;; ("d"   . vundo--debug)
   ;; ("e"   . vundo-stem-end)
   ;; ("i"   . vundo--inspect)
   ;; ("q"   . vundo-quit)
   )

  )

;; difftastic + magit
;; {{{
;; (with-eval-after-load 'magit
(use-package magit
  ;; :defer 1
  :bind (
         ("C-x g"   . magit-status)
         ("C-c v g" . magit-status)
         ("H-m H-m" . magit-status)
         :map magit-status-mode-map
         ("#" . my/magit-aux-commands)
         )
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

(use-package wgrep
  :ensure nil
  :bind
  (
   :map grep-mode-map
   ("C-c C-q" . wgrep-change-to-wgrep-mode)
   )
  )

;; deadgrep
;; {{{
(use-package deadgrep
  ;; :defer 1.5
  :bind
  (
   ("C-c d g" . deadgrep)
   ("C-c g o"   . my/grep-org-files)
   :map deadgrep-mode-map
   ("e" . deadgrep-edit-mode)
   :map deadgrep-edit-mode-map ; wgrep
   ("C-c C-c" . deadgrep-mode)
   )
  :config
  (defun my/grep-org-files (words)
    (interactive "sSearch(ripgrep) org-roam files: ")
    (let ((default-directory org-roam-directory)
          (deadgrep--file-type '(glob . "*.org"))
          (deadgrep--context '(1 . 1))
          (deadgrep--search-type 'regexp))
      (deadgrep words)
      )
    )
  )
;; }}}

(use-package pinyinlib
  :ensure nil
  )

;; ace-pinyin
;; {{{
(use-package ace-pinyin
  :defer 1
  :config
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode +1)
  )
;; }}}

;; pyim
;; {{{
(use-package pyim
  :ensure nil
  :defer 1
  :bind
  (
   ("H-e" . toggle-input-method)
   ([remap backward-word] . pyim-backward-word)
   ([remap forward-word]  . pyim-forward-word)
   ("H-b" . pyim-backward-word)
   ("H-f" . pyim-forward-word)
   ("H-c H-s" . pyim-convert-string-at-point) ; 金手指：将字符串转换为中文。
   :map minibuffer-local-map
   ("H-c" . pyim-cregexp-convert-at-point) ; 将光标前字符转换为搜索中文的 regexp.
   :map pyim-mode-map
   ("-"   . pyim-page-previous-page)
   ("+"   . pyim-page-next-page)
   ("H-h" . pyim-page-previous-page)
   ("H-l" . pyim-page-next-page)
   )
  :init
  ;; (setq default-input-method "pyim")
  (setq pyim-default-scheme 'quanpin) ; shaungpin/rime
  ;; (setq default-input-method "pyim")
  (setq pyim-page-style 'vertical)
  (setq pyim-page-tooltip '(posframe minibuffer popup))
  (setq pyim-page-length 5)
  :config
  (require 'pyim-cregexp-utils)
  (require 'pyim-cstring-utils)

  ;; (require 'pyim-basedict) ; 拼音词库设置
  ;; (pyim-basedict-enable)   ; 拼音词库
  ;; (require 'pyim-greatdict)
  ;; (pyim-greatdict-enable)
  (require 'pyim-tsinghua-dict)
  (pyim-tsinghua-dict-enable)

  (setq-default pyim-punctuation-translate-p '(auto)) ;; 全角半角

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

  (pyim-isearch-mode -1) ; 性能差，不启用
  )
;; }}}

;; orderless: minibuffer filter, works with icomplete
;; {{{
(use-package orderless
  :ensure nil
  :init
  (setq completion-styles '(basic partial-completion orderless))
  ;; (setq completion-styles '(orderless basic initials substring partial-completion flex)
  (setq orderless-component-separator "[ &]") ; & is for company because space will break completion
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil)
  ;; (setq completion-category-overrides '(
  ;;                                    (file
  ;;                                     (styles basic partial-completion)
  ;;                                     )
  ;;                                    )
  ;;    )
  ;; :config
  ;; ;; make completion support pinyin, refer to
  ;; ;; https://emacs-china.org/t/vertico/17913/2
  ;; ;; list 版
  ;; (defun completion--regex-pinyin (str)
  ;;   (orderless-regexp (pinyinlib-build-regexp-string str))
  ;;   )
  ;; (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
  ;; ;; advice 版
  ;; (defun orderless-regexp-pinyin (str)
  ;;   (setf (car str) (pinyinlib-build-regexp-string (car str)))
  ;;   str)
  ;; (advice-add 'orderless-regexp :filter-args #'orderless-regexp-pinyin)
  )
;; }}}

;; pangu-spacing
;; {{{
(use-package pangu-spacing
  :defer 1
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

;; FIXME args-out-of-range
(setq ispell-extra-args '(
                          "--sug-mode=ultra"
                          "--lang=en_US"
                          "--camel-case"
                          "--run-together"
                          "--run-together-limit=16"
                          ))

;; ispell-personal-dictionary
;; }}}

;; wucuo
;; {{{
;; [redguardtoo](https://github.com/redguardtoo/emacs.d/lisp/init-spelling.el)
(defvar my-default-spell-check-language "en_US"
  "Language used by aspell and hunspell CLI.")

(use-package flyspell
  :bind
  ("C-c s" . flyspell-auto-correct-word)
  ;; You can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
  ;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html

  ;; flyspell-lazy is outdated and conflicts with latest flyspell
  :config
  ;; better performance
  (setq flyspell-issue-message-flag nil)
  )

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
        (setq args (list "--sug-mode=ultra"
                         (format "--lang=%s" my-default-spell-check-language)))
        ;; "--run-together-min" could not be 3, see `check` in "speller_impl.cpp".
        ;; The algorithm is not precise.
        ;; Run `echo tasteTableConfig | aspell --lang=en_US -C --run-together-limit=16  --encoding=utf-8 -a` in shell.
        (when run-together
          (cond
           ;; Kevin Atkinson said now aspell supports camel case directly
           ;; https://github.com/redguardtoo/emacs.d/issues/796
           ((string-match "--.*camel-case"
                          (shell-command-to-string
                           (concat ispell-program-name " --help")))
            (setq args (append args '("--camel-case"))))

           ;; old aspell uses "--run-together". Please note we are not dependent on this option
           ;; to check camel case word. wucuo is the final solution. This aspell options is just
           ;; some extra check to speed up the whole process.
           (t
            (setq args
                  (append args
                          '("--run-together" "--run-together-limit=16")))))))

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
  (setq wucuo-update-interval 2)
  )

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
  :ensure nil
  :diminish yas-minor-mode
  :hook ((after-init . yas-reload-all)
         ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
         )
  :config
  ;; (yas-global-mode 1)
  ;; Suppress warning for yasnippet code.
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  ;; (setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))

  ;; (defun smarter-yas-expand-next-field ()
  ;;   "Try to `yas-expand' then `yas-next-field' at current cursor position."
  ;;   (interactive)
  ;;   (let ((old-point (point))
  ;;         (old-tick (buffer-chars-modified-tick)))
  ;;     (yas-expand)
  ;;     (when (and (eq old-point (point))
  ;;                (eq old-tick (buffer-chars-modified-tick)))
  ;;       (ignore-errors (yas-next-field))))
  ;;   )
  )
;; }}}

;; helpful
;; {{{
(use-package helpful
  :ensure nil
  :bind
  ;; ("C-h C" . helpful-command) ;; Look up *C*ommands.
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  ;; ("C-h F" . helpful-function) ; functions only
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  ("C-h f" . helpful-callable)    ; both functions and macros
  ("C-h h" . #'helpful-at-point)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable) ; "C-h v"
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key) ; "C-h k"
  ("C-c C-d" . helpful-at-point) ; Lookup the current symbol at point.
  ;; :config
  ;; helpful + ivy
  ;; (setq counsel-describe-function-function #'helpful-callable)
  ;; (setq counsel-describe-variable-function #'helpful-variable)
  )
;; }}}

;; sticky header: topsy
;; {{{
(use-package topsy
  :ensure nil
  :hook (prog-mode . topsy-mode) ;; (add-hook 'prog-mode-hook #'topsy-mode)
  )
(use-package org-sticky-header
  :ensure nil
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'full)
  )
;; }}}

;; diff-hl
;; {{{
(use-package diff-hl
  :ensure nil
  :hook (
         (dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         )
  :init
  (global-diff-hl-mode t)
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  )
;; (global-git-gutter-mode +1) ; BUG/Bad performance when deleting folded 17000+lines

;; }}}

;; goggles: visual hint for operations
;; {{{
(use-package goggles
  :ensure nil
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t) ;; set to nil to disable pulsing
  )
;; }}}

;; highlight-parentheses
;; {{{
(use-package highlight-parentheses
  ;; :defer 1
  :hook (after-init . highlight-parentheses-mode)
  :config
  (setq-default global-highlight-parentheses-mode t)
  ;; 括号颜色（由内向外）
  (setq-default highlight-parentheses-colors '(
                                       "Green"
                                       "Blue"
                                       "Orange"
                                       "Purple"
                                       "Yellow"
                                       "Red"
                                       ;; "Pink" ;; only six colors supported ?
                                       ))
  )

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
  ;; :init (doom-modeline-mode 1)
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  :config
    (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-height 18)
  (setq doom-modeline-window-width-limit 85)
  (setq doom-modeline-icon (display-graphic-p))
  (setq find-file-visit-truename t)
  (setq doom-modeline-highlight-modified-buffer-name t)
  (setq doom-modeline-project-detection 'auto) ;auto/project
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  )
;; }}}

;; all-the-icons
;; {{{
(use-package all-the-icons
  :ensure nil
  ;; :when (display-graphic-p)
  :if (display-graphic-p)
  :hook
  (
   (dired-mode . all-the-icons-dired-mode)
   (marginalia-mode . all-the-icons-completion-marginalia-setup)
   )
  )

(use-package all-the-icons-completion
  :ensure nil
  :hook
  (
   (after-init . all-the-icons-completion-mode)
   (marginalia-mode . all-the-icons-completion-marginalia-setup)
   )
  )

(use-package all-the-icons-dired
  :ensure nil
  :when (display-graphic-p)
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )
;; }}}

;; fold: origami
;; {{{
(add-hook 'prog-mode-hook 'origami-mode)
(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)
  )
;; }}}

;; expand-region
;; {{{
(keymap-global-unset "C-=")
(keymap-global-unset "C--")
(use-package expand-region
  :bind (
	 ("C-=" . er/expand-region)
         ("C--" . er/contract-region)
	 )
  )
;; }}}

;; symbol-overlay
;; {{{
(use-package symbol-overlay
  :bind(
        ("M-I" . symbol-overlay-remove-all)
        ("M-i"  . symbol-overlay-put) ; 高亮或取消高亮当前 symbol
        ("M-n"  . symbol-overlay-switch-forward)
        ("M-p"  . symbol-overlay-switch-backward)
        ;; ("<f7>" . symbol-overlay-mode)
        ;; ("<f8>" . symbol-overlay-remove-all)
        :map symbol-overlay-map
        ("<" . symbol-overlay-jump-first)
        (">" . symbol-overlay-jump-last)
        ("R" . symbol-overlay-query-replace)      ; 查找替换 symbol
        ("c" . symbol-overlay-save-symbol)        ; 复制当前 symbol
        ("d" . symbol-overlay-jump-to-definition) ; 跳转到定义
        ("e" . symbol-overlay-echo-mark)          ; 撤销上一次跳转
        ("i" . symbol-overlay-put)                ; 高亮或取消高亮当前 symbol
        ("n" . symbol-overlay-jump-next)
        ("p" . symbol-overlay-jump-prev)
        ("q" . symbol-overlay-remove-all)
        ("r" . symbol-overlay-rename)
        ("s" . symbol-overlay-isearch-literally)  ; 切换为 isearch 并搜索 symbol
        ("t" . symbol-overlay-toggle-in-scope)    ; 切换高亮范围到作用域
        )
  )
;; }}}

;; consult
;; {{{
;; Example configuration for Consult
(use-package consult
  ;; :defer 1
  :after org
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (
         ("C-c f d" . consult-find)
         ("C-c r e" . consult-grep)
         ("C-c r g" . consult-ripgrep)
         ("C-x H-l" . consult-focus-lines)
         ;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
	          ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
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
         ("H-r" . consult-history) ;; orig. previous-matching-history-element
         :map org-mode-map
         ("C-c C-j"  . consult-org-heading)
         :map prog-mode-map
         ("C-c C-j"  . consult-outline)
         )
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  ;; macOS locate doesn't support `--ignore-case --existing' args.
  (setq consult-locate-args (pcase system-type
                              ('gnu/linux
                               "locate --ignore-case --existing --regex")
                              ('darwin
                               "mdfind -name")
                              ))
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
   :preview-key (kbd "M-.")
   )

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
  :bind
  (
   :map vertico-map
   ("<tab>" . vertico-insert)    ; Choose selected candidate
   ("<escape>" . minibuffer-keyboard-quit) ; Close minibuffer
   )
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

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; ;;  More convenient directory navigation commands
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        )
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
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

;; marginalia: minibuffer annotations
;; {{{
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  ;; :bind (("C-M-a" . marginalia-cycle)
  ;;        :map minibuffer-local-map
  ;;        ("C-M-a" . marginalia-cycle))
  ;; :custom (marginalia-align 'right)
  :init
  ;; The :init configuration is always executed (Not lazy!)
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  ;; (marginalia-mode)
  :hook (after-init . marginalia-mode)
  )
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

(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (
   ("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   :map minibuffer-mode-map
   ("H-o" . embark-export)
   )
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

(use-package org-modern
  :after org
  ;; :hook (org-mode . global-org-modern-mode)
  :config
  (setq
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (if (display-graphic-p)
      (setq org-modern-table t)
    (setq org-modern-table nil)
    )
  (global-org-modern-mode)
  )

;; org-roam: basic config
;; {{{
(use-package org-roam
  :after (org)
  :defer 1
  :init
  (setq org-roam-directory "~/org-roam")
  (setq org-roam-db-location "~/org-roam/org-roam.db")
  :bind (
         ("C-c n a" . org-roam-alias-add)
         ("C-c n A" . org-roam-alias-remove)
         ("C-c n c" . org-roam-capture)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n h" . org-fold-hide-entry)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n j" . org-roam-dailies-capture-today) ;; Dailies
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n o" . org-id-get-create)
         ("C-c n r" . org-roam-node-random)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n T" . org-roam-tag-remove)
         )
  :config
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-mode-sections
        '((org-roam-backlinks-section :unique t)
          org-roam-reflinks-section
          org-roam-unlinked-references-section))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-file-extensions '("org" "md")) ;; enable Org-roam for markdown
  ;; (setq org-roam-node-display-template "${title:50} ${tags:30}")
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (require 'org-roam-protocol)  ;; org-roam-protocol
  (org-roam-db-autosync-mode)
  ;; (org-roam-db-autosync-mode 1) ;; if md-roam installed, move to md-roam config
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
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n")
         :immediate-finish t
         :unnarrowed  t)
        ("c" "角色" plain "%?"
         :target (file+head "topics/角色/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ;; C
        ("d" "default" plain "%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n")
         :empty-lines 1
         :immediate-finish t
         :unnarrowed  t)
        ;; D
        ("e" "Emacs" plain "%?"
         :target (file+head "Emacs/${slug}.org"
                            "#+title: ${title}\n#+category:\n")
         :immediate-finish t
         :unnarrowed  t)
        ;; E
        ;; f:
        ("f" "Emacs 命令与函数/Command & Function" plain "%?"
         :target (file+head "Emacs/function/${title}.org"
                            "#+title: ${title}\n#+category:\n")
         :immediate-finish t
         :unnarrowed  t)
        ;; F
        ;; g:
        ("G" "游戏" plain "%?"
         :target (file+head "游戏/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("h" "人物" plain "%?"
         :target (file+head "topics/人物/${slug}.org"
                            "#+title: ${title}\n* ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ;; H
        ;; i:
        ;; I
        ;; j:
        ;; J
        ("k" "Emacs 快捷键/keymap" plain "%?"
         :target (file+head "Emacs/keymap/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ;; K
        ;; l:
        ;; L
        ("m" "音乐" plain "%?"
         :target (file+head "音乐/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("M" "电影" plain "%?"
         :target (file+head "电影/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
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
                            "#+title: ${title}\n#+filetags: :Emacs:\n")
         :immediate-finish t
         :unnarrowed t)
        ;; q:
        ;; Q
        ("r" "reference" plain "%?"
         :target (file+head "reference/${slug}.org"
                            "#+title: ${title}\n#+date: %<%FT%T%z>\n")
         :immediate-finish t
         :unnarrowed t)
        ("R" "纪录片" plain "%?"
         :target (file+head "纪录片/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
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
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("T" "电视剧" plain "%?"
         :target (file+head "电视剧/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ;; u:
        ;; U
        ("v" "Emacs 变量" plain "%?"
         :target (file+head "Emacs/variable/${title}.org"
                            "#+title: ${title}\n")
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

;; org-roam-ui
;; {{{
(use-package org-roam-ui
  :after (org-roam)
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :bind (
	 ("C-c n u" . org-roam-ui-open)
	 ("C-c n z" . org-roam-ui-node-zoom)
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
     (format
      "open -b \"net.sourceforge.sqlitebrowser\" --args --table nodes %s"
      org-roam-db-location)))
   (t
    (message "my/org-roam-view-db not yet working on this system-type"))))
;; }}}

;; consult-org-roam
;; {{{
(use-package consult-org-roam
  :ensure nil
  :after (org consult)
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n s" . consult-org-roam-search)
  ("C-c n F" . consult-org-roam-file-find)
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
   :preview-key (kbd "M-.")
   )
  )
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
  :defer t
  :config
  (setq rfc-mode-directory (expand-file-name "~/Documents/GitHub/RFC-all/txt/"))
  (setq rfc-mode-index-path (concat rfc-mode-directory"rfc-index.txt"))
  )
;; }}}

(with-eval-after-load "moom"
  (setq moom-use-font-module nil)
  ;; (moom-recommended-keybindings '(all wof))
  (moom-recommended-keybindings '(move fit expand fill font reset undo))
  (when (require 'moom-transient nil t)
    (moom-transient-hide-cursor) ;; if needed
    (define-key moom-mode-map (kbd "C-c o") #'moom-transient-dispatch)
    )
  )

;; {{{ ace-window
;; (require 'ace-window)
;; (keymap-global-set "H-o" #'ace-window)
(use-package ace-window
  :ensure nil
  :bind
  ("H-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )
;; }}}

;; goto-line-preview
;; {{{
(use-package goto-line-preview
  :bind
  ([remap goto-line] . goto-line-preview)
  ("C-c H-l" . goto-line-preview)
  )
;; }}}

;; auto-dark
;; {{{
(use-package auto-dark
  :init (auto-dark-mode t)
  :config
  (setq auto-dark-allow-osascript t
        auto-dark-dark-theme 'solarized-dark)
  )
;; }}}

;; graphviz-dot-mode
;; {{{
(use-package graphviz-dot-mode
  :ensure nil
  :defer t
  ;; :bind
  ;; ()
  :config
  (setq graphviz-dot-indent-width 4)
  (setq graphviz-dot-preview-extension "svg")
  )
;; }}}

;; osm: OpenStreetMap
;; {{{
(use-package osm
  :defer t
  ;; :after org
  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol))
  :bind (
         ("C-c m h" . osm-home)
         ("C-c m s" . osm-search)
         ("C-c m v" . osm-server)
         ("C-c m t" . osm-goto)
         ("C-c m x" . osm-gpx-show)
         ("C-c m j" . osm-bookmark-jump)
         :map osm-mode-map
         ("q" . (lambda() (interactive) (quit-window t)))
         )
  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information
  )
;; }}}

;; mybigword
;; {{{
(use-package mybigword
  :defer t
  :config
  (setq mybigword-excluded-words
        (split-string (with-temp-buffer
                        (insert-file-contents  (expand-file-name
                                                "assets/mybigword.txt"
                                                (concat user-emacs-directory)
                                                ))
                        (buffer-string)) "[\r\n]+"))
  )
;; mybigword-excluded-words
;; mybigword-personal-excluded-words
;; mybigword-upper-limit
;; }}}

;; elfeed
;; {{{
;; reference: https://github.com/jiacai2050/jiacai2050.github.io/blob/hugo/playground/mu4e-elfeed-config.el
(use-package elfeed
  :defer t
  :ensure nil
  ;; :bind
  ;; (
  ;;  :map elfeed-search-mode-map
  ;;  ("A" . bjm/elfeed-show-all)
  ;;  ("E" . bjm/elfeed-show-emacs)
  ;;  ("D" . bjm/elfeed-show-daily)
  ;;  ("q" . bjm/elfeed-save-db-and-bury)
  ;;  )
  :custom((elfeed-use-curl t)
          (elfeed-db-directory "~/Downloads/elfeed/")
          (elfeed-curl-timeout 20)
          )
  )

;; elfeed-dashboard
(use-package elfeed-dashboard
  :ensure nil
  :after elfeed
  )

;;elfeed-org
(use-package elfeed-org
  :ensure nil
  :defer t
  ;; :after elfeed
  ;; :hook (elfeed-dashboard-mode . elfeed-org)
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.config/emacs/elfeed.org"))
  )
;; }}}

;; org-mac-link
;; {{{
(use-package org-mac-link
  :ensure nil
  :bind
  ("H-i H-i" . org-mac-link-get-link)
  )
;; }}}

(use-package mode-minder
  :ensure nil
  :defer t
  )

(use-package explain-pause-mode
  :ensure nil
  :defer 1
  )
;; (run-with-idle-timer
;;  1 nil
;;  #'(lambda ()
;;      (require 'explain-pause-mode)
;;      ))

(use-package browser-hist
  :ensure nil
  :init
  (use-package sqlite)
  :bind
  ("H-s H-s" . browser-hist-search)
  :config
  (setq browser-hist-db-paths
        '(
          (chrome . "$HOME/Library/Application Support/Google/Chrome/Default/History")
          (brave . "$HOME/Library/Application Support/BraveSoftware/Brave-Browser/Default/History")
          (firefox . "$HOME/Library/Application Support/Firefox/Profiles/*.default-release/places.sqlite")
          (safari . "$HOME/Library/Safari/History.db")
          ))
  (setq browser-hist-default-browser 'safari) ; FIXME
  :commands
  (browser-hist-search)
  )

(use-package advance-words-count
  :ensure nil
  ;; :defer 2
  :bind
  ("C-c w c" . advance-words-count)
  )

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
          mwheel-scroll
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
          org-self-insert-command
          self-insert-command
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

;; lsp-bridge                                        ; FIXME
;; {{{
(use-package lsp-bridge
  :ensure nil
  :after (yasnippet)
  :hook (prog-mode . lsp-bridge-mode)
  :init
  (setq-default lsp-bridge-enable-mode-line nil)
  ;; (setq lsp-bridge-use-ds-pinyin-in-org-mode t)
  ;; (setq lsp-bridge-use-wenls-in-org-mode t)
  (setq acm-enable-quick-access t)
  (setq acm-quick-access-modifier 'meta)
  ;; :hook ((prog-mode org-mode) . lsp-bridge-mode)

  :bind (
         :map acm-mode-map
         ("C-j"       . acm-insert-common)
         ;; complete
         ("SPC"       . acm-complete)
         ("RET"       . acm-complete)
         ;; select
         ("TAB"       . acm-select-next)
         ("<backtab>" . acm-select-prev)
         ;; ("H-TAB" . acm-select-prev)
         ("H-j"       . acm-select-next)
         ("H-k" . acm-select-prev)
         )
  ;; :custom
  :config
  ;; lsp-bridge-org-babel-lang-list
  ;; default: clojure latex python
  (add-to-list 'lsp-bridge-org-babel-lang-list "emacs-lisp")
  (add-to-list 'lsp-bridge-org-babel-lang-list "shell")
  )
;; }}}

;; unicode
;; {{{
(use-package modeline-char
  :ensure nil
  :hook
  (after-init . mlc-char-in-mode-line-mode-global)
  )
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

(use-package diff-lisp
  :ensure nil
  :bind
  ("s-/" . diff-lisp-set-a-and-b)
  :config
  (defvar diff-lisp-set-a-and-b nil)
  (defun diff-lisp-set-a-and-b ()
    (interactive)
    (if (eq diff-lisp-set-a-and-b nil)
        (progn
          (diff-lisp-mark-selected-text-as-a)
          (setq diff-lisp-set-a-and-b t)
          )
      (progn
        (diff-lisp-diff-a-and-b)
        (setq diff-lisp-set-a-and-b nil)
        ))
    )
  )

;; subed: subtitle edit
;; {{{
(use-package subed
  :defer t
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

(use-package d2-mode
  :ensure nil
  :defer t
  ;; :bind
  ;; (
  ;;  :map d2-mode-map
  ;;  )
  )

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
  :defer t
  :ensure nil
  )
;; }}}

;;; init.el ends here.
