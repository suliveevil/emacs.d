;;; init-packge.el
;; packages (installed by package.el) configuration
;; siblings: init.el init-lib.el

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; exec-path-from-shell
;; {{{
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))
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

;; ace-pinyin
;; {{{
(use-package ace-pinyin
  ;; :defer 1
  :config
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode +1)
  )
;; }}}

;; pangu-spacing
;; {{{
(use-package pangu-spacing
  ;; :defer 1
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor t)
  )
;; }}}

;; smart-input-source
;; {{{
(use-package sis
  :init
  ;; `C-s/r' 默认优先使用英文 必须在 sis-global-respect-mode 前配置
  (setq sis-respect-go-english-triggers
        (list 'isearch-forward 'isearch-backward) ; isearch-forward 时默认进入 en
        sis-respect-restore-triggers
        (list 'isearch-exit 'isearch-abort))
  :config
  (sis-ism-lazyman-config
   "com.apple.keylayout.ABC"
   "com.apple.inputmethod.SCIM.ITABC"
   'macism
   )
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t)
  (sis-global-context-mode t)
  (sis-global-inline-mode t)   ; 中文状态下，中文后<spc>切换英文，结束后切回中文
  ;; (keymap-global-set "<f9>" #'sis-log-mode) ; 开启日志
  ;; 特殊定制
  (setq sis-default-cursor-color "green" ;; 英文光标色
        sis-other-cursor-color "purple"  ;; 中文光标色 green
        ;; sis-inline-tighten-head-rule 'all ; 删除头部空格，默认 1，删除一个空格，1/0/'all
        sis-inline-tighten-tail-rule 'all ; 删除尾部空格，默认 1，删除一个空格，1/0/'all
        sis-inline-with-english t ; 默认是 t, 中文 context 下输入<spc>进入内联英文
        sis-inline-with-other t) ; 默认是 nil，而且 prog-mode 不建议开启, 英文 context 下输入<spc><spc>进行内联中文
  ;; 特殊 buffer 禁用 sis 前缀,使用 Emacs 原生快捷键  setqsis-prefix-override-buffer-disable-predicates
  (setq sis-prefix-override-buffer-disable-predicates
        (list 'minibufferp
              (lambda (buffer) ; magit revision magit 的 keymap 是基于 text property 的，优先级比 sis 更高。进入 magit 后，disable sis 的映射
                (sis--string-match-p "^magit-revision:" (buffer-name buffer)))
              (lambda (buffer) ; special buffer，所有*打头的 buffer，但是不包括*Scratch* *New, *About GNU 等 buffer
                (and (sis--string-match-p "^\*" (buffer-name buffer))
                     (not (sis--string-match-p "^\*About GNU Emacs" (buffer-name buffer))) ; *About GNU Emacs" 仍可使用 C-h/C-x/C-c 前缀
                     (not (sis--string-match-p "^\*New" (buffer-name buffer)))
                     (not (sis--string-match-p "^\*Scratch" (buffer-name buffer))))))) ; *Scratch*  仍可使用 C-h/C-x/C-c 前缀
  )
;; }}}

;; dictionary: Apple 词典: osx-dictionary
;; {{{
;; (require 'osx-dictionary)
(keymap-global-set "C-c d" #'osx-dictionary-search-word-at-point)
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


;; diff-hl
;; {{{
(global-diff-hl-mode)
;; (global-git-gutter-mode +1) ; BUG when deleting folded 17000+lines

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

;; all-the-icons
;; {{{
;; (use-package all-the-icons :if (display-graphic-p))
(when (display-graphic-p)
  (require 'all-the-icons)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))
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

;; moom
;; {{{
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (add-hook 'after-init-hook 'moom-mode)
  ;; moom + transient
  (with-eval-after-load "moom"
    (setq moom-use-font-module nil)
    (when (require 'moom-transient nil t)
      (moom-transient-hide-cursor) ;; if needed
      (define-key moom-mode-map (kbd "C-c o") #'moom-transient-dispatch)
      )
    )
  )
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

;; olivetti
;; {{{
;; https://emacs-china.org/t/emacs/19797/4
(use-package olivetti
  :diminish
  :bind ("<f8>" . olivetti-mode)
  :init
  (setq olivetti-body-width 90)         ; default: fill-column+2
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

(provide 'init-package)

;;; init-package.el ends here
