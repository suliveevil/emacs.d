;; -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-
;; -*- origami-fold-style: triple-braces -*-

;;; Commentary:

;; early-init.el --- Emacs 27+ pre-initialisation config
;; Code loaded before the package system and GUI is initialized.
;; Date: 2023-01-06 07:23:09 +0800

;;; Code:

(let (
      ;; (gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil)
      )
  )

(setq gc-cons-threshold most-positive-fixnum) ; Don't collect garbage when init
;; (setq gc-cons-threshold (* 500 1024 1024)) ; 500 MiB

;; Restore/decrease GC after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 10 1024 1024)) ; default 800000
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; (setq load-prefer-newer t)
(setq load-prefer-newer noninteractive)

(setq inhibit-automatic-native-compilation t)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.6f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; startup
;; {{{
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message (user-login-name))
;; }}}

(setq python-shell-interpreter "python3")

;; debug warning and error
;; {{{
;; debug
(setq debug-on-error t)
;; warning
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)
;; (add-to-list 'warning-suppress-log-types '((defvaralias))) ; FIXME
;; error
;; }}}

(setq confirm-kill-emacs (lambda (prompt) (y-or-n-p-with-timeout "确认退出？" 10 "y")))
;; (setq confirm-kill-emacs 'yes-or-no-p)
(setq use-short-answers t) ;; use y/n instead of yes/no

;; custome-file
;; {{{
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(when (file-exists-p custom-file)
  (load custom-file))
;; }}}

;; encoding: prefer UTF-8 everywhere
;; {{{
;; [hick/emacs-chinese: Emacs 相关中文问题以及解决方案](https://github.com/hick/emacs-chinese)
(set-charset-priority 'unicode)
(set-language-environment "UTF-8") ;; System default coding
(prefer-coding-system 'utf-8) ;; prefer
(set-buffer-file-coding-system 'utf-8-unix) ;;
(set-charset-priority 'unicode) ;;
(set-clipboard-coding-system 'utf-8) ;; clipboard
(set-default-coding-systems 'utf-8) ;; buffer/file: 打开文件时的默认编码
(set-file-name-coding-system 'utf-8-unix) ;; unix/linux/macos
(set-keyboard-coding-system 'utf-8-unix) ;; keyboard
(set-next-selection-coding-system 'utf-8-unix) ;; selection
(set-selection-coding-system 'utf-8) ;; selection
(set-terminal-coding-system 'utf-8-unix) ;; terminal
(setq coding-system-for-read 'utf-8) ;;
(setq-default buffer-file-coding-system 'utf-8) ;;
(setq locale-coding-system 'utf-8) ;; local

(setq process-coding-system-alist
      (cons '("zsh" . (utf-8 . utf-8)) process-coding-system-alist))

(setq process-coding-system-alist
      (cons '("bash" . (utf-8 . utf-8)) process-coding-system-alist))

(setq process-coding-system-alist
      (cons '("git" . (utf-8 . utf-8)) process-coding-system-alist))

(setq process-coding-system-alist
      (cons '("grep" . (utf-8 . utf-8)) process-coding-system-alist))

(setq process-coding-system-alist
      (cons '("diff" . (utf-8 . utf-8)) process-coding-system-alist))

;; }}}

;; locale
;; {{{
(setenv "LANG" "zh_CN.UTF-8")
;; (setq system-time-locale "C")
;; }}}

(setq vc-follow-symlinks t)

;; keymap
;; {{{
;; bind: 全局按键/快捷键 (Global key bindings)
(setq echo-keystrokes 0.1)
(setq mac-command-modifier       'super   ;; s: super(Command/Win)
      mac-control-modifier       'control ;; C: Ctrl
      mac-option-modifier        'meta    ;; M: Meta (Option/Alt)
      mac-right-command-modifier 'hyper   ;; H: hyper (reachable for thumb)
      mac-right-option-modifier  'none    ;; Leave Option to macOS
      mac-right-control-modifier 'control ;; C: Ctrl
      ;; mac-function-modifier            ;; Function Key
      ;;                                  ;; A: Alt (redundant and not used)
      ;;                                  ;; H: Hyper
      ;;                                  ;; S: Shift
      )

;; }}}

;; basic keybinding
;; {{{
(keymap-global-set "s-a" #'mark-whole-buffer)
(keymap-global-set "s-c" #'kill-ring-save)          ;; M-w     copy       复制
;; clipboard-kill-ring-save
(keymap-global-set "s-q" #'save-buffers-kill-emacs) ;;         copy       复制
(keymap-global-set "s-v" #'yank)                    ;; C-y     paste/yank 粘贴
(keymap-global-set "s-w" #'delete-frame)            ;;
(keymap-global-set "s-s" #'save-buffer)             ;; C-x C-s save       保存
(keymap-global-set "s-x" #'kill-region)             ;; C-w     cut        剪切
;; clipboard-kill-region
(keymap-global-set "s-z" #'undo)                    ;; C-_     undo       撤销
(keymap-global-set "s-Z" #'undo-redo)               ;; C-M-_   undo-redo  重做
;;
(keymap-global-set     "S-s-<return>" #'toggle-frame-maximized)
(keymap-global-set     "C-s-f"        #'toggle-frame-fullscreen) ;; macOS
;;
(keymap-global-set "C-<backspace>" '(lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))
;;
(keymap-set global-map "H-q"          #'restart-emacs)
(keymap-global-set     "H-x"          #'execute-extended-command)

;; check-parens
(keymap-global-set "H-M-c" #'check-parens) ; <escape> H-c
;; }}}

(keymap-global-set "H-a" #'universal-argument)

(context-menu-mode 1)       ;; 鼠标右键菜单
(setq context-menu-functions
      '(context-menu-ffap
        occur-context-menu
        context-menu-region
        context-menu-undo
        context-menu-minor
        context-menu-local
        ))
(setq use-dialog-box nil)   ;; 鼠标点击不触发弹窗

;; 快速打开文件
;; {{{
(defun my/open-init-file () ;; Emacs init
  (interactive)
  (find-file-other-window user-init-file)
  (delete-other-windows)
  )

(keymap-global-set "C-c E" #'my/open-init-file)

(defun my/open-init-org () ;; Emacs init
  (interactive)
  (find-file-other-window
   (expand-file-name "init.org" (concat user-emacs-directory)))
  (delete-other-windows)
  )

(keymap-global-set "C-c H-e" #'my/open-init-org)
;; (defun open-goku-file()      ;; Emacs early-init
;;   (interactive)
;;   (find-file "~/.config/karabiner.edn")
;;   (find-file "~/.config/goku/karabiner.edn")
;; )

(defun my/find-shell-init-file ()
  "Edit the shell init file (bashrc/zshrc) in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file
          (cond
           ((string-equal "zsh" shell)
            ".zshrc")
           ((string-equal "bash" shell)
            ".bashrc")
           (t
            (error "Unknown shell")))))
    (find-file-other-window
     (expand-file-name shell-init-file (getenv "HOME"))))
  (delete-other-windows)
  )
;; }}}

;; UI
;; {{{
;; (push '(fullscreen . maximized) default-frame-alist)
(setq default-frame-alist
      '(
	(height . 46)
        (width . 97)
        (left . 700)
        (top . 20)
        (alpha . (95 .80))
        (vertical-scroll-bars . nil)
        ;; (horizontal-scroll-bars . nil)
        (tool-bar-lines . 0)
        ))
(push '(tool-bar-mode . nil) default-frame-alist)
(setq inhibit-splash-screen t)        ;; 禁用欢迎界面
(set-fringe-mode 10)        ;;
(global-visual-line-mode 1) ;;
(setq visible-bell t)       ;; 关闭提示声音
;; }}}

;; user name & email
;; {{{
(setq user-full-name "suliveevil")
(setq user-mail-address "suliveevil@qq.com")
;; user-domain          ""
;; user-organisation    ""
;; user-gpg-encrypt-key ""
;; }}}

;; package: package-enable-at-startup is before init but after early-init
;; {{{
(setq package-enable-at-startup nil) ;; don't enable at startup, pair with (package-initialize)
;; }}}

;; package
;; {{{
(setq package-archives
      '(
        ("elpa"                . "https://elpa.gnu.org/packages/")
        ("melpa"               . "http://melpa.org/packages/")
        ;; ("elpa-devel"          . "https://elpa.gnu.org/devel/")
        ;; ("jcs-elpa"            . "https://jcs-emacs.github.io/jcs-elpa/packages/")
        ;; ("gnu"                 . "http://elpa.gnu.org/packages/")
        ;; ("gnu-devel"           . "https://elpa.gnu.org/devel/")
        ;; ("gnu-tsinghua"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ;; ("gnu-ustc"            . "http://mirrors.ustc.edu.cn/elpa/gnu/")
        ;; ("melpa-stable"        . "https://stable.melpa.org/packages/")
        ;; ("melpa-tsinghua"      . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ;; ("melpa-ustc"          . "http://mirrors.ustc.edu.cn/elpa/melpa/")
        ;; ("nongnu"              . "https://elpa.nongnu.org/nongnu/")
        ;; ("nongnu-devel"        . "https://elpa.nongnu.org/devel/")
        ;; ("nongnu-ustc"         . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
        ))
;; (setq package-archive-priorities
;; '(
;;         ("elpa"                       . 22)
;;         ("nongnu"                     . 21)
;;         ("gnu"                        . 17)
;;         ("gnu-devel"                  . 18)
;;         ("gnu-tsinghua"               . 50)
;;         ("gnu-ustc"                   . 49)
;;         ("melpa"                      . 51)
;;         ("melpa-stable"               . 14)
;;         ("melpa-tsinghua"             . 48)
;;         ("melpa-ustc"                 . 47)
;;         ("nongnu"                     . 10)
;;         ("nongnu-devel"               . 11)
;;         ("nongnu-ustc"                . 46)
;;         ("jcs-elpa"                   . 7)
;;   )
;; )
;; }}}

;; package: add other source packages to load path
;; {{{
(require 'cl-lib)
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升 Emacs 启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录、 语言相关和版本控制目录都移除
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升 Emacs 启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非 Elisp 语言编写的 Emacs 动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样 Emacs 会从父目录到子目录的顺序搜索 Elisp 插件，顺序反过来会导致 Emacs 无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(add-subdirs-to-load-path (expand-file-name "lib" user-emacs-directory))
;; }}}

;;; early-init.el ends here
