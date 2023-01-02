;; -*- origami-fold-style: triple-braces -*-
;; early-init.el --- Emacs 27+ pre-initialisation config
;; Code loaded before the package system and GUI is initialized.

;; profile
;; {{{
;; M-x profiler-start
;; M-x profiler-report
;; profiler-report-render-calltree
(defconst my/before-load-init-time (current-time))

;;;###autoload
(defun my/load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time my/before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) my/before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))
(add-hook 'after-init-hook #'my/load-init-time t)

(defvar my/tick-previous-time my/before-load-init-time)

;;;###autoload
(defun my/tick-init-time (msg)
  "Tick boot sequence at loading MSG."
  (when my/loading-profile-p
    (let ((ctime (current-time)))
      (message "---- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my/tick-previous-time)))
               msg)
      (setq my/tick-previous-time ctime))))

(defun my/emacs-init-time ()
  "Emacs booting time in msec."
  (interactive)
  (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))

(add-hook 'after-init-hook #'my/emacs-init-time)
;; }}}

(setq debug-on-error t)

(setq use-short-answers t) ;; use y/n instead of yes/no
(setq confirm-kill-emacs (lambda (prompt) (y-or-n-p-with-timeout "确认退出？" 10 "y")))

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
(set-language-environment               "UTF-8")     ;; System default coding
(prefer-coding-system                   'utf-8)      ;; prefer
(set-buffer-file-coding-system          'utf-8-unix) ;;
(set-charset-priority                   'unicode)    ;;
(set-clipboard-coding-system            'utf-8-unix) ;; clipboard
(set-default-coding-systems             'utf-8)	;; buffer/file: 打开文件时的默认编码
(set-file-name-coding-system            'utf-8-unix)	;; unix/linux/macos
(set-keyboard-coding-system             'utf-8-unix)	;; keyboard
(set-next-selection-coding-system       'utf-8-unix)	;; selection
(set-selection-coding-system            'utf-8)		;; selection
(set-terminal-coding-system             'utf-8-unix)		;; terminal
(setq coding-system-for-read            'utf-8)			;;
(setq default-buffer-file-coding-system 'utf-8)				;;
(setq locale-coding-system              'utf-8)	;; local
;; }}}

;; locale
;; {{{
(setq system-time-locale "C")
;; }}}

;; UI
;; {{{
(push '(fullscreen . maximized) default-frame-alist)
(setq inhibit-splash-screen t)        ;; 禁用欢迎界面
(tool-bar-mode -1)          ;; 工具栏
(set-fringe-mode 10)        ;;
(global-visual-line-mode 1) ;;
(setq visible-bell t)	    ;; 关闭提示声音
;; }}}


;; symlink and version control
;; {{{
(setq vc-follow-symlinks t)
;; }}}

;; minibuffer
;; {{{
(setq enable-recursive-minibuffers t)
;; }}}

;; email
;; {{{
(setq user-mail-address "suliveevil@qq.com")
(setq user-full-name "suliveevil")
;; }}}

;; package: package-enable-at-startup is before init but after early-init
;; {{{
(setq package-enable-at-startup nil) ;; don't enable at startup, pair with (package-initialize)
;; }}}

;; package: add other source packages to load path
;; {{{
(require 'cl-lib)
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
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
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(add-subdirs-to-load-path "~/.config/emacs/lib")
;; }}}
