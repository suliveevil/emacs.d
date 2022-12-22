;; -*- origami-fold-style: triple-braces -*-
;; early-init.el --- Emacs 27+ pre-initialisation config
;; Code loaded before the package system and GUI is initialized.

(setq debug-on-error t)

(setq use-short-answers t) ;; use y/n instead of yes/no
(setq confirm-kill-emacs (lambda (prompt) (y-or-n-p-with-timeout "确认退出？" 10 "y")))


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
(tool-bar-mode -1)          ;; 工具栏
(set-fringe-mode 10)        ;;
(global-visual-line-mode 1) ;;
(setq visible-bell t)	    ;; 关闭提示声音
;; }}}


;; version control
;; {{{
(setq vc-follow-symlinks t)
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

;; symlink and version control
;; {{{
(setq vc-follow-symlinks t)
;; }}}
