;; [manateelazycat.github.io/2022-11-18-write-emacs-plugin.md](https://github.com/manateelazycat/manateelazycat.github.io/blob/master/_posts/2022-11-18-write-emacs-plugin.md)

;; {{{ 通过外部命令行工具扩展 Emacs
(defun my-first-elisp-code ()
  (interactive)
  (message "%s" (shell-command-to-string "ls"))) ;; 同步执行的，数据量大会卡住 Emacs
;; M-x load-file
;; M-x my-first-elisp-code
;; }}}

;; {{{ 读取输入
(defun interactive-example (file)
  (interactive "fRead file: ")
  (message "Hello %s" file))
;; }}}

(with-temp-buffer
  (insert "hello")
  (buffer-string))
