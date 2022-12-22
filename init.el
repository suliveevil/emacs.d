;; init

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
;; }}}

;; package config
;; {{{
;; (add-to-list 'load-path (expand-file-name "init-package.el"  (concat user-emacs-directory))) ;; :FIXME:
;; (add-to-list 'load-path "~/.config/emacs/init-package.el")
;; (require 'init-package) ;; packages installed by package.el
;; }}}

;; package out of package.el :FIXME:
;; {{{
;; (add-to-list 'load-path (expand-file-name "init-lib.el" user-emacs-directory)) ;; :FIXME:
;; (add-to-list 'load-path "~/.config/emacs/init-lib.el")
;; (require 'init-lib)     ;; packages out of package.el
;; }}}



;; init.el
