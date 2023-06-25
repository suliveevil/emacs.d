(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dark-allow-osascript t)
 '(auto-dark-dark-theme 'solarized-dark)
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "swy-M1")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("59820d0ca7ba29984ab4d255f9fcfaa28041ad6ae4b6ae2ffb2ccfbfaec210e4" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(describe-char-unidata-list
   '(name old-name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored iso-10646-comment uppercase lowercase titlecase))
 '(elfeed-feeds
   '("https://oylenshpeegul.gitlab.io/blog/index.xml" "https://takeonrules.com/index.xml" "https://andersmurphy.com/feed.xml" "https://jarodise.com/rss.xml" "https://www.drdobbs.com/rss/all" "http://www.robalni.org/links/links.rss" "http://www.robalni.org/posts/posts.rss" "https://www.swift.org/atom.xml" "https://thenewstack.io/feed/" "https://oremacs.com/atom.xml" "https://hsingko.github.io/post/index.xml" "https://www.scanbuf.net/index.xml" "https://scripter.co/index.xml" "https://xenodium.com/rss.xml" "https://andreyorst.gitlab.io/feed.xml" "https://sam217pa.github.io/index.xml" "http://blog.binchen.org/rss.xml" "https://eason0210.github.io/index.xml" "https://blog.einval.eu/index.xml" "https://emacsformacosx.com/atom/pretest" "https://emacsredux.com/atom.xml" "https://emacsrocks.com/atom.xml" "https://emacstil.com/feed.xml" "https://emacsair.me/feed.xml" "https://pinecast.com/feed/emacscast" "https://emacsconf.org/index.rss" "https://emacstalk.codeberg.page/podcast/index.xml" "https://emacstalk.github.io/podcast/index.xml" "https://qiita.com/tags/emacs/feed" "https://kisaragi-hiu.com/index.xml" "https://hongchao.me/feed.xml" "https://oracleyue.github.io/index.xml" "https://impaktor.gitlab.io/index.xml" "https://andschwa.com/index.xml" "https://yqrashawn.com/feed.rss" "http://johnbokma.com/index.rss" "https://kristofferbalintona.me/index.xml" "https://blog.lambda.cx/index.xml" "https://manateelazycat.github.io/feed.xml" "https://www.masteringemacs.org/feed" "https://meliache.de/index.xml" "https://lucidmanager.org/categories/productivity//index.xml" "https://www.murilopereira.com/index.xml" "https://archive.casouri.cc/note/rss.xml" "https://nullprogram.com/feed/" "https://blog.phundrak.com/index.xml" "https://planet.emacslife.com/atom.xml" "https://blog.markhepburn.com/posts/index.xml" "https://pragmaticemacs.wordpress.com/feed/" "https://sachachua.com/blog/feed/" "https://sanemacs.com/updates.xml" "https://emacs.dyerdwelling.family/index.xml" "https://joy.pm/index.xml" "https://kitchingroup.cheme.cmu.edu/blog/category/emacs/feed/" "https://www.loopinsight.com/feed/" "https://www.dr-qubit.org/rss/all.xml" "https://www.dr-qubit.org/rss/computing-code.xml" "https://tsdh.org/rss.xml" "https://amodernist.com/all.atom" "https://whhone.com/index.xml" "https://ag91.github.io/rss.xml" "https://wikemacs.org/api.php?hidebots=1&days=7&limit=50&action=feedrecentchanges&feedformat=atom" "https://www.wilfred.me.uk/rss.xml" "https://www.wisdomandwonder.com/feed" "https://christiantietze.de/feed.atom" "https://wwwtech.de/articles.atom" "http://xahlee.info/emacs/emacs/blog.xml" "https://www.zwitterio.it/en/rss.xml" "https://utgd.net/feed" "http://brett.trpstra.net/brettterpstra" "https://matthewcassinelli.com/feed/?_ga=2.193772968.469924047.1671400200-1960225928.1671400200&_gl=1%2Acb3ql5%2A_ga%2AMTk2MDIyNTkyOC4xNjcxNDAwMjAw%2A_ga_WD1DZDGBRR%2AMTY3MTQwMDE5OS4xLjEuMTY3MTQwMDI5OS4zMy4wLjA." "https://mjtsai.com/blog/feed/" "https://osxdaily.com/feed/" "https://feedpress.me/sixcolors?type=xml" "https://bohutang.me/atom.xml" "https://weekly.howie6879.cn/rss/rss.xml" "https://www.ruanyifeng.com/blog/atom.xml" "https://www.zhihu.com/rss" "https://cate.blog/feed/" "https://leancrew.com/all-this/feed/" "https://beckyhansmeyer.com/feed/" "https://c-for-dummies.com/blog/?feed=rss2" "https://www.thisiscolossal.com/feed/" "https://daringfireball.net/feeds/json" "https://endlessparentheses.com/atom.xml" "https://ericasadun.com/feed/" "https://furbo.org/feed/json" "https://www.hopperapp.com/rss/changelog.php" "https://inessential.com/feed.json" "https://github.com/SwiftOldDriver/iOS-Weekly/releases.atom" "http://ithare.com/feed/" "https://itsfoss.com/feed/" "https://jvns.ca/atom.xml" "https://karthinks.com/index.xml" "https://liujiacai.net/atom.xml" "http://feeds.kottke.org/json" "http://macshuo.com/?feed=rss2" "https://www.manton.org/feed/json" "https://endler.dev/rss.xml" "https://vincode.io/feed.xml" "https://nnw.ranchero.com/feed.json" "https://onefoottsunami.com/feed/json/" "https://opensource.com/feed" "https://github.com/suliveevil.private.atom?token=AIQ3IJNUVTB2YHHNUONL2PWBI56EE" "https://rosemaryorchard.com/feed.xml" "http://rss.slashdot.org/slashdot/slashdotMain" "https://this-week-in-rust.org/rss.xml" "https://www.tweaking4all.com/feed/" "https://blog.vivekhaldar.com/rss" "https://www.skywind.me/blog/feed" "http://feeds.vimgolf.com/latest-challenges"))
 '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(package-selected-packages
   '(modern-fringes htmlize wgrep-deadgrep timu-macos-theme repeat-help consult-dir nyan-mode bing-dict consult-yasnippet consult-eglot zones empv ebib esup rainbow-mode visual-regexp-steroids visual-regexp wgrep puni embark-consult elisp-depmap khoj ace-pinyin ace-window all-the-icons-completion all-the-icons-dired applescript-mode benchmark-init browser-hist closql color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow consult-org-roam csv-mode d2-mode deadgrep diff-hl doom-modeline el-fetch electric-pair electric-pair-mode elfeed elfeed-dashboard elfeed-org elisp-demos embark epkg exec-path-from-shell expand-region free-keys fuck goggles goto-line-preview helpful highlight-parentheses keycast llama lsp-bridge magit-delta markdown-mode mermaid-mode multiple-cursors mybigword nov nov-xwidget olivetti opencc orderless org-auto-tangle org-modern org-roam org-roam-ui org-similarity org-sticky-header origami osx-dictionary pangu-spacing parrot pcre2el pyim request rfc-mode semantic-mode shrface simple-httpd sis solarized-theme subed symbol-overlay topsy transient ts-fold vertico vertico-posframe vundo websocket which-key which-key-posframe with-editor wucuo yasnippet))
 '(pyim-dicts nil)
 '(safe-local-variable-values
   '((eval setq-local org-roam-db-location
	   (expand-file-name "org-roam.db" org-roam-directory))
     (eval setq-local org-roam-directory
	   (expand-file-name
	    (locate-dominating-file default-directory ".dir-locals.el")))
     (org-roam-db-location expand-file-name "./org-roam.db")
     (org-roam-directory expand-file-name ".")
     (origami-fold-style . triple-braces))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "grey")))))
