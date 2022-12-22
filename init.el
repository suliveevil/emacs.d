;; -*- origami-fold-style: triple-braces -*-
;; init

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


;; completion: dabbrev: dynamic abbreviation expand
;; {{{
(keymap-global-set "C-<tab>"               #'dabbrev-expand)
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
(setq make-backup-files t ; backup of a file the first time it is saved.
      backup-by-copying t ; don't clobber symlinks
      version-control t	  ; version numbers for backup files
      delete-old-versions t	 ; delete excess backup files silently
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
(defun open-init-file() ;; Emacs init
  (interactive)
  (find-file "~/.config/emacs/init.el"))
;; (keymap-global-set "<f2>" #'open-init-file)
;;
(defun open-early-init-file() ;; Emacs early-init
  (interactive)
  (find-file "~/.config/emacs/early-init.el"))
;; (keymap-global-set "<f2>" #'open-early-init-file)
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

;; org-auto-tangle
;; {{{
(require 'org-auto-tangle)
(add-hook 'org-mode-hook 'org-auto-tangle-mode)
;; }}}

;; magit + git-gutter
(global-git-gutter-mode +1)

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

;; profile: benchmark-init
;; {{{
(require 'benchmark-init-modes)
(require 'benchmark-init)
(benchmark-init/activate)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)
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

;; ace-pinyin
;; {{{
(require 'ace-pinyin)
(setq ace-pinyin-use-avy t)
(ace-pinyin-global-mode +1)
;; }}}

;; package config
;; {{{
;; (add-to-list 'load-path (expand-file-name "init-package.el"  (concat user-emacs-directory))) ;; :FIXME:
;; (add-to-list 'load-path "~/.config/emacs/init-package.el")
;; (require 'init-package) ;; packages installed by package.el
;; }}}


;; fold: origami
;; {{{
(add-hook 'prog-mode-hook 'origami-mode)
(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)
  )
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
;; (require 'orderless)
;; (setq completion-styles '(orderless basic)
;;       completion-category-defaults nil
;;       completion-category-overrides '((file (styles basic partial-completion))))
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

;; org-roam: basic config
;; {{{
(use-package org-roam
  :ensure t
  :defer t ;; autoload
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today) ;; Dailies
         (:map org-mode-map
               ("C-c n i" . org-roam-node-insert)
               ("C-c n o" . org-id-get-create)
               ("C-c n t" . org-roam-tag-add)
               ("C-c n a" . org-roam-alias-add)
               ("C-c n m". completion-at-point)
               ("C-c n l" . org-roam-buffer-toggle))
         )
  :config
  ;; If you're using a vertical completion framework,
  ;; you might want a more informative completion interface
  ;; file-truename is optional
  ;; it seems required when use symbolic links, which Org-roam does not resolve
  (setq org-roam-file-extensions '("org" "md")) ;; enable Org-roam for markdown
  (require 'org-roam-protocol)                  ;; org-roam-protocol
  )
;; }}}

;; org-roam: backlink count & node hierarchy
;; {{{
;; https://github.com/Jousimies/.emacs.d/blob/master/lisp/init-roam.el
(require 'org)
(require 'org-roam)
;;
(cl-defmethod org-roam-node-directories ((node org-roam-node))
  (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (format "(%s)" (car (split-string dirs "/")))
    ""))
;;
(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node)))))
    (format "[%d]" count)))
;;
(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  (let ((level (org-roam-node-level node)))
    (concat
     (when (> level 0) (concat (org-roam-node-file-title node) " > "))
     (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
     (org-roam-node-title node))))
;;
(setq org-roam-node-display-template
      "${directories:10} ${title:50} ${hierarchy:*} ${backlinkscount:6} ${tags:50}")
;;
;; (setq org-roam-node-display-template (concat
;;                                    "${type:8} ${backlinkscount:3} ${hierarchy:*}"
;;                                   (propertize "${tags:20}" 'face 'org-tag) " "))
;;
;; (setq org-roam-node-display-template "${hierarchy:*} ${tags:20}")
;;
;; (setq org-roam-node-display-template
;;       (concat "${title:*} "
;;               (propertize "${tags:10}" 'face 'org-tag)
;;               ))
;; }}}

;; org-roam: completion
;; {{{
(setq org-roam-completion-everywhere t)
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

;; org-roam: template,  id (uuid) timestamps and so on
;; {{{
(setq org-roam-capture-templates
      '(
        ;; a: audio & music
        ;; b: book
        ;; c:
        ("d" "default" plain "%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+category:\n#+filetags:\n")
         ;; #+date: %<%Y-%m-%d-%H:%M:%S %Z>\n
         :empty-lines 1
         :immediate-finish t
         :kill-buffer t
         :unnarrowed  t)
        ;; ("e" "emacs" plain "%?"
        ;;  :target (file+head "${title}.org"
        ;;                     "#+title: ${title}\n#+category:\n#+filetags: \n")
        ;;  :unnarrowed  t)
        ;; f:
        ;; g:
        ;; h: human
        ;; i:
        ;; j:
        ;; k:
        ;; l:
        ;; m:
        ;; n:
        ;; o:
        ;; p: project
        ;; q:
        ("r" "reference" plain "%? \n %(v-i-or-nothing) \n\n%(v-a-or-nothing)"
         :target
         (file+head "references/%<%Y%m%d%H%M%S>-${title}.org"
                    "#+title: ${title}\n")
         :unnarrowed t)
        ;; s:
        ;; t: topic todo
        ("t" "topic" plain "%?"
         :target (file+head "topics/${title}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ;; u:
        ;; v:
        ;; w:
        ;; x:
        ;; y:
        ;; z:
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
     (format "open \"/Applications/DB Browser for SQLite.app\" --args --table nodes %s" org-roam-db-location)))
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
(org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active
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
(setq keyfreq-folder "~/.config/emacs/lib/keyfreq")
(require 'keyfreq)        ;; 导入插件包
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



;; init.el
