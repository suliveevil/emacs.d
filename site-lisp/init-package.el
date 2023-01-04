;;; init-packge.el
;; packages (installed by package.el) configuration
;; siblings: init.el init-lib.el 



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
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  )
;; }}}


;; org-roam: basic config
;; {{{
(use-package org-roam
  :ensure t
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
  :ensure t
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
