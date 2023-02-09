;; [[file:../package.org::*File Header][File Header:1]]
;; -*- coding: utf-8; lexical-binding: t; -*-
;;; my-package.el

;; DATE 2023-02-09T02:38:27+0800
;; File Header:1 ends here

;; [[file:../package.org::*Emacs Package Database epkg+epkgs][Emacs Package Database epkg+epkgs:1]]
(use-package epkg
  :ensure nil
  :defer t
  ;; :custom (epkg-database-connector 'sqlite-builtin)
  :config
  (setq epkg-repository "~/Documents/GitHub/epkgs")
  (setq package-list-unversioned t) ;; unversioned packages(ibuffer and so on)
  ;; 怎样快速找到 elpa 目录下那些重复的包 - Emacs China
  ;; https://emacs-china.org/t/topic/4244
  (defun my/list-packages-and-versions ()
    "Returns a list of all installed packages and their versions"
    (interactive)
    (mapcar
     (lambda (pkg)
       `(,pkg ,(package-desc-version
                (cadr (assq pkg package-alist)))))
     package-activated-list))
  )

(use-package epkgs
  :ensure nil
  :defer t
  )
;; Emacs Package Database epkg+epkgs:1 ends here

;; [[file:../package.org::*helpful][helpful:1]]
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
  ("C-h h" . #'helpful-at-point) ; Lookup the current symbol at point.
  ([remap describe-command] . helpful-command) ; C-h x
  ([remap describe-function] . helpful-callable)    ; both functions and macros
  ([remap describe-key] . helpful-key) ; C-h k
  ([remap describe-symbol] . helpful-symbol) ; C-h o
  ([remap describe-variable] . helpful-variable) ; C-h v
  ;; :config
  ;; helpful + ivy
  ;; (setq counsel-describe-function-function #'helpful-callable)
  ;; (setq counsel-describe-variable-function #'helpful-variable)
  )
;; }}}
;; helpful:1 ends here

;; [[file:../package.org::*RFC][RFC:1]]
;; RFC
;; {{{
(use-package rfc-mode
  :ensure nil
  :defer t
  :config
  (setq rfc-mode-directory (expand-file-name "~/Documents/GitHub/RFC-all/txt/"))
  (setq rfc-mode-index-path (concat rfc-mode-directory "rfc-index.txt"))
  )
;; }}}
;; RFC:1 ends here

;; [[file:../package.org::*osx-dictionary][osx-dictionary:1]]
;; dictionary: Apple 词典: osx-dictionary
;; {{{
(use-package osx-dictionary
  :ensure nil
  :defer t
  :bind
  ("C-c d f" . osx-dictionary-search-word-at-point) ; DeFine word
  )
;; }}}
;; osx-dictionary:1 ends here

;; [[file:../package.org::*org-mac-link][org-mac-link:1]]
;; org-mac-link
;; {{{
(use-package org-mac-link
  :ensure nil
  :bind
  ("H-i H-i" . org-mac-link-get-link)
  )
;; }}}
;; org-mac-link:1 ends here

;; [[file:../package.org::*环境变量][环境变量:1]]
;; exec-path-from-shell
;; {{{
;; https://emacs-china.org/t/exec-path-from-shell/2515/10
(use-package exec-path-from-shell
  :defer 1
  ;; :if (memq window-system '(mac ns x))
  ;; :when (or (memq window-system '(mac ns x))
  ;;           (unless (memq system-type '(windows-nt dos))
  ;;             (daemonp)))
  :init
  ;; 设成 nil 则不从 .zshrc 读取，只从 .zshenv 读取可以加快速度
  ;; 但需要将环境变量都放到 .zshenv 中，而非 .zshrc 中
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments '("-l" )) ;remove -i read form .zshenv
  (setq exec-path-from-shell-variables '("PATH"))
  :config
  ;; (setq exec-path-from-shell-variables
  ;;       '(
  ;;         "PATH"
  ;;         "MANPATH"
  ;;         "GOROOT"
  ;;         "GOPATH"
  ;;         "EDITOR"
  ;;         "PYTHONPATH"
  ;;         ))

  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))

    (when (daemonp)
      (exec-path-from-shell-initialize)
      )
    )
  )
;; (exec-path-from-shell-initialize)
;; }}}
;; 环境变量:1 ends here

;; [[file:../package.org::*browser-hist][browser-hist:1]]
(use-package browser-hist
  :ensure nil
  :defer t
  :after (sqlite)
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
;; browser-hist:1 ends here

;; [[file:../package.org::*代码分析][代码分析:1]]
(use-package elisp-depmap
  :ensure nil
  :init
  ;; read-symbol-positions-list is deleted from Emacs 29
  (defvar read-symbol-positions-list nil)
  :bind
  (
   ("C-c H-d" . elisp-depmap-graphviz-digraph)
   ("C-c H-g" . elisp-depmap-graphviz)
   ("C-c H-s" . elisp-depmap-makesummarytable)
   )
  :config
  (setq elisp-depmap-parse-hashtablesize 1024)
  (setq elisp-depmap-exec-file
        (expand-file-name "assets/elisp-dep-ana.dot" user-emacs-directory))
  )
;; 代码分析:1 ends here

;; [[file:../package.org::*格式化 elisp-autofmt][格式化 elisp-autofmt:1]]
(use-package elisp-autofmt
  :commands
  (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode)
  :config
  (setq elisp-autofmt-cache-directory
        (expand-file-name "var/elisp-autofmt-cache" user-emacs-directory))
  (setq elisp-autofmt-python-bin "/opt/homebrew/bin/python3")
  )
;; 格式化 elisp-autofmt:1 ends here

;; [[file:../package.org::*快捷键历史记录 keyfreq][快捷键历史记录 keyfreq:3]]
;; keyfreq fork: keyfreq-html-v2 show keyboard heat map
(use-package keyfreq
  :ensure nil
  :defer 1
  :config
  (keyfreq-mode 1) ;; 启动插件包
  (keyfreq-autosave-mode 1) ;; 自动保存模式

  (setq keyfreq-folder
        (expand-file-name
         "lib/keyfreq"
         (concat user-emacs-directory)))

  (setq-default keyfreq-file
                (expand-file-name
                 "assets/keyfreq-log"
                 (concat user-emacs-directory)))

  (setq-default keyfreq-file-lock
                (expand-file-name
                 "var/keyfreq-log.lock"
                 (concat user-emacs-directory)))

  (setq keyfreq-excluded-commands
        '(
          mwheel-scroll
          org-self-insert-command
          self-insert-command
          ))

  ;; (setq keyfreq-excluded-regexp
  ;;       '(
  ;;         "^w3m-"
  ;;        ))

  ;; (defun turnon-keyfreq-mode ()
  ;;   "Turn on keyfreq."
  ;;   (interactive)
  ;;   (my-run-with-idle-timer 4 (lambda () ; lazy load keyfreq
  ;;                               (keyfreq-mode 1)
  ;;                               (keyfreq-autosave-mode 1))))
  )
;; 快捷键历史记录 keyfreq:3 ends here

;; [[file:../package.org::*空闲快捷键 free-keys][空闲快捷键 free-keys:2]]
;; free-keys
;; {{{
(use-package free-keys
  :ensure nil
  :defer 2
  :config
  (setq free-keys-modifiers '(
                              ""
                              "C"     ; Ctrl/Capslock                 + <key>
                              "H"     ; Hyper/right_command           + <key>
                              "M"     ; Meta/ESC/Option               + <key>
                              "s"     ; super/Command                 + <key>
                              "ESC M" ; ESC             ESC             <key>
                              "C-c M" ; C-c             ESC             <key>
                              "C-x M" ; C-x             ESC             <key>
                              "C-H"   ; Ctrl          + right_command + <key>
                              "C-M"   ; Ctrl                          + <key>
                              "C-s"   ; Ctrl          + Command       + <key>
                              "H-M"   ; right_command + Option        + <key>
                              "H-s"   ; right_command + left_command  + <key>
                              ))
  )
;; }}}
;; 空闲快捷键 free-keys:2 ends here

;; [[file:../package.org::*keycast][keycast:1]]
(use-package keycast
  :ensure nil
  :defer t
  :config
  (setq keycast-log-newest-first nil)
  )
;; keycast:1 ends here

;; [[file:../package.org::*which-key + posframe][which-key + posframe:1]]
(use-package which-key
  :ensure nil
  :hook
  (
   (after-init . which-key-mode)
   ;; (repeat-mode . which-key-mode) ; :init (which-key-mode)
   )
  ;; :custom
  ;; for repeat-mode
  ;; Disable the built-in repeat-mode hinting
  ;; (repeat-echo-function 'ignore)
  :init
  ;; for repeat-mode
  ;; Spawn or hide a which-key popup
  ;; (advice-add
  ;;  'repeat-post-hook
  ;;  :after
  ;;  (defun repeat-help--which-key-popup ()
  ;;    (if-let ((cmd (or this-command real-this-command))
  ;;             (keymap (or repeat-map (repeat--command-property 'repeat-map))))
  ;;      (run-at-time
  ;;       0 nil
  ;;       (lambda ()
  ;;         (which-key--create-buffer-and-show nil (symbol-value keymap))))
  ;;      (which-key--hide-popup))))
  :config (setq which-key-popup-type 'minibuffer)
  (setq
   which-key-idle-delay 0.5
   which-key-idle-secondary-delay 0.5
   which-key-show-operator-state-maps t)
  )
;; which-key + posframe:1 ends here

;; [[file:../package.org::*which-key + posframe][which-key + posframe:2]]
(use-package which-key-posframe
  :ensure nil
  :config
  (which-key-posframe-mode)
  ;; (setq which-key-posframe-poshandler
  ;;       'posframe-poshandler-window-top-right-corner)
  )
;; which-key + posframe:2 ends here

;; [[file:../package.org::*repeat-help][repeat-help:1]]
(use-package repeat-help
  :ensure nil
  :hook (repeat-mode . repeat-help-mode)
  :config
  (setq repeat-echo-function 'ignore)
  (setq repeat-help-popup-type 'which-key)
  )
;; repeat-help:1 ends here

;; [[file:../package.org::*multiple-cursors][multiple-cursors:1]]
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
         ([remap cua-rectangle-mark-mode] . set-rectangular-region-anchor)
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
;; multiple-cursors:1 ends here

;; [[file:../package.org::*光标跳转: avy][光标跳转: avy:1]]
(use-package avy
  :ensure nil
  :bind
  ;; ("" . avy-goto-char-timer)
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
;; 光标跳转: avy:1 ends here

;; [[file:../package.org::*右键菜单: embark][右键菜单: embark:1]]
(use-package embark
  :ensure nil
  :defer 1
  :bind
  (
   ;; ("C-<return>" . embark-act)
   ("C-;"        . embark-act)
   ("H-M-o"      . embark-collect) ; equals `ESC H-o'
   ("M-."        . embark-dwim)
   ([remap describe-bindings]  . embark-bindings)
   ;; ("C-h B"      . embark-bindings)
   :map minibuffer-mode-map
   ;; ("C-c C-l" . embark-collect)
   ("H-o"     . embark-export)
   ;; ("C-c C-e" . embark-export)
   ;; ("C-d" . embark-act)
   )
  :custom
  ;; (embark-quit-after-action nil)
  ;; (embark-indicators '(embark-minimal-indicator
  ;;                      embark-highlight-indicator
  ;;                      ;; embark-verbose-indicator
  ;;                      embark-isearch-highlight-indicator
  ;;                      ))
  (embark-cycle-key ".")
  (embark-help-key "?")
  :config
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defvar embark--target-mode-timer nil)
  (defvar embark--target-mode-string "")

  (defun embark--target-mode-update ()
    (setq embark--target-mode-string
          (if-let (targets (embark--targets))
              (format "[%s%s] "
                      (propertize (symbol-name (plist-get (car targets) :type)) 'face 'bold)
                      (mapconcat (lambda (x) (format ", %s" (plist-get x :type)))
                                 (cdr targets)
                                 ""))
            "")))

  (define-minor-mode embark-target-mode
    "Shows the current targets in the modeline."
    :global t
    (setq mode-line-misc-info (assq-delete-all 'embark-target-mode mode-line-misc-info))
    (when embark--target-mode-timer
      (cancel-timer embark--target-mode-timer)
      (setq embark--target-mode-timer nil))
    (when embark-target-mode
      (push '(embark-target-mode (:eval embark--target-mode-string)) mode-line-misc-info)
      (setq embark--target-mode-timer
            (run-with-idle-timer 0.1 t #'embark--target-mode-update))))
  )
;; 右键菜单: embark:1 ends here

;; [[file:../package.org::*embark + consult][embark + consult:1]]
(use-package embark-consult
  :ensure nil
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )
;; embark + consult:1 ends here

;; [[file:../package.org::*Unicode: modeline-char][Unicode: modeline-char:1]]
;; unicode
;; {{{
(use-package modeline-char
  :ensure nil
  :hook
  (after-init . mlc-char-in-mode-line-mode-global)
  )
;; }}}
;; Unicode: modeline-char:1 ends here

;; [[file:../package.org::*括号: puni][括号: puni:1]]
(use-package puni
  :ensure nil
  :defer t
  )
;; 括号: puni:1 ends here

;; [[file:../package.org::*近义词/反义词等: parrot][近义词/反义词等: parrot:1]]
(use-package parrot
  ;; :defer t
  :ensure nil
  :hook (after-init . parrot-mode)
  :bind (
         ;;
         ;; ("H-r k" . parrot-rotate-prev-word-at-point)
         ("H-k H-j" . parrot-rotate-prev-word-at-point)
         ;; ("H-r j" . parrot-rotate-next-word-at-point)
         ("H-k H-k" . parrot-rotate-next-word-at-point)

         )
  :config
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
;; 近义词/反义词等: parrot:1 ends here

;; [[file:../package.org::*中英文空格: pangu-spacing][中英文空格: pangu-spacing:1]]
(use-package pangu-spacing
  :defer 3
  :custom
  (pangu-spacing-inhibit-mode-alist
   '(eshell-mode shell-mode term-mode fundamental-mode))
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor t))
;; 中英文空格: pangu-spacing:1 ends here

;; [[file:../package.org::*字符统计 advance-words-count][字符统计 advance-words-count:1]]
(use-package advance-words-count
  :ensure nil
  ;; :defer 2
  :bind
  ("C-c w c" . advance-words-count)
  )
;; 字符统计 advance-words-count:1 ends here

;; [[file:../package.org::*visual-regexp][visual-regexp:1]]
(use-package visual-regexp
  :ensure nil
  :defer 1
  :bind
  (
   ("C-c r e" . vr/query-replace)
   ("C-c r r" . vr/replace)
   ("C-c m m" . vr/mc-mark)        ; for multiple-cursors
   )
  )
;; visual-regexp:1 ends here

;; [[file:../package.org::*expand-region][expand-region:1]]
;; expand-region
;; {{{
(keymap-global-unset "C-=")
(keymap-global-unset "C--")
(use-package expand-region
  :bind
  (
   ("C-=" . er/expand-region)
   ("C--" . er/contract-region)
   )
  :config
  (defun treesit-mark-bigger-node ()
    (let* ((root (treesit-buffer-root-node))
           (node
            (treesit-node-descendant-for-range
             root (region-beginning) (region-end)))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
        (when-let ((node (treesit-node-parent node)))
          (setq
           node-start (treesit-node-start node)
           node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))

  (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node))
;; }}}
;; expand-region:1 ends here

;; [[file:../package.org::*symbol-overlay][symbol-overlay:1]]
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
        ;; quit
        ("q" . symbol-overlay-remove-all)
        ("<escape>" . symbol-overlay-remove-all)
        ;; jump/move/scope
        ("n" . symbol-overlay-jump-next)
        ("p" . symbol-overlay-jump-prev)
        ("<" . symbol-overlay-jump-first)
        (">" . symbol-overlay-jump-last)
        ("d" . symbol-overlay-jump-to-definition) ; 跳转到定义
        ("e" . symbol-overlay-echo-mark)          ; 撤销上一次跳转
        ("t" . symbol-overlay-toggle-in-scope)    ; 切换高亮范围到作用域
        ;; copy/edit/search/rename/replace
        ("c" . symbol-overlay-save-symbol)        ; 复制当前 symbol
        ("r" . symbol-overlay-rename)
        ("R" . symbol-overlay-query-replace)      ; 查找替换 symbol
        ("s" . symbol-overlay-isearch-literally)  ; 切换为 isearch 并搜索 symbol
        ;; toggle
        ("i" . symbol-overlay-put)                ; 高亮或取消高亮当前 symbol
        )
  )
;; }}}
;; symbol-overlay:1 ends here

;; [[file:../package.org::*wucuo][wucuo:1]]
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
;; wucuo:1 ends here

;; [[file:../package.org::*consult][consult:1]]
;; consult
;; {{{
;; Example configuration for Consult
(use-package consult
  :ensure nil
  :after org
  :defer 1
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind
  (
   ("C-c H-f H-d" . consult-find)
   ("C-c H-k" . consult-mark)
   ("C-c H-l" . consult-line)
   ("C-c H-r H-e" . consult-grep)
   ("C-c H-r H-g" . consult-ripgrep)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap load-theme] . consult-theme)
   ([remap locate] . consult-locate)
   ([remap man] . consult-man)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ;; ([remap apropos] . consult-apropos)
   ;; ;; ("C-x H-l" . consult-focus-lines)
   ;; ;; C-c bindings (mode-specific-map)
   ("C-c M-x" . consult-mode-command)
   ;; ("C-c h" . consult-history)
   ;; ("C-c k" . consult-kmacro)
   ;; ("C-c i" . consult-info)
   ;; ([remap Info-search] . consult-info)
   ;; ;; C-x bindings (ctl-x-map)
   ;; ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
   ;; ;; Custom M-# bindings for fast register access
   ;; ("M-#" . consult-register-load)
   ;; ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
   ;; ("C-M-#" . consult-register)
   ;; ;; Other custom bindings

   ;; ;; M-g bindings (goto-map)
   ;; ("M-g e" . consult-compile-error)
   ;; ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
   ;; ("M-g g" . consult-goto-line)   ;; orig. goto-line
   ;; ("M-g M-g" . consult-goto-line) ;; orig. goto-line
   ;; ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
   ;; ("M-g m" . consult-mark)
   ;; ("M-g k" . consult-global-mark)

   ;; ("M-g I" . consult-imenu-multi)
   ;; ;; M-s bindings (search-map)
   ;; ("M-s d" . consult-find)
   ;; ("M-s D" . consult-locate)
   ;; ("M-s g" . consult-grep)
   ;; ("M-s G" . consult-git-grep)
   ;; ("M-s r" . consult-ripgrep)
   ;; ("M-s l" . consult-line) ; goto line with string
   ;; ("M-s L" . consult-line-multi)
   ;; ("M-s k" . consult-keep-lines)
   ;; ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ;; ("M-s e" . consult-isearch-history)
   ("M-s s" . consult-isearch-history)
   :map org-mode-map
   ([remap imenu]          . consult-org-heading)
   ([remap consult-imenu]  . consult-org-heading)
   ([remap org-goto]       . consult-org-heading) ; C-c C-j
   :map prog-mode-map
   ("C-c C-j"  . consult-outline)
   :map isearch-mode-map
   ([remap isearch-edit-string] . consult-isearch-history)
   ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)  ;; orig. next-matching-history-element
   ("H-h" . consult-history) ;; orig. previous-matching-history-element
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

  (setq consult-narrow-key "<" ;; (kbd "C-+")
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.2)
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;; consult-completion-in-region
  ;; :completion-styles (basic partial-completion flex) ; FIXME
  ;; :cycle-threshold 3
  ;; )

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

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  ;; (setq-default completion-in-region-function
  ;;               (lambda (&rest args)
  ;;                 (apply (if vertico-mode
  ;;                            #'consult-completion-in-region
  ;;                          #'completion--in-region)
  ;;                        args)))
  )
;; }}}
;; consult:1 ends here

;; [[file:../package.org::*consult][consult:3]]
(use-package consult-dir
  :ensure nil
  ;; :after (consult vertico)
  :bind
  (
   ([remap list-directory] . consult-dir)
   :map  vertico-map ; minibuffer-local-completion-map
   ("C-c C-d" . consult-dir)
   ("C-c C-j" . consult-dir-jump-file)
   )
  :config
  (setq consult-dir-default-command 'consult-find)
  )
;; consult:3 ends here

;; [[file:../package.org::*consult][consult:5]]
(use-package consult-yasnippet
  :ensure nil
  ;; :defer t
  ;; :demand t
  :after (consult yasnippet)
  )
;; consult:5 ends here

;; [[file:../package.org::*marginalia][marginalia:1]]
;; marginalia: minibuffer annotations
;; {{{
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure nil
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  ;; :bind (("C-M-a" . marginalia-cycle)
  ;;        :map minibuffer-local-map
  ;;        ("C-M-a" . marginalia-cycle))
  ;; :custom (marginalia-align 'right)
  ;; :init
  ;; The :init configuration is always executed (Not lazy!)
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  ;; (marginalia-mode)
  :hook (after-init . marginalia-mode)
  :config
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
  )
;; }}}
;; marginalia:1 ends here

;; [[file:../package.org::*vertico+posframe][vertico+posframe:1]]
(use-package vertico
  :ensure nil
  :bind
  (
   :map vertico-map
   ([backtab] . vertico-previous)
   ("<tab>" . vertico-insert)    ; Choose selected candidate
   ;; ("<escape>" . minibuffer-keyboard-quit) ; Close minibuffer
   ("<escape>" . vertico-exit)
   :map minibuffer-local-map
   ("M-h" . backward-kill-word)
   )
  :init
  (fido-mode -1)
  (fido-vertical-mode -1)
  (vertico-mode)
  (vertico-mouse-mode)
  ;; (setq vertico-scroll-margin 0) ; Different scroll margin
  (setq vertico-count 20)  ; Show more candidates
  (setq vertico-resize nil) ; Grow and shrink the Vertico minibuffer
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )
;; vertico+posframe:1 ends here

;; [[file:../package.org::*vertico+posframe][vertico+posframe:3]]
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
  (setq enable-recursive-minibuffers t)
  )
;; vertico+posframe:3 ends here

;; [[file:../package.org::*vertico+posframe][vertico+posframe:4]]
(use-package vertico-posframe
  :ensure nil
  :after (vertico posframe)
  :config
  (vertico-multiform-mode 1)
  ;; (setq vertico-multiform-commands
  ;;       '((consult-line
  ;;          posframe
  ;;          (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
  ;;          (vertico-posframe-border-width . 10)
  ;;          ;; NOTE: This is useful when emacs is used in both in X and
  ;;          ;; terminal, for posframe do not work well in terminal, so
  ;;          ;; vertico-buffer-mode will be used as fallback at the
  ;;          ;; moment.
  ;;          (vertico-posframe-fallback-mode . vertico-buffer-mode))
  ;;         (t posframe)))

  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters
        '(
          (left-fringe . 20)
          (right-fringe . 20)
          ))
  )
;; vertico+posframe:4 ends here

;; [[file:../package.org::*YASnippet][YASnippet:1]]
;; yasnippet
;; {{{
(use-package yasnippet
  :ensure nil
  ;; :diminish yas-minor-mode
  :hook
  (after-init . yas-reload-all)
  ;; ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :config
  (yas-global-mode 1)
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
;; YASnippet:1 ends here

;; [[file:../package.org::*基础设置][基础设置:1]]
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
   ;; ("H-b" . pyim-backward-word)
   ;; ("H-f" . pyim-forward-word)
   ("H-c H-c" . pyim-convert-string-at-point) ; 金手指：将字符串转换为中文。
   :map minibuffer-local-map
   ("H-c" . pyim-cregexp-convert-at-point) ; 将光标前字符转换为搜索中文的 regexp.
   :map pyim-mode-map
   ("-"   . pyim-page-previous-page)
   ("+"   . pyim-page-next-page)
   ("H-h" . pyim-page-previous-page)
   ("H-l" . pyim-page-next-page)
   ("H-j" .  pyim-forward-imelem)
   ("H-k" . pyim-backward-imelem)
   )
  :init
  ;; (setq default-input-method "pyim")
  (setq pyim-default-scheme 'quanpin) ; shaungpin/rime
  ;; (setq default-input-method "pyim")
  (setq pyim-page-style 'vertical)
  (setq pyim-page-tooltip '(posframe minibuffer popup))
  (setq pyim-page-length 9)
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
;; 基础设置:1 ends here

;; [[file:../package.org::*输入法切换探针][输入法切换探针:1]]
(use-package pyim
  :ensure nil
  :config
  ;; 探针
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-auto-english
                  pyim-probe-program-mode
                  pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-org-structure-template
                  pyim-probe-org-speed-commands))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  )
;; 输入法切换探针:1 ends here

;; [[file:../package.org::*pinyinlib][pinyinlib:1]]
(use-package pinyinlib
  :ensure nil
  :defer t
  )
;; pinyinlib:1 ends here

;; [[file:../package.org::*ace-pinyin][ace-pinyin:1]]
;; ace-pinyin
;; {{{
(use-package ace-pinyin
  :defer 1
  :config
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode +1)
  )
;; }}}
;; ace-pinyin:1 ends here

;; [[file:../package.org::*orderless][orderless:1]]
;; orderless: minibuffer filter, works with icomplete
;; {{{
(use-package orderless
  :ensure nil
  :init
  ;; basic partial-completion emacs22 substring initials flex
  (setq completion-styles '(basic partial-completion orderless))
  ;; (setq completion-styles
  ;;       '(orderless basic initials substring partial-completion flex)
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil)
  ;; (setq completion-category-overrides
  ;;       '((file
  ;;          (styles basic partial-completion)
  ;;          )))
  (setq orderless-component-separator "[ &]") ; & is for company because space will break completion
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

  ;; https://emacs-china.org/t/consult-ripgrep/23237/9
  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))
  :config
  (add-to-list 'orderless-style-dispatchers #'without-if-bang)
  )
;; }}}
;; orderless:1 ends here

;; [[file:../package.org::*wgrep][wgrep:1]]
(use-package wgrep
  :ensure nil
  :bind
  (
   :map grep-mode-map
   ("C-c C-q" . wgrep-change-to-wgrep-mode)
   )
  :config
  (setq wgrep-auto-save-buffer t)
  )
;; wgrep:1 ends here

;; [[file:../package.org::*ripgrep: deadgrep][ripgrep: deadgrep:1]]
;; deadgrep
;; {{{
(use-package deadgrep
  :ensure nil
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
  (setq deadgrep-max-buffers 1)
  (setq kill-buffer-query-functions nil)

  (defun my/grep-org-files (words)
    (interactive "sSearch(ripgrep) org-roam files: ")
    (let ((default-directory org-roam-directory)
          (deadgrep--file-type '(glob . "*.org"))
          (deadgrep--context '(1 . 1))
          (deadgrep--search-type 'regexp))
      (deadgrep words)
      )
    )

  (setq my/home-directory "~/")
  (defun my/deadgrep-home-dir ()
    (interactive)
    (if (equal major-mode 'dired-mode)
        (setq search-term
              (read-from-minibuffer "Search : "))
      (setq search-term
            (read-from-minibuffer "Search : " (thing-at-point 'symbol)))
      )
    (deadgrep search-term my/home-directory)
    )

  (defun my/deadgrep ()
    (interactive)
    (if (equal major-mode 'dired-mode)
        (setq search-term
              (read-from-minibuffer "Search : "))
      (setq search-term
            (read-from-minibuffer "Search : " (thing-at-point 'symbol)))
      )
    (deadgrep search-term)
    )
  )
;; }}}
;; ripgrep: deadgrep:1 ends here

;; [[file:../package.org::*版本管理: git: Magit][版本管理: git: Magit:1]]
;; difftastic + magit
;; {{{
;; (with-eval-after-load 'magit
(use-package magit
  ;; :defer 1
  :bind
  (
   ("C-x g"   . magit-status)
   ("C-c v g" . magit-status)
   ("H-m H-m" . magit-status)
   :map magit-status-mode-map
   ("#" . my/magit-aux-commands)
   )
  :custom
  (magit-diff-refine-hunk t)
  (magit-module-sections-nested nil)
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
;; 版本管理: git: Magit:1 ends here

;; [[file:../package.org::*文件对比 diff-lisp][文件对比 diff-lisp:1]]
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
;; 文件对比 diff-lisp:1 ends here

;; [[file:../package.org::*vundo][vundo:1]]
(use-package vundo
 :ensure nil
 :bind
 (
  ("H-z" . vundo)
  :map vundo-mode-map
  ;;
  ("j" . vundo-forward) ; f
  ("k" . vundo-backward) ; b
  ;; ("j" . vundo-next)     ; n
  ;; ("k" . vundo-previous) ; p
  ("ESC" . vundo-quit) ("SPC" . vundo-confirm)
  ;; ("a"   . vundo-stem-root)
  ;; ("d"   . vundo--debug)
  ;; ("e"   . vundo-stem-end)
  ;; ("i"   . vundo--inspect)
  ;; ("q"   . vundo-quit)
  ))
;; vundo:1 ends here

;; [[file:../package.org::*书签 Bookmark][书签 Bookmark:1]]
;; binky-mode
;; {{{

;; }}}
;; 书签 Bookmark:1 ends here

;; [[file:../package.org::*posframe][posframe:1]]
(use-package posframe
  :ensure nil
  :defer 1
  ;; posframe-poshandler-window-top-center
  )
;; posframe:1 ends here

;; [[file:../package.org::*header][header:1]]
;; sticky header: topsy
;; {{{
(use-package topsy
  :ensure nil
  :hook (prog-mode . topsy-mode) ;; (add-hook 'prog-mode-hook #'topsy-mode)
  )

;; https://github.com/alphapapa/org-sticky-header/issues/14
(use-package org-sticky-header
  :ensure nil
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'full)
  )
;; }}}
;; header:1 ends here

;; [[file:../package.org::*版本控制][版本控制:1]]
;; diff-hl
;; {{{
(use-package diff-hl
  :ensure nil
  :hook (
         (after-init . global-diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         )
  :bind
  (
   :map diff-hl-mode-map
   ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk)
   )
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  )

;; (global-git-gutter-mode +1) ; BUG/Bad performance when deleting folded 17000+lines
;; }}}
;; 版本控制:1 ends here

;; [[file:../package.org::*region: goggles][region: goggles:1]]
;; goggles: visual hint for operations
;; {{{
(use-package goggles
  :ensure nil
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq goggles-pulse-delay 0.35)
  (setq-default goggles-pulse t) ;; set to nil to disable pulsing
  )
;; }}}
;; region: goggles:1 ends here

;; [[file:../package.org::*括号: highlight-parentheses][括号: highlight-parentheses:1]]
;; highlight-parentheses
;; {{{
(use-package highlight-parentheses
  :defer 1
  ;; :hook (after-init . highlight-parentheses-mode)
  :config
  (setq  highlight-parentheses-colors ; 括号颜色（由内向外）
         '(
           "green1"
           "blue1"
           "orange1"
           "purple1"
           "yellow1"
           "red1"
           ;; "pink" ; only six colors supported ?
           ))
  (global-highlight-parentheses-mode t)
  )

;; Apple Six Colors
;; (setq highlight-parentheses-colors
;; '(
;;   "#61BB46"
;;   "#FDB827"
;;   "#F5821F"
;;   "#E03A3E"
;;   "#963D97"
;;   "#009DDC"
;;   ))
;; }}}
;; 括号: highlight-parentheses:1 ends here

;; [[file:../package.org::*doom-modeline][doom-modeline:1]]
;; doom-modeline
;; {{{
;; (add-hook 'after-init-hook #'doom-modeline-mode)
;; (setq doom-modeline-support-imenu t)
(use-package doom-modeline
  :ensure nil
  ;; :init (doom-modeline-mode 1)
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  :config
  ;; Buffer File Project
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-highlight-modified-buffer-name t)
  (setq doom-modeline-project-detection 'auto) ;auto/project
  ;; Mode
  (setq doom-modeline-major-mode-color-icon (display-graphic-p))
  (setq doom-modeline-minor-modes nil)
  ;; UI
  (setq doom-modeline-height 18)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-window-width-limit 85)
  )
;; }}}
;; doom-modeline:1 ends here

;; [[file:../package.org::*all-the-icons][all-the-icons:1]]
;; all-the-icons
;; {{{
(use-package all-the-icons
  :ensure nil
  ;; :when (display-graphic-p)
  :if (display-graphic-p)
  )

(use-package all-the-icons-completion
  :ensure nil
  :if (display-graphic-p)
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
;; all-the-icons:1 ends here

;; [[file:../package.org::*ts-fold][ts-fold:1]]
;; ts-fold
;; {{{
;; (use-package ts-fold
;;   :ensure nil
;;   )
;; (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode)
;; }}}
;; ts-fold:1 ends here

;; [[file:../package.org::*origami][origami:1]]
;; fold: origami
;; {{{
(add-hook 'prog-mode-hook 'origami-mode)
(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)
  )
;; }}}
;; origami:1 ends here

;; [[file:../package.org::*ace-window][ace-window:1]]
;; {{{ ace-window
;; (require 'ace-window)
;; (keymap-global-set "H-w H-w" #'ace-window)
(use-package ace-window
  :ensure nil
  :bind
  ("H-w H-w" . ace-window)
  :config
  (setq aw-keys '(?e ?a ?s ?d ?f ?g ?h ?j ?k ?l ?v ?n))
  )
;; }}}
;; ace-window:1 ends here

;; [[file:../package.org::*org-modern][org-modern:1]]
(use-package org-modern
  :hook (org-mode . org-modern-mode)
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
  )
;; org-modern:1 ends here

;; [[file:../package.org::*org-auto-tangle][org-auto-tangle:1]]
;; org-auto-tangle
;; {{{
(use-package org-auto-tangle
  :ensure nil
  :hook (org-mode . org-auto-tangle-mode)
  )
;; }}}
;; org-auto-tangle:1 ends here

;; [[file:../package.org::*basic config][basic config:1]]
;; org-roam: basic config
;; {{{
(use-package org-roam
  ;; :after (org)
  :defer 2
  :init
  (setq org-roam-directory "~/org-roam")
  (setq org-roam-db-location "~/org-roam/org-roam.db")
  :bind
  (
   :map org-mode-map
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
   ("C-c n s" . org-roam-db-sync)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n T" . org-roam-tag-remove)
   )
  :config
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-mode-sections
        '((org-roam-backlinks-section :unique t)
          org-roam-reflinks-section
          org-roam-unlinked-references-section))
  ;; (setq org-roam-completion-everywhere t)
  (setq org-roam-file-extensions '("org" "md")) ;; enable Org-roam for markdown
  ;; (setq org-roam-node-display-template "${title:50} ${tags:30}")
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))

  (require 'org-roam-protocol)  ;; org-roam-protocol

  (org-roam-db-autosync-mode)
  ;; (org-roam-db-autosync-mode 1) ;; if md-roam installed, move to md-roam config

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  )
;; }}}
;; basic config:1 ends here

;; [[file:../package.org::*tag][tag:1]]
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
;; tag:1 ends here

;; [[file:../package.org::*template][template:1]]
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
;; template:1 ends here

;; [[file:../package.org::*UI & ORUI][UI & ORUI:1]]
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
;; UI & ORUI:1 ends here

;; [[file:../package.org::*database][database:1]]
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
;; database:1 ends here

;; [[file:../package.org::*consult-org-roam][consult-org-roam:1]]
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
;; consult-org-roam:1 ends here

;; [[file:../package.org::*org-similarity][org-similarity:1]]
;; org-similarity
;; {{{
(use-package org-similarity
  :ensure nil
  :after (org)
  :defer t
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
;; org-similarity:1 ends here

;; [[file:../package.org::*md-roam][md-roam:1]]
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
;; md-roam:1 ends here

;; [[file:../package.org::*markdown][markdown:1]]
;; markdown-mode
;; {{{
(use-package markdown-mode
  :ensure nil
  :defer t
  :commands
  (markdown-mode gfm-mode)
  :mode
  (
   ("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . markdown-mode)
   )
  :init
  (setq markdown-command "pandoc")
  :config
  (setq visual-line-column 90)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-wiki-links t) ;; wikilink/backlink
  (setq markdown-wiki-link-search-type "project")
  (setq markdown-enable-math t)
  )
;; }}}
;; markdown:1 ends here

;; [[file:../package.org::*ekg][ekg:1]]

;; ekg:1 ends here

;; [[file:../package.org::*必应词典: bing-dict][必应词典: bing-dict:1]]
(use-package
  bing-dict
  :ensure nil
  :bind ("C-c d b" . bing-dict-brief)
  :config (setq bing-dict-vocabulary-save t)
  (setq bing-dict-vocabulary-file
        (expand-file-name "assets/vocabulary.org" (concat user-emacs-directory)))
  (setq bing-dict-pronunciation-style 'us) ; us uk
  (setq bing-dict-show-thesaurus 'both) ; synonym and antonym

  (defun my/open-vocabulary-file () ;; Emacs init
    (interactive)
    (find-file-other-window bing-dict-vocabulary-file))
  )
;; 必应词典: bing-dict:1 ends here

;; [[file:../package.org::*elfeed][elfeed:1]]
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
  (setq rmh-elfeed-org-files
        (list (expand-file-name "assets/elfeed.org" user-emacs-directory)))
  )
;; }}}
;; elfeed:1 ends here

;; [[file:../package.org::*shrface][shrface:1]]
;; shrface eww nov
;; {{{
(use-package shrface
  :defer t
  :hook (eww-after-render-hook . shrface-mode)
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t)
  )
;; }}}
;; shrface:1 ends here

;; [[file:../package.org::*nyan-mode][nyan-mode:1]]
(use-package nyan-mode
  :ensure nil
  :defer 3
  :hook (parrot-mode . nyan-mode)
  :config
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  )
;; nyan-mode:1 ends here

;; [[file:../package.org::*auto-dark][auto-dark:1]]
;; auto-dark
;; {{{
(use-package auto-dark
  :ensure nil
  :defer 3
  :if (display-graphic-p)
  ;; :init (auto-dark-mode t)
  ;; :hook (after-init . auto-dark-mode)
  :config
  (setq auto-dark-allow-osascript t
        auto-dark-dark-theme 'solarized-dark)
  )
;; }}}
;; auto-dark:1 ends here

;; [[file:../package.org::*Graphviz][Graphviz:1]]
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
;; Graphviz:1 ends here

;; [[file:../package.org::*D2][D2:1]]
(use-package d2-mode
  :ensure nil
  :defer t
  ;; :bind
  ;; (
  ;;  :map d2-mode-map
  ;;  )
  )
;; D2:1 ends here

;; [[file:../package.org::*image-roll][image-roll:1]]
;; image-roll
;; {{{
(use-package image-roll
  :defer t
  :ensure nil
  )
;; }}}
;; image-roll:1 ends here

;; [[file:../package.org::*地图 map][地图 map:1]]
;; osm: OpenStreetMap
;; {{{
(use-package osm
  :defer t
  ;; :after org
  :init
  ;; Load Org link support
  ;; (with-eval-after-load 'org
  ;;   (require 'osm-ol))
  :bind (
         ("C-c m h" . osm-home)
         ("C-c m s" . osm-search)
         ("C-c m v" . osm-server)
         ("C-c m t" . osm-goto)
         ("C-c m x" . osm-gpx-show)
         ("C-c m j" . osm-bookmark-jump)
         :map osm-mode-map
         ("q" . (lambda () (interactive) (quit-window t)))
         )
  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information
  )
;; }}}
;; 地图 map:1 ends here

;; [[file:../package.org::*empv: 视频播放控制][empv: 视频播放控制:1]]
(use-package empv
  :ensure nil
  :demand t
  :defer t
  )
;; empv: 视频播放控制:1 ends here

;; [[file:../package.org::*subed][subed:1]]
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
;; subed:1 ends here

;; [[file:../package.org::*mybigword][mybigword:1]]
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
;; mybigword:1 ends here

;; [[file:../package.org::*olivetti][olivetti:1]]
(use-package
  olivetti
  :hook
  (
   (after-init . olivetti-mode)
   ;; (window-configuration-change . my/toggle-olivetti-for-org)
   )
  :bind ("H-v H-v" . olivetti-mode)
  :init
  (setq olivetti-body-width 90) ; default: fill-column+2

  ;; https://emacs-china.org/t/emacs/19797/4
  (defun my/toggle-olivetti-for-org ()
    "if current buffer is org and only one visible buffer
  enable olivetti mode"
    (interactive)
    (if (and (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
             (or (eq (length (window-list nil nil nil)) 1)
                 (window-at-side-p (frame-first-window) 'right))) ;; frame-first-window 的 mode 是 org-mode 并且没有右边 window
        (olivetti-mode 1)
      (olivetti-mode 0)
      (when (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
        (visual-line-mode 1)))))
;; olivetti:1 ends here

;; [[file:../package.org::*nov-xwidget][nov-xwidget:1]]
(use-package nov-xwidget
  :ensure nil
  :demand t
  :after nov
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))
;; nov-xwidget:1 ends here

;; [[file:../package.org::*File End][File End:1]]
;; 2023-02-09T02:34:54+0800

(provide 'my-package)

;;; my-package.el ends here.
;; File End:1 ends here
