;;; build-emacs.el --- Build custom Emacs from sources  -*- lexical-binding: t; -*-

;; https://gist.github.com/amno1/52ae2ec4fecfe720998270795f9bc89b

;; Copyright (C) 2022  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Backup of my personal script to (re)build Emacs from within Emacs.
;; It will automatically pull emacs sources from git, apply custom patches,
;; and install a symlink to newly build Emacs in ~/.config/.

;; This is very opinionated and personal setup, don't use for anything but
;; inspriation :) or use on your own risk.

;; Run from the source directory; don't install Emacs. Add symlink/src and
;; symlink/lib-src to the PATH (for emacs and emacs-client executable). Symlink
;; is installed by the script in ~/.config/ directory (no sudo pass required). 

;; It will create a worktree for a new build for the purpose of keeping the git
;; repository clean of any accidental changes so we can "git pull" without
;; git asking for eventual intervention in case of unstashed changes etc.

;; The old build is not removed automatically; for the reason of not being left
;; with buggy setup in case of breaking changes and alike. Symlink will
;; be changed, because in most of cases, new Emacs works, but just in case;
;; symlink is easy to revert to previous build. Harddrive space is cheap; I
;; typically execute emacs-clean-auto-worktrees after I have built Emacs several
;; times.

;; Worktrees also let us have several branches checked out at the same time, and
;; I do have some custom patches I use in my build. Those are placed in
;; "patches" directory and applied automatically at build time. Custom patched
;; worktrees are not removed by emacs-clean-auto-worktrees.

;; Typically use emacs-build-async. For debugging purposes use
;; emacs-build. Modify configuration to suit your needs. There are also some
;; hardcoded make option, modifty for your needs if used.

;; This is work in progress, done by me from time to time, when I get annoyed by
;; having to repeat som actions on each time I compile Emacs. This is not
;; actively developed!

;; Shell script should be able to bootstrap too, without need to have working
;; Emacs, and certainly less verbose then an elisp program, but stepping through
;; the code is indispensible when debugging. If someone finds this interesting
;; and translates to a shell script, please let me know.

;;; Code:

(defvar emacs-cflags-dbg "-gddb3 -O0 -march=native")
(defvar emacs-cflags-opt "-O2 -march=native")
(defvar emacs-configs
  '(("default-with-native" "--with-native-compilation")
    ("no-gtk-with-cairo-and-native"
     "--with-native-compilation"
     "--with-x"
     "--with-x-toolkit=no"
     "--without-gconf"
     "--without-gsettings"
     "--with-cairo"
     "--without-toolkit-scroll-bars"
     "--with-xinput2"
     "--without-included-regex"
     "--without-compress-install")))

(defvar build-log "*build-log*")

(defun build-log ()
  (and (get-buffer build-log)
      (kill-buffer build-log))
  (generate-new-buffer "*build-log*")
  (with-current-buffer build-log
    (special-mode)
    (read-only-mode -1)
    (current-buffer)))

(defun git (&rest args)
  (apply #'call-process "git" nil build-log nil args))
(defun emq (&rest args)
  (apply #'call-process "emacs" nil build-log nil "-Q" args))
(defun make (&rest args)
  (apply #'call-process "make" nil build-log nil args))
(defun lns (&rest args)
  (apply #'call-process "ln" nil build-log nil "-s" args))
(defun autogen (&rest args)
  (apply #'call-process (expand-file-name "./autogen.sh") nil build-log nil args))
(defun configure (&rest args)
  (apply #'call-process (expand-file-name "./configure") nil build-log nil args))

(defun git-root (&optional directory)
  "Find git root in a git project.
If DIRECTORY is not given, return git root of DEFAULT-DIRECTORY.
This should correctly find git root even for worktrees placed outside of a git
repository as well as in subdirectories in main git repository."
  (let* ((directory (expand-file-name (or directory default-directory)))
         (git-dir (locate-dominating-file directory ".git"))
         (git-file (and git-dir (expand-file-name ".git" git-dir))))
    (if (and git-file (file-directory-p git-file))
          git-dir
      (and git-file
           (with-temp-buffer
             (insert-file-contents-literally git-file)
             (when (search-forward "gitdir: " nil t)
               (let ((beg (point))
                     (end (search-forward ".git" nil t)))
                 (buffer-substring-no-properties beg (- end 4)))))))))

(defun autobuildp (name)
  (catch 'auto
   (dolist (build emacs-configs)
     (if (string-match-p (car build) name)
         (throw 'auto t)))))

(defun dirp (string) (= ?/ (aref string 0)))

(defun emacs-clean-auto-worktrees ()
  "Remove auto-generated worktrees from configs. This will remove ALL but
the current one."
  (interactive)
  (let ((git-root (git-root source-directory))
        (currtree (file-truename (expand-file-name "~/.config/emacs/"))))
    (when git-root
      (cd git-root)
      (let ((out
             (cl-remove-if-not
              #'autobuildp
              (cl-remove-if-not
               #'dirp
               (split-string
                (shell-command-to-string "git worktree list"))))))
        (dolist (tree out)
          (unless (string-match-p tree currtree)
            (let ((worktree (expand-file-name
                             (concat "../" (file-name-nondirectory tree)))))
              (message "Removing: %s" worktree)
              ;; these builds are not used for patching; all changes are misstakes
              (git "worktree" "remove" "--force" worktree))))))))

(defun emacs-download-and-build (configname)
  (let* ((config (assoc configname emacs-configs))
         (build-name (car config))
         (config (cdr config))
         (git-root (git-root source-directory))
         (worktree
          (format "../%s-%s" build-name (format-time-string "%y%m%d-%H%M%S")))
         (status "Emacs build failed.")
         (build-log (build-log)))    
    (cd git-root)
    (git "pull")
    (git "worktree" "add" worktree)
    (when (file-directory-p worktree)
      (let* ((worktree (concat worktree "/"))
             (link (expand-file-name "~/.config/emacs"))
             (target (expand-file-name "src/emacs" worktree)))
        (cd worktree)
        (let ((patches (directory-files "../patches/" t "patch" t)))
          (dolist (patch patches) (git "apply" patch)))
        (autogen)
        (apply #'configure config)
        (make "bootstrap" "-j8")
        (make "-j8")
        (when (file-executable-p target)
          (if (file-exists-p link) (delete-file link))
          (lns (expand-file-name worktree) link))))
    (with-current-buffer build-log
      (goto-char (point-max))
      (insert status)
      (write-file (concat (file-name-nondirectory worktree) ".log")))))

(defun emacs-patch (&optional patch-name)
  "Add new Emacs patch."
  (interactive "sPatch name: ")
  (message "Patch name %s" patch-name)
  (let ((pwd default-directory)
        (worktree (format "../%s" patch-name))
        (path (expand-file-name patch-name))
        (git-root (git-root source-directory)))
    (when (file-exists-p path)
      (error "A worktree with this name already exists"))
    (cd git-root)
    (git "worktree" "add" worktree)
    (cd worktree)
    (message "Succesfully changed to %s directory." worktree)))

(defun emacs-remove-patch (&optional patch-name)
  "Remove an Emacs patch."
  (interactive "sPatch name: ")
  (message "Patch name %s" patch-name)
  (let ((pwd default-directory)
        (wname (format "../%s" patch-name))
        (wdir (expand-file-name (format "../%s/" patch-name)))
        (path (expand-file-name patch-name))
        (git-root (git-root source-directory)))
    (unless (file-exists-p wname)
      (error "A worktree with this name does not exist"))
    (cd git-root)
    (git "worktree" "remove" "--force" wname)
    (if (equal pwd wdir) (cd "..") (cd pwd))
    (message "%s removed." patch-name)))

(defun emacs-build ()
  (interactive)
  (emacs-download-and-build
   (completing-read "Configuration: " emacs-configs)))

(defun emacs-build-async ()
  (interactive)
  (setenv "CFLAGS" emacs-cflags-opt)
  (let* ((config (completing-read "Configuration: " emacs-configs))
         (command
          (concat "emacs -Q --batch -l "
                  "~/repos/emsrc/build-emacs.el "
                  "--eval '(emacs-download-and-build \"" config "\")'")))
    (async-shell-command
     command
     (generate-new-buffer "*build-errors*")
     (generate-new-buffer "*build-emacs*"))))

(provide 'build-emacs)
;;; build-emacs.el ends here
