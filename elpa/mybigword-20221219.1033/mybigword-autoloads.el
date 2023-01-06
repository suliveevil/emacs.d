;;; mybigword-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from mybigword.el

(autoload 'mybigword-update-cache "mybigword" "\
Update cache using `mybigword-data-file'.")
(autoload 'mybigword-extract-words "mybigword" "\
Words whose usage frequency is below `mybigword-upper-limit' in TEXT.

(fn TEXT)")
(autoload 'mybigword-show-big-words-from-current-buffer "mybigword" "\
Show big words in current buffer." t)
(autoload 'mybigword-show-big-words-from-file "mybigword" "\
Show bug words from FILE.

(fn FILE)" t)
(autoload 'mybigword-play-video-of-word-at-point "mybigword" "\
Search video's subtitle (*.srt) and play the video of the word.
The video file should be in the same directory of subtitle.
Its file name should be similar to the subtitle's file name.
If video file is missing, the mp3 with similar name is played.
The word is either the word at point, or selected string or string from input." t)
(autoload 'mybigword-pronounce-word-internal "mybigword" "\
Use cambridge dictionary to pronounce WORD.

(fn WORD)")
(autoload 'mybigword-pronounce-word "mybigword" "\
Pronounce word.  If INPUT-P is t, user need input word.

(fn &optional INPUT-P)" t)
(autoload 'mybigword-show-image-of-word "mybigword" "\
Show image of word." t)
(autoload 'mybigword-big-words-in-current-window "mybigword" "\
Show visible big words in current window.
`mybigword-select-visible-word-function' is executed if a big word is selected.
The word is pronounced and its definition is displayed." t)
(autoload 'mybigword-video2mp3 "mybigword" "\
Convert videos in DIRECTORY into mp3.
If QUIET is t, no message output.

(fn DIRECTORY &optional QUIET)" t)
(register-definition-prefixes "mybigword" '("mybigword-"))

;;; End of scraped data

(provide 'mybigword-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; mybigword-autoloads.el ends here