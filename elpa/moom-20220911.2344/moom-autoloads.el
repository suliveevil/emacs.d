;;; moom-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from moom.el

(autoload 'moom-undo "moom" "\
Undo.
If INDEX is non-nil, revert to the provided id of history.

(fn &optional INDEX)" t)
(autoload 'moom-identify-current-monitor "moom" "\
Update `moom--screen-margin' to identify and focus on the current monitor.
SHIFT can control the margin, if needed.
If SHIFT is nil, `moom--common-margin' will be applied.

(fn &optional SHIFT)" t)
(autoload 'moom-print-monitors "moom" "\
Print available monitors with index number.
`moom-jump-to-monitor' could be useful to jump to a monitor." t)
(autoload 'moom-jump-to-monitor "moom" "\
Jump to a monitor by specifying ID.

(fn ID)" t)
(autoload 'moom-cycle-monitors "moom" "\
Cycle monitors.
`moom-after-select-monitor-hook' could be useful to add some additional
actions when selecting a monitor." t)
(autoload 'moom-fill-screen "moom" "\
Expand frame width and height to fill screen.
The font size in buffers will be increased so that the frame width could be
maintained at 80. The top left corner of the frame is moved to that of screen.
`moom-before-fill-screen-hook' and `moom-after-fill-screen-hook' can be
used to add additional actions." t)
(autoload 'moom-toggle-frame-maximized "moom" "\
Toggle frame maximized.
No information is stored for undo." t)
(autoload 'moom-fill-top "moom" "\
Fill upper half of screen." t)
(autoload 'moom-fill-bottom "moom" "\
Fill lower half of screen." t)
(autoload 'moom-fill-left "moom" "\
Fill left half of screen." t)
(autoload 'moom-fill-right "moom" "\
Fill right half of screen." t)
(autoload 'moom-fill-top-left "moom" "\
Fill top left quarter of screen." t)
(autoload 'moom-fill-top-right "moom" "\
Fill top right quarter of screen." t)
(autoload 'moom-fill-bottom-left "moom" "\
Fill bottom left quarter of screen." t)
(autoload 'moom-fill-bottom-right "moom" "\
Fill bottom right quarter of screen." t)
(autoload 'moom-fill-band "moom" "\
Fill screen by band region.
If PLIST is nil, `moom-fill-band-options' is applied.

(fn &optional PLIST)" t)
(autoload 'moom-cycle-line-spacing "moom" "\
Change `line-spacing’ value between a range." t)
(autoload 'moom-reset-line-spacing "moom" "\
Reset to the defaut value for line spacing." t)
(autoload 'moom-move-frame-right "moom" "\
PIXEL move the current frame to right.

(fn &optional PIXEL)" t)
(autoload 'moom-move-frame-left "moom" "\
PIXEL move the current frame to left.

(fn &optional PIXEL)" t)
(autoload 'moom-move-frame-to-horizontal-center "moom" "\
Move the current frame to the horizontal center of the screen." t)
(autoload 'moom-move-frame-to-vertical-center "moom" "\
Move the current frame to the vertical center of the screen." t)
(autoload 'moom-move-frame-to-edge-top "moom" "\
Move the current frame to the top of the screen.
If you find the frame is NOT moved to the top exactly,
please configure the margins by variable `moom-user-margin'." t)
(autoload 'moom-move-frame-to-edge-bottom "moom" "\
Move the current frame to the top of the screen.
If you find the frame is NOT moved to the bottom exactly,
please configure the margins by variable `moom-user-margin'." t)
(autoload 'moom-move-frame-to-edge-right "moom" "\
Move the current frame to the right edge of the screen." t)
(autoload 'moom-move-frame-to-edge-left "moom" "\
Move the current frame to the left edge of the screen." t)
(autoload 'moom-move-frame-to-centerline-from-left "moom" "\
Fit frame to vertical line in the middle from left side." t)
(autoload 'moom-move-frame-to-centerline-from-right "moom" "\
Fit frame to vertical line in the middle from right side." t)
(autoload 'moom-move-frame-to-centerline-from-top "moom" "\
Fit frame to horizontal line in the middle from above." t)
(autoload 'moom-move-frame-to-centerline-from-bottom "moom" "\
Fit frame to horizontal line in the middle from below." t)
(autoload 'moom-move-frame-to-center "moom" "\
Move the current frame to the center of the screen." t)
(autoload 'moom-move-frame "moom" "\
Move the frame to somewhere (default: left top of workarea).
When ARG is a list like '(10 10), move the frame to the position.
When ARG is a single number like 10, shift the frame horizontally +10 pixel.
When ARG is nil, then move to the default position (i.e. left top of workarea).

(fn &optional ARG)" t)
(autoload 'moom-cycle-frame-height "moom" "\
Change frame height and update the internal ring.
If you find the frame is NOT changed as expected,
please configure the margins by variable `moom-user-margin'.
No information is stored for undo." t)
(autoload 'moom-fill-height "moom" "\
Expand frame height to fill screen vertically without changing frame width." t)
(autoload 'moom-change-frame-height "moom" "\
Change the hight of the current frame.
Argument FRAME-HEIGHT specifies new frame height.
If PIXELWISE is non-nil, the frame height will be changed by pixel value.

(fn &optional FRAME-HEIGHT PIXELWISE)" t)
(autoload 'moom-change-frame-width "moom" "\
Change the frame width by the FRAME-WIDTH argument.
This function does not effect font size.
If FRAME-WIDTH is nil, `moom-frame-width-single' will be used.
If PIXELWISE is non-nil, the frame width will be changed by pixel value.
In that case, variable `moom--frame-width' will keep the same value.

(fn &optional FRAME-WIDTH PIXELWISE)" t)
(autoload 'moom-change-frame-width-single "moom" "\
Change the frame width to single.
This function does not effect font size." t)
(autoload 'moom-change-frame-width-double "moom" "\
Change the frame width to double.
This function does not effect font size." t)
(autoload 'moom-change-frame-width-half-again "moom" "\
Change the frame width to half as large again as single width.
This function does not effect font size." t)
(autoload 'moom-fill-width "moom" "\
Change the frame width to fill display horizontally.
This function does not effect font size." t)
(autoload 'moom-delete-windows "moom" "\
Delete all window and make frame width single.
No information is stored for undo." t)
(autoload 'moom-split-window "moom" "\
Split window and make frame width double.
No information is stored for undo." t)
(autoload 'moom-reset "moom" "\
Reset associated parameters." t)
(autoload 'moom-update-height-steps "moom" "\
Change number of step of the height ring by ARG.
The default step is 4.

(fn ARG)")
(autoload 'moom-screen-margin "moom" "\
Change top, bottom, left, and right margin by provided MARGINS.
MARGINS shall be a list consists of 4 integer variables like '(23 0 0 0).
If FILL is non-nil, the frame will cover the screen with given margins.

(fn MARGINS &optional FILL)")
(autoload 'moom-update-user-margin "moom" "\
Update variable `moom-user-margin' and apply it to internal margin.
MARGIN is a list with 4 integers in order of {top, down, left, right}.

(fn MARGIN)" t)
(autoload 'moom-check-user-margin "moom" "\
Change top, bottom, left, and right margin by provided MARGINS.
MARGIN shall be a list consists of 4 integer variables like '(10 10 20 20).

(fn MARGIN)" t)
(autoload 'moom-restore-last-status "moom" "\
Restore the last frame position, size, and font-size.
STATUS is a list consists of font size, frame position, frame region,
and pixel-region.

(fn &optional STATUS)" t)
(autoload 'moom-toggle-font-module "moom" "\
Toggle `moom--font-module-p'.
When `moom--font-module-p' is nil, font size is fixed except for `moom-reset'
even if \"moom-font.el\" is loaded." t)
(autoload 'moom-generate-font-table "moom" "\
Generate a font table.
The last frame position and size will be restored." t)
(autoload 'moom-recommended-keybindings "moom" "\
Apply pre defined keybindings.
OPTIONS is a list of moom API types.  If you want to set all recommemded
keybindings, put the following code in your init.el.
 (with-eval-after-load \"moom\"
   (moom-recommended-keybindings ='all))
='all is identical to ='(move fit expand fill font reset undo).
If OPTIONS includes ='wof, then each binding is configured not to use fn key.
If you give only ='(reset) as the argument, then \\[moom-reset] is activated.
The keybindings will be assigned only when Emacs runs in GUI.

(fn OPTIONS)")
(autoload 'moom-print-status "moom" "\
Print font size, frame size and origin in mini buffer." t)
(autoload 'moom-version "moom" "\
The release version of Moom." t)
(defvar moom-mode nil "\
Non-nil if Moom mode is enabled.
See the `moom-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `moom-mode'.")
(custom-autoload 'moom-mode "moom" nil)
(autoload 'moom-mode "moom" "\
Toggle the minor mode `moom-mode'.

This mode provides a set of commands to control frame position and size.
The font size in buffers will be changed with synchronization of the updated
frame geometry so that the frame width could be maintained at 80.

No keybindings are configured as default but recommended keybindings are
implemented in `moom-mode', thus user setting is very easy.
You just use `moom-recommended-keybindings' to apply the recommended
keybindings.

To see more details and examples, please visit https://github.com/takaxp/moom.

This is a global minor mode.  If called interactively, toggle the
`Moom mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='moom-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "moom" '("moom-"))


;;; Generated autoloads from moom-font.el

(autoload 'moom-font-ascii "moom-font" "\
Set ASCII font family by given FONT.
If PLIST is non-nil and it has immediate property,
given FONT is immediately applied.

(fn FONT &optional PLIST)")
(autoload 'moom-font-ja "moom-font" "\
Set Japanese font family by given FONT.
If PLIST is non-nil and it has immediate property,
given FONT is immediately applied.

(fn FONT &optional PLIST)")
(autoload 'moom-font-resize "moom-font" "\
Resize font.
`frame-width' will be updated accordingly.
Optional argument N specifies the target font size.
If WIDTH is non-nil, ensure an appropriate font size so that
the actual pixel width will not exceed the WIDTH.

(fn &optional N WIDTH)" t)
(autoload 'moom-font-size-reset "moom-font" "\
Reset font to the initial size." t)
(autoload 'moom-font-increase "moom-font" "\
Increase font size.
Optional argument INC specifies an increasing step.

(fn &optional INC)" t)
(autoload 'moom-font-decrease "moom-font" "\
Decrease font size.
Optional argument DEC specifies a decreasing step.

(fn &optional DEC)" t)
(autoload 'moom-font-print-name-at-point "moom-font" "\
Print font family name at point." t)
(register-definition-prefixes "moom-font" '("moom-font-"))


;;; Generated autoloads from moom-transient.el

(autoload 'moom-transient-dispatch "moom-transient")
(put 'moom-transient-dispatch 'interactive-only t)
(put 'moom-transient-dispatch 'function-documentation "\
Command list of `moom'.")
(put 'moom-transient-dispatch 'transient--prefix (transient-prefix :command 'moom-transient-dispatch :transient-suffix 'moom-transient--do-stay))
(put 'moom-transient-dispatch 'transient--layout (list (vector 1 'transient-columns (list :description 'moom-transient--dispatch-description) (list (vector 1 'transient-column (list :description "Move") (list (list 1 'transient-suffix (list :key "0" :description "top-left" :command 'moom-move-frame)) (list 1 'transient-suffix (list :key "1" :description "left" :command 'moom-move-frame-left)) (list 1 'transient-suffix (list :key "2" :description "center" :command 'moom-move-frame-to-center)) (list 1 'transient-suffix (list :key "3" :description "right" :command 'moom-move-frame-right)) (list 1 'transient-suffix (list :key "4" :description "center (hol)" :command 'moom-move-frame-to-horizontal-center)) (list 1 'transient-suffix (list :key "5" :description "center (ver)" :command 'moom-move-frame-to-vertical-center)))) (vector 1 'transient-column (list :description "Expand") (list (list 1 'transient-suffix (list :key "s" :description "single" :command 'moom-change-frame-width-single)) (list 1 'transient-suffix (list :key "d" :description "double" :command 'moom-change-frame-width-double)) (list 1 'transient-suffix (list :key "a" :description "3/2" :command 'moom-change-frame-width-half-again)) (list 1 'transient-suffix (list :key "w" :description "width (full)" :command 'moom-fill-width)) (list 1 'transient-suffix (list :key "h" :description "height (full)" :command 'moom-fill-height)) (list 1 'transient-suffix (list :key "H" :description "height (cycle)" :command 'moom-cycle-frame-height)))) (vector 1 'transient-column (list :description "Fit (edge)") (list (list 1 'transient-suffix (list :key "e l" :description "edge left" :command 'moom-move-frame-to-edge-left)) (list 1 'transient-suffix (list :key "e r" :description "edge right" :command 'moom-move-frame-to-edge-right)) (list 1 'transient-suffix (list :key "e t" :description "edge top" :command 'moom-move-frame-to-edge-top)) (list 1 'transient-suffix (list :key "e b" :description "edge bottom" :command 'moom-move-frame-to-edge-bottom)))) (vector 1 'transient-column (list :description "Fit (center)") (list (list 1 'transient-suffix (list :key "c l" :description "center left" :command 'moom-move-frame-to-centerline-from-left)) (list 1 'transient-suffix (list :key "c r" :description "center right" :command 'moom-move-frame-to-centerline-from-right)) (list 1 'transient-suffix (list :key "c t" :description "center top" :command 'moom-move-frame-to-centerline-from-top)) (list 1 'transient-suffix (list :key "c b" :description "center bottom" :command 'moom-move-frame-to-centerline-from-bottom)))))) (vector 1 'transient-columns (list :description 'moom-transient--font-module-status-description) (list (vector 1 'transient-column nil (list (list 1 'transient-suffix (list :key "f 1" :description "top-left" :command 'moom-fill-top-left)) (list 1 'transient-suffix (list :key "f 2" :description "top-right" :command 'moom-fill-top-right)) (list 1 'transient-suffix (list :key "f 3" :description "bottom-left" :command 'moom-fill-bottom-left)) (list 1 'transient-suffix (list :key "f 4" :description "bottom-right" :command 'moom-fill-bottom-right)))) (vector 1 'transient-column nil (list (list 1 'transient-suffix (list :key "f l" :description "left" :command 'moom-fill-left)) (list 1 'transient-suffix (list :key "f r" :description "right" :command 'moom-fill-right)) (list 1 'transient-suffix (list :key "f t" :description "top" :command 'moom-fill-top)) (list 1 'transient-suffix (list :key "f b" :description "bottom" :command 'moom-fill-bottom)))) (vector 1 'transient-column nil (list (list 1 'transient-suffix (list :key "f s" :description "screen" :command 'moom-fill-screen)) (list 1 'transient-suffix (list :key "f m" :description "band" :command 'moom-fill-band)) "" (list 1 'transient-suffix (list :key "F" :description "toggle font resizing" :command 'moom-transient-toggle-font-module)))))) (vector 1 'transient-columns nil (list (vector 1 'transient-column (list :description "Monitors") (list (list 1 'transient-suffix (list :key "m c" :description "monitor cycle" :command 'moom-cycle-monitors)) (list 1 'transient-suffix (list :key "m p" :description "monitor print" :command 'moom-print-monitors)))) (vector 1 'transient-column (list :description "Window") (list (list 1 'transient-suffix (list :key "S" :description "split" :command 'moom-split-window)) (list 1 'transient-suffix (list :key "D" :description "delete" :command 'moom-delete-windows)))) (vector 1 'transient-column (list :description 'moom-transient--font-api-status-description :if (lambda nil moom--font-module-p)) (list (list 1 'transient-suffix (list :key "=" :description "increase" :command 'moom-font-increase)) (list 1 'transient-suffix (list :key "-" :description "decrease" :command 'moom-font-decrease)) (list 1 'transient-suffix (list :key "R" :description "reset" :command 'moom-font-size-reset)))) (vector 1 'transient-column (list :description "Utilities") (list (list 1 'transient-suffix (list :key "r" :description "reset" :command 'moom-reset)) (list 1 'transient-suffix (list :key "u" :description "undo" :command 'moom-undo)) (list 1 'transient-suffix (list :key "p" :description "print" :command 'moom-print-status)))) (vector 1 'transient-column (list :description "") (list (list 1 'transient-suffix (list :key "C" :description "config" :command 'moom-transient-config)) (list 1 'transient-suffix (list :key "v" :description "version" :command 'moom-transient-version)) (list 1 'transient-suffix (list :key "q" :description "quit" :command 'transient-quit-all))))))))
(autoload 'moom-transient-config "moom-transient")
(put 'moom-transient-config 'interactive-only t)
(put 'moom-transient-config 'function-documentation "\
Command list of `moom' configuration.")
(put 'moom-transient-config 'transient--prefix (transient-prefix :command 'moom-transient-config))
(put 'moom-transient-config 'transient--layout (list (vector 1 'transient-columns (list :description "[moom] Configuration") (list (vector 1 'transient-column nil (list (list 1 'transient-suffix (list :key "t" :description "generate font table" :command 'moom-generate-font-table)) (list 1 'transient-suffix (list :key "c" :description "check margin" :command 'moom-check-user-margin)) (list 1 'transient-suffix (list :key "p" :description "print font name" :command 'moom-font-print-name-at-point)) (list 1 'transient-suffix (list :key "D" :description "dispatch" :command 'moom-transient-dispatch)) (list 1 'transient-suffix (list :key "q" :description "quit" :command 'transient-quit-all))))))))
(autoload 'moom-transient-toggle-font-module "moom-transient" "\
Call `moom-toggle-font-module' with `moom-transient-dispatch'." t)
(autoload 'moom-transient-hide-cursor "moom-transient" "\
Hide cursor in transient buffer.")
(autoload 'moom-transient-version "moom-transient" "\
Printing version of `moom' and `moom-transient'." t)
(register-definition-prefixes "moom-transient" '("moom-transient-"))

;;; End of scraped data

(provide 'moom-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; moom-autoloads.el ends here
