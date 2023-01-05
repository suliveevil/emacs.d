;;; init-test.el

;; (defun language-detect-zh ()
;;   (interactive)
;;   (let ((zh-words 0) (en-words 0))
;;     (with-temp-buffer
;;       (insert (format (thing-at-point 'paragraph)))
;;       (goto-char (point-min))
;;       (while (< (point) (point-max))
;;         (let ((ch (char-to-string (char-after))))
;;           (cond
;;            ((string-match "\\cC" ch)
;;             (let ((start-point (point)))
;;               (forward-word)
;;               (setq zh-words (+ zh-words (- (point) start-point)))))
;;            ((string-match "[a-zA-Z]" ch)
;;             (forward-word)
;;             (setq en-words (1+ en-words)))
;;            (t
;;             (forward-char))))))
;;     (if (< en-words zh-words)
;;      (message "中文")
;;       (message "English")
;;         ;; (cons "zh-CN" "en")
;;       ;; (cons "en" "zh-CN")
;;       )
;;     )
;;   )


;; test my little functions

(provide 'init-test)
;;; init-test.el ends here
