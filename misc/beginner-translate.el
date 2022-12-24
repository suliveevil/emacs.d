;;; beginner-translate.el ---- A tool for read the news -*-
;;; Commentary:

;; I don't know what to say,Oh I use the BaidU API to translate
;; Why?Because it's free,that's so good.

;;; Code:

;;; C-SPACE select the content and the C-x t to enjoy

;; Require the json package
(require 'json)

;; Define the source language
(defvar beginner-source-language "en")

;; Define the target language
(defvar beginner-target-language "zh")

;; Set the app id
(defvar beginner-appid "20220129001070543")

;; Set the app key
(defvar beginner-appkey "Xf6UKQix5zUnRgZgXPXI")

;; Set the salt
(defvar beginner-salt  (number-to-string (random 10000)))

;; Generate the sign
(defun generate-sign (content) (md5 (concat beginner-appid content beginner-salt beginner-appkey)))

;; Generate the url
(defun generate-url (content) (concat "https://api.fanyi.baidu.com/api/trans/vip/translate?" "q=" content "&" "from=" beginner-source-language "&" "to=" beginner-target-language "&" "appid=" beginner-appid "&" "salt=" beginner-salt "&" "sign=" (generate-sign content)))

;; Call the service to translate
(defun call-translate-engine (url) (render-engine-result (url-retrieve-synchronously url)))

;; Thanks to Emacs China and the BlogSpot
;; Here's code is source from https://emacs-china.org/t/curl-unnnn/6935/3
;; Begin:

(defun unicode-char (code) (decode-char 'ucs code))

(defun unicode-unescape-string (str)
  (with-temp-buffer
    (insert str)
    (unicode-unescape-region (point-min) (point-max))
    (buffer-string)
    )
  )

(defun unicode-unescape-region (start end)
  "指定した範囲のUnicodeエスケープ文字(\\uXXXX)をデコードする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\u\\([[:xdigit:]]\\{4\\}\\)" nil t)
      (replace-match (string (unicode-char
                              (string-to-number (match-string 1) 16)))
                     nil t))))

;; End;

;; Render the result of the engine
(defun render-engine-result (current-buffer)
  (set-buffer current-buffer)
  (setq translate-result (substring (buffer-string) (+ 6 (string-match "dst\":\"" (buffer-string))) (string-match "\"}]}" (buffer-string))))
  (kill-buffer current-buffer)
  (unicode-unescape-string translate-result)
  )

;; Render the result of the engine
(defun render-engine-result (current-buffer)
  (set-buffer current-buffer)
  (setq translate-result (substring (buffer-string) (+ 6 (string-match "dst\":\"" (buffer-string))) (string-match "\"}]}" (buffer-string))))
  (kill-buffer current-buffer)
  (unicode-unescape-string translate-result)
  )



;; Collect the translate of the content
(defun beginner-translate-content (content)
  (call-translate-engine (generate-url content)))

(defun beginner-translate-main (content_start content_end)
  "The main part of the program."
  (interactive "r")
  (message (beginner-translate-content (buffer-substring content_start content_end)))
  )

(global-set-key (kbd "C-x t") 'beginner-translate-main)

(provide 'beginner-translate)
;;; beginner-translate.el ends here
