;; https://github.com/sgpthomas/ob-json
(defun org-babel-execute:json (body params)
  (org-babel-eval "jq -M" body))

;;;###autoload
(define-derived-mode json-mode javascript-mode "JSON Mode"
  "Major Mode for highlighting json org babel blocks.")
(provide 'ob-json)