(define-package "magit" "20230120.1616" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (compat "29.1.1.0")
    (dash "20210826")
    (git-commit "20221127")
    (magit-section "20221127")
    (transient "20220325")
    (with-editor "20220318"))
  :commit "cd6fbe28873c1ec973bfe51af0f3ade5d69e9ba6" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "jonas@bernoul.li"))
  :maintainer
  '("Jonas Bernoulli" . "jonas@bernoul.li")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
