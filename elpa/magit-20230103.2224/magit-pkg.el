(define-package "magit" "20230103.2224" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (compat "28.1.1.2")
    (dash "20210826")
    (git-commit "20221127")
    (magit-section "20221127")
    (transient "20220325")
    (with-editor "20220318"))
  :commit "f47b68929f01a1b8299f3160b0ab02d69d59fcb7" :authors
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
