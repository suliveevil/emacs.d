(define-package "magit" "20230105.1859" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (compat "29.1.0.1")
    (dash "20210826")
    (git-commit "20221127")
    (magit-section "20221127")
    (transient "20220325")
    (with-editor "20220318"))
  :commit "b281f05d22f0c9f0deec5e1714b3b554918853c3" :authors
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
