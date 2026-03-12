;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20260224.1637"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (compat        "30.1")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "20934cfb5a2e7ae037ec10bbc81ca97478738178"
  :revdesc "20934cfb5a2e"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
