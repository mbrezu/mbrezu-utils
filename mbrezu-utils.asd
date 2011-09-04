;;;; mbrezu-utils.asd

(asdf:defsystem #:mbrezu-utils
  :serial t
  :depends-on (:fiveam :split-sequence)
  :components ((:file "package")
               (:file "mbrezu-utils")
               (:file "tests")))

