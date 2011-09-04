;;;; mbrezu-utils.asd

(asdf:defsystem #:mbrezu-utils
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "mbrezu-utils")
               (:file "tests")))

