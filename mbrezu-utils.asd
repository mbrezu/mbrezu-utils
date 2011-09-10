;;;; mbrezu-utils.asd

(asdf:defsystem #:mbrezu-utils
  :serial t
  :depends-on (:fiveam :split-sequence :cl-postgres)
  :components ((:file "package")
               (:file "mbrezu-utils")
               (:file "mbrezu-utils-postgres")
               (:file "mbrezu-utils-threads")
               (:file "tests")))

