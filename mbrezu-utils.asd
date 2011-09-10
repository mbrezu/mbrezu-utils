;;;; mbrezu-utils.asd

(asdf:defsystem #:mbrezu-utils
  :serial t
  :depends-on (:fiveam :split-sequence :cl-postgres :bordeaux-threads)
  :components ((:file "package")
               (:file "mbrezu-utils")
               (:file "list-diff")
               (:file "mbrezu-utils-postgres")
               (:file "mbrezu-utils-threads")
               (:file "tests")))

