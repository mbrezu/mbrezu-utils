;;;; package.lisp

(defpackage #:mbrezu-utils
  (:use #:cl)
  (:nicknames #:mabu)
  (:export mkstr mksymb to-list from-list defclassf -> deep-equal diff grep-apropos))

(defpackage #:mbrezu-utils-tests
  (:use #:cl)
  (:use #:mabu)
  (:use #:fiveam))

