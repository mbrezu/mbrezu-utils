;;;; package.lisp

(defpackage #:mbrezu-utils
  (:use #:cl)
  (:use #:cl-match)
  (:nicknames #:mabu)
  (:export mkstr mksymb
           to-list from-list
           defclassf
           ->
           deep-equal diff grep-apropos
           print-all
           list-diff list-patch
           aif awhen bif bwhen
           store-big-endian load-big-endian
           binary-diff binary-patch
           binary-patch-encode
           binary-patch-decode))

(defpackage #:mbrezu-utils-postgres
  (:use #:cl)
  (:use #:mabu)
  (:nicknames #:mup)
  (:export with-connection with-transaction exec
           retry-on-serialization-error))

(defpackage #:mbrezu-utils-threads
  (:use #:cl)
  (:use #:mabu)
  (:use #:bordeaux-threads)
  (:nicknames #:mut)
  (:export *log* clear-log log-message get-log log-sleep))

(defpackage #:mbrezu-utils-tests
  (:use #:cl)
  (:use #:mabu)
  (:use #:fiveam)
  (:export :mbrezu-utils-tests))




