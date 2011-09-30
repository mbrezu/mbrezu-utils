;;;; package.lisp

(defpackage #:mbrezu-utils
  (:use #:cl #:cl-match)
  (:nicknames #:mabu)
  (:export mkstr mksymb
           to-list from-list
           defclassf
           ->
           deep-equal diff grep-apropos shallow-copy
           print-all ematch
           aif awhen bif bwhen
           store-big-endian load-big-endian
           binary-diff binary-patch
           binary-patch-encode
           binary-patch-decode))

(defpackage #:mbrezu-utils-postgres
  (:use #:cl #:mabu)
  (:nicknames #:mup)
  (:export with-connection with-transaction exec
           retry-on-serialization-error))

(defpackage #:mbrezu-utils-threads
  (:use #:cl #:mabu #:bordeaux-threads)
  (:nicknames #:mut)
  (:export *log* clear-log log-message get-log log-sleep))

(defpackage #:mbrezu-utils-pretty-print
  (:use #:cl #:mabu #:cl-match)
  (:nicknames #:mupp)
  (:export pretty-print))

(defpackage #:mbrezu-utils-tests
  (:use #:cl #:mabu #:fiveam #:mupp)
  (:export :mbrezu-utils-tests))

