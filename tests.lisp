
(in-package :mbrezu-utils-tests)

(declaim (optimize debug))

(def-suite mbrezu-utils-tests
    :description "Tests for mbrezu-utils.")

(in-suite mbrezu-utils-tests)

(mabu:defclassf point () (x y))

(mabu:defclassf rectangle () (ul-corner dr-corner))

(test basic-to-list
  "Test that converting a class to a list works."
  (is (equalp '(:CLASS POINT :X 1 :Y 2)
              (to-list (mk-point 1 2))))
  (is (equalp '(:CLASS RECTANGLE
                :UL-CORNER (:CLASS POINT :X 0 :Y 1)
                :DR-CORNER (:CLASS POINT :X 2 :Y 3))
              (to-list (mk-rectangle (mk-point 0 1) (mk-point 2 3)))))
  (is (equalp '((:CLASS POINT :X 1 :Y 2) (:CLASS POINT :X 2 :Y 3))
              (to-list (list (mk-point 1 2) (mk-point 2 3))))))

(test basic-from-list
  "Test that converting a class into a list and back into a class
works."
  (let ((tests (list (mk-point 1 2)
                     (list (mk-point 1 2) (mk-point 1 3))
                     (mk-rectangle (mk-point 1 2) (mk-point 1 3)))))
    (dolist (test tests)
      (is (deep-equal test (-> test to-list from-list))))))

(mabu:defclassf person () (name age gender))

(mabu:defclassf meeting () (people name mtime))

(mabu:defclassf schedule () (meetings author))

(test complex-to-list
  "Test that more complex classes are properly converted to lists and back."
  (let ((schedule
         (make-schedule :author (mk-person "John" 24 :male)
                        :meetings (list (make-meeting :people
                                                      (list (mk-person "John" 24 :male)
                                                            (mk-person "Jane" 22 :female)
                                                            (mk-person "Andrew" 30 :male))
                                                      :name "Meeting 1"
                                                      :mtime "Today")
                                        (make-meeting :people
                                                      (list (mk-person "John" 24 :male)
                                                            (mk-person "Anne" 28 :female)
                                                            (mk-person "Simon" 20 :male))
                                                      :name "Meeting 2"
                                                      :mtime "Tomorrow")))))
    (is (deep-equal schedule (from-list (to-list schedule))))))

(test corner-cases
  "Test that escaping for :class works."
  (let ((tests (list '(:class 1)
                     '(:class)
                     '((:atom :class))
                     '((:atom . :class)))))
    (dolist (test tests)
      (is (deep-equal test (-> test to-list from-list))))))

(test diff
  "Test that diffing fields works."
  (is (equalp '((:CHANGE DR-CORNER ((:CHANGE Y (:OLD 3 :NEW 4)))))
              (diff (mk-rectangle (mk-point 1 2) (mk-point 2 3))
                    (mk-rectangle (mk-point 1 2) (mk-point 2 4))))))

(defun mk-random-bytes (size)
  (let ((result (make-array size :element-type '(mod 256))))
    (dotimes (i size)
      (setf (aref result i) (random 256)))
    result))

(defun mutate-bytes (bytes times)
  (dotimes (i times)
    (ecase (random 3)
      ((0) (setf (aref bytes (random (length bytes))) (random 256)))
      ((1) (when (> (length bytes) 0)
             (let ((point (random (length bytes))))
               (setf bytes (concatenate 'simple-vector
                                        (subseq bytes 0 point)
                                        (subseq bytes (1+ point)))))))
      ((2) (let ((point (random (length bytes))))
             (setf bytes (concatenate 'simple-vector
                                      (subseq bytes 0 point)
                                      (make-array 1 :initial-element (random 256))
                                      (subseq bytes point)))))))
  bytes)

(defvar *last-bd-original*)
(defvar *last-bd-modified*)

(defun test-binary-diff-patch (size mutations)
  (let (original-array modified-array)
    (setf original-array (mk-random-bytes size))
    (setf modified-array (mutate-bytes (copy-seq original-array) mutations))
    (let ((must-hold (equalp modified-array
                             (binary-patch original-array (binary-diff original-array
                                                                       modified-array)))))
      (unless must-hold
        (setf *last-bd-original* original-array)
        (setf *last-bd-modified* modified-array))
      must-hold)))

(test binary-diff
  (let ((original #(10 20 30 40 50 50 60 70 80 90 100))
        (modified #(10 20 30 40 50 60 70 80 90 100)))
    (is (equalp modified
                (binary-patch original (binary-diff original modified)))))
  (let ((original #(10 20 30 40 50 60 70 80 90 100 110))
        (modified #(10 20 30 40 50 60 70 80 90 100 110)))
    (is (null (binary-diff original modified)))
    (is (equalp modified
                (binary-patch original (binary-diff original modified)))))
  (dotimes (i 500)
    (is (identity (test-binary-diff-patch 101 5))))
  (dotimes (i 200)
    (is (identity (test-binary-diff-patch 1001 20))))
  (dotimes (i 100)
    (is (identity (test-binary-diff-patch 10001 50)))))

(defun test-binary-diff-encoding (patches)
  (equalp patches (-> patches
                      binary-patch-encode
                      binary-patch-decode)))

(test binary-diff-encoding
  (is (identity (test-binary-diff-encoding '())))
  (is (identity (test-binary-diff-encoding '((:copy 0 100)))))
  (is (identity (test-binary-diff-encoding '((:add #(0 100 200 50))))))
  (is (identity (test-binary-diff-encoding
                 `((:copy 70000 80000) (:add ,(make-array 100 :element-type '(mod 256)))))))
  (is (identity (test-binary-diff-encoding
                 `((:copy ,(expt 2 33) 80000) (:add ,(make-array 500 :element-type '(mod 256)))))))
  (is (identity (test-binary-diff-encoding
                 `((:copy ,(expt 2 33) 80000)
                   (:add ,(make-array 500 :element-type '(mod 256)))
                   (:add ,(make-array 70000 :element-type '(mod 256))))))))

(test pretty-printing
  (is (string-equal "  ahabahatoo
         boo
         hoo
         gooo
         
" (pretty-print '(:never-break "  "
                                    (:maybe-break "aha" "baha")
                                    (:maybe-break "too" "boo" "hoo" "gooo"))
                                  10)))
  (is (string-equal "  aha
  baha
  too
  boo
  hoo
  gooo
  
" (pretty-print '(:never-break "  "
                                    (:maybe-break "aha" "baha")
                                    (:maybe-break "too" "boo" "hoo" "gooo"))
                                  5)))
  (is (string-equal "  ahabahatooboohoogooo
" (pretty-print '(:never-break "  "
                                    (:maybe-break "aha" "baha")
                                    (:maybe-break "too" "boo" "hoo" "gooo"))
                                  50))))

(test html-generation
  (is (string-equal "<html><head><title>Test</title></head><body/></html>"
                    (with-output-to-string (str)
                      (muhg:gen-html '(:html
                                       (:head (:title "Test"))
                                       (:body))
                                     str))))
  (is (string-equal "<html><head><title>Test</title></head><body>100%<p id=\"p1\">hello, world</p></body></html>"
                    (with-output-to-string (str)
                      (muhg:gen-html '(:html
                                       (:head (:title "Test"))
                                       (:body "100%"
                                        (:p :id "p1" "hello, world")))
                                     str))))
  (is (string-equal "<html><head><title>Test</title></head><body>100%<p id=\"p1\" class=\"para\">hello, world</p></body></html>"
                    (with-output-to-string (str)
                      (muhg:gen-html '(:html
                                       (:head (:title "Test"))
                                       (:body "100%"
                                        (:p :id "p1" :class "para"
                                         "hello, world")))
                                     str)))))

(test css-generation
  (is (string-equal ".footer:visited {
    list-style: none;
    display: block;
}

h1, h2 {
    background-color: yellow;
}
" (with-output-to-string (str)
    (mucg:gen-css '(".footer:visited" ((list-style none)
                                       (display block))
                    (h1 h2) ((background-color yellow)))
                  str))))

  (is (string-equal "a {
    list-style: none;
    display: block;
}
" (with-output-to-string (str)
    (mucg:gen-css '(a ((list-style none)
                       (display block)))
                  str)))))
