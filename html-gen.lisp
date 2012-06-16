
(in-package #:mbrezu-utils-html-gen)

(defun gen-html (html-tree &optional (stream *standard-output*))
  (cond ((stringp html-tree)
         (write-string (escape-html html-tree) stream))
        ((and (listp html-tree)
              (keywordp (first html-tree)))
         (make-tag html-tree stream))
        (t (error "invalid html tree")))
  (values))

(defun gen-all-html (html-trees stream)
  (mapc (lambda (html-tree)
          (gen-html html-tree stream))
        html-trees))

(defun tag-name (tag)
  (-> tag symbol-name string-downcase))

(defun make-tag (html-tree stream)
  (let ((tag (first html-tree))
        attributes
        (children (rest html-tree)))
    (loop
       while (-> children first keywordp)
       do (push (list (first children) (second children))
                attributes)
         (setf children (cddr children)))
    (cond (children
           (write-string "<" stream)
           (write-string (tag-name tag) stream)
           (write-attributes (nreverse attributes) stream)
           (write-string ">" stream)
           (gen-all-html children stream)
           (write-string "</" stream)
           (write-string (tag-name tag) stream)
           (write-string ">" stream))
          (t
           (write-string "<" stream)
           (write-string (tag-name tag) stream)
           (write-attributes (nreverse attributes) stream)
           (write-string "/>" stream)))))

(defun write-attributes (attributes stream)
  (dolist (attr attributes)
    (write-string " " stream)
    (write-string (tag-name (first attr)) stream)
    (write-string "=\"" stream)
    (write-string (escape-html (second attr)) stream)
    (write-string "\"" stream)))

(defun escape-html (string)
  (with-output-to-string (str)
    (loop
       for ch being the elements of string
       do (cond ((char= #\< ch)
                 (write-string "&lt;" str))
                ((char= #\> ch)
                 (write-string "&gt;" str))
                ((char= #\& ch)
                 (write-string "&amp;" str))
                ((char= #\" ch)
                 (write-string "&quot;" str))
                ((char= #\' ch)
                 (write-string "&#039;" str))
                (t (write-char ch str))))))
