
(in-package #:mbrezu-utils-html-find-path)

(defstruct path-segment
  (direct-descendant)
  (element)
  (attributes)
  (count))

(defun parse-path-segment (input &optional (direct-descendant nil) (count nil))
  (cond ((atom input) (make-path-segment :direct-descendant direct-descendant
                                         :element input
                                         :attributes nil
                                         :count count))
        ((eq (first input) '!)
         (parse-path-segment (rest input) t count))
        ((numberp (first input))
         (parse-path-segment (rest input) direct-descendant (first input)))
        ((and (atom (first input))
              (null (rest input)))
         (parse-path-segment (first input) direct-descendant count))
        (t (make-path-segment :direct-descendant direct-descendant
                              :element (first input)
                              :attributes (rest input)
                              :count count))))

(defun parse-path (path)
  (cond ((null path) nil)
        (t (cons (parse-path-segment (first path))
                 (parse-path (rest path))))))

(defun chunk (list chunk-size)
  (cond ((null list) nil)
        ((< (length list) chunk-size) (list list))
        (t (cons (subseq list 0 chunk-size)
                 (chunk (subseq list chunk-size) chunk-size)))))

(defun attribute-include (attributes1 attributes2-chunked)
  (every #'(lambda (c)
             (member c attributes2-chunked :test #'equal))
         (chunk attributes1 2)))

(defstruct trail-segment
  (element)
  (count))

(defun walk-parsed-html (parsed-html
                         fn
                         &optional
                           (trail (make-array 10 :adjustable t :fill-pointer 0))
                           (count 1))
  (vector-push-extend (make-trail-segment :element parsed-html
                                          :count count)
                      trail)
  (when (funcall fn trail)
    (when (consp parsed-html)
      (let ((counts (make-hash-table)))
        (mapc #'(lambda (child)
                  (let ((key (if (atom child)
                                 ""
                                 (first child))))
                    (when (null (gethash key counts))
                      (setf (gethash key counts) 0))
                    (incf (gethash key counts))
                    (walk-parsed-html child fn trail (gethash key counts))))
              (cddr parsed-html)))))
  (vector-pop trail)
  (values))

(defun match-path-and-trail (parsed-path trail index)
  (cond ((null parsed-path) t)
        ((< (- (length trail) index)
            (length parsed-path))
         nil)
        (t (let* ((trail-item (aref trail index))
                  (trail-element (trail-segment-element trail-item))
                  (trail-count (trail-segment-count trail-item))
                  (path-item (first parsed-path))
                  (path-count (path-segment-count path-item)))
             (cond ((atom trail-element) nil)
                   ((and (eq (first trail-element)
                             (path-segment-element path-item))
                         (or (null path-count)
                             (= path-count trail-count))
                         (attribute-include (path-segment-attributes path-item)
                                            (cadr trail-element)))
                    (match-path-and-trail (rest parsed-path)
                                          trail
                                          (1+ index)))
                   ((not (path-segment-direct-descendant path-item))
                    (match-path-and-trail parsed-path trail (1+ index)))
                   (t nil))))))

(defun find-path (path parsed-html)
  "
XPath-like function to list nodes in a parsed HTML file by
path. The parsed HTML must be in the form provided by
\(chtml:parse html (chtml:make-lhtml-builder))

\(chtml from system closure-html, html is the raw text of the html
page).

Examples:

\(find-path \'(:html :div) parsed-html) will find the <div>
elements in <html> (not necessarily direct descendants).

\(find-path \'(:html (:div (:author \"me\")) parsed-html) will find
the <div> elements in <body> that have an attribute :author
with value \"me\".

\(find-path \'((:div (:author \"me\")) (! :strong)) parsed-html) will
find <strong> elements that are direct descendants (children, not
grand-children or grand-grand-children etc.) of a <div> element
that has an :author attribute with value \"me\".

\(find-path \'((:div :id \"content\") (! 2 :div )) parsed-html) will
find the <div> elements that are direct descendants of a <div>
element with an :id attribute with value \"content\"; only the <div>
elements that are second in their list of sibilings of the same
type are returned.
"
  (let ((acc nil)
        (parsed-path (parse-path path)))
    (walk-parsed-html parsed-html
                      #'(lambda (trail)
                          (if (and (> (length trail) 0)
                                   (match-path-and-trail parsed-path trail 0))
                              (progn
                                (push (trail-segment-element (aref trail (1- (length trail))))
                                      acc)
                                nil)
                              t)))
    (nreverse acc)))
