
(in-package :mupp)

(declaim (optimize debug))

(defclassf pp-builder ()
  (lines current-line max-length current-length max-column))

(defun empty-builder (&optional (max-column 80))
  (make-instance 'pp-builder
                 :lines ()
                 :current-line ()
                 :max-length 0
                 :current-length 0
                 :max-column max-column))

(defun new-line (builder indent)
  (let ((last-line (list (make-string indent :initial-element #\space))))
    (make-instance 'pp-builder
                   :lines (cons (apply #'concatenate 'string (reverse (current-line builder)))
                                (lines builder))
                   :current-line last-line
                   :current-length indent
                   :max-length (max-length builder)
                   :max-column (max-column builder))))

(defun add-text (builder text)
  (let ((new-length (+ (length text) (current-length builder))))
    (make-instance 'pp-builder
                   :lines (lines builder)
                   :current-line (cons text (current-line builder))
                   :max-length (max (max-length builder) new-length)
                   :current-length new-length
                   :max-column (max-column builder))))

(defun with-max-column (builder new-max-column)
  (let ((new-builder (shallow-copy builder)))
    (setf (max-column new-builder) new-max-column)
    new-builder))

(defun render (builder)
  (let ((complete-builder (new-line builder 0)))
    (with-output-to-string (str)
      (dolist (line (reverse (lines complete-builder)))
        (format str "~a~%" line)))))

(defun do-never-break (builder children)
  (dolist (child children)
    (setf builder (pretty-print-impl child builder)))
  builder)

(defun do-always-break (builder children)
  (let ((indent (current-length builder)))
    (dolist (child children)
      (setf builder (-> builder
                        (pretty-print-impl child $)
                        (new-line $ indent)))))
  builder)

(defun pretty-print-impl (document builder)
  (ematch document
    ((cons :never-break children)
     (do-never-break builder children))
    ((cons :always-break children)
     (do-always-break builder children))
    ((cons :maybe-break children)
     (let* ((try-one-line (do-never-break (with-max-column builder nil) children)))
       (if (or (-> builder max-column null)
               (< (max-length try-one-line) (max-column builder)))
           (with-max-column try-one-line (max-column builder))
           (do-always-break builder children))))
    ((type string)
     (add-text builder document))))

(defun pretty-print (document &optional (max-column 80))
  (-> document
      (pretty-print-impl $ (empty-builder max-column))
      render))
