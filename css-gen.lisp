
(in-package :mucg)

(defun action-with-separator (elements element-action separator-action)
  (unless elements (return-from action-with-separator))
  (funcall element-action (first elements))
  (dolist (element (rest elements))
    (funcall separator-action)
    (funcall element-action element)))

(defun gen-css (css-declarations &optional (stream *standard-output*))
  (action-with-separator (group css-declarations 2)
                         (lambda (css-declaration)
                           (write-css-decl (first css-declaration)
                                           (second css-declaration)
                                           stream))
                         (lambda ()
                           (terpri stream))))

(defun write-css-decl (selectors rules stream)
  (cond ((or (stringp selectors) (symbolp selectors))
         (write-atom selectors stream))
        ((listp selectors)
         (write-atoms selectors stream))
        (t (error "invalid selectors")))
  (write-string " " stream)
  (write-string "{" stream)
  (terpri stream)
  (action-with-separator rules
                         (lambda (rule)
                           (write-string "    " stream)
                           (write-atom (first rule) stream)
                           (write-string ": " stream)
                           (write-atom (second rule) stream)
                           (write-string ";" stream))
                         (lambda ()
                           (terpri stream)))
  (terpri stream)
  (write-string "}" stream)
  (terpri stream))

(defun write-atom (atom stream)
  (cond ((stringp atom)
         (write-string atom stream))
        ((symbolp atom)
         (write-string (-> atom symbol-name string-downcase) stream))))

(defun write-atoms (atoms stream)
  (action-with-separator atoms
                         (lambda (atom)
                           (write-atom atom stream))
                         (lambda ()
                           (write-string ", " stream))))