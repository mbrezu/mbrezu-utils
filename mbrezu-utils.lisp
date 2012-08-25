;;;; mbrezu-utils.lisp

(in-package #:mbrezu-utils)

(declaim (optimize debug))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (format nil "~{~a~}" args))

  (defun mksymb (&rest args)
    (intern (string-upcase (apply #'mkstr args))))

  (defun group (list n)
    (if (< (length list) n)
        (if list
            (list list))
        (cons (subseq list 0 n)
              (group (subseq list n) n)))))

(defmacro -> (obj &rest forms)
  "Similar to the -> macro from clojure, but with a tweak: if there is
  a $ symbol somewhere in the form, the object is not added as the
  first argument to the form, but instead replaces the $ symbol."
  (if forms
      (if (consp (car forms))
          (let* ((first-form (first forms))
                 (other-forms (rest forms))
                 (pos (position '$ first-form)))
            (if pos
                `(-> ,(append (subseq first-form 0 pos)
                              (list obj)
                              (subseq first-form (1+ pos)))
                     ,@other-forms)
                `(-> ,(list* (first first-form) obj (rest first-form))
                     ,@other-forms)))
          `(-> ,(list (car forms) obj)
               ,@(cdr forms)))
      obj))

(defgeneric to-list (instance))

(defgeneric deep-equal (instance1 instance2))

(defgeneric shallow-copy (instance))

(defun from-list (list &optional (is-first t))
  (cond ((atom list) list)
        ((consp list)
         (cond ((and is-first (-> list first to-unescape))
                (cons (-> list first unescape)
                      (-> list rest (from-list $ nil))))
               ((eq :class (car list))
                (apply #'make-instance
                       (second list)
                       (mapcar (lambda (elm)
                                 (cond ((keywordp elm) elm)
                                       (t (from-list elm))))
                               (nthcdr 2 list))))
               (t (cons (-> list first from-list)
                        (-> list rest (from-list $ nil))))))))

(defun to-unescape (data)
  (and (consp data) (-> data first (eq :atom)) (to-escape data)))

(defun unescape (data)
  (-> data rest first))

(defun to-escape (data)
  (cond ((eq :class data) t)
        ((and (consp data) (eq :atom (car data)) (consp (cdr data)))
         (-> data second to-escape))
        (t nil)))

(defun escape (data)
  (list :atom data))

(defmethod to-list ((instance t))
  (cond ((atom instance)
         instance)
        (t (error "Not an atom."))))

(defun to-list-list (list &optional (is-first t))
  (if (atom list)
      list
      (cons (if (and is-first (-> list first to-escape))
                (-> list first escape)
                (-> list first to-list))
            (to-list-list (rest list) nil))))

(defmethod to-list ((instance cons))
  (to-list-list instance))

(defmethod deep-equal ((instance1 t) (instance2 t))
  (equalp instance1 instance2))

(defmethod deep-equal ((instance1 cons) (instance2 cons))
  (and (deep-equal (car instance1) (car instance2))
       (deep-equal (cdr instance1) (cdr instance2))))

(defmethod deep-equal ((instance1 sequence) (instance2 sequence))
  (every #'deep-equal instance1 instance2))

(defgeneric diff (instance1 instance2))

(defmethod diff ((instance1 t) (instance2 t))
  (unless (deep-equal instance1 instance2)
    `(:old ,(to-list instance1)
           :new ,(to-list instance2))))

(defun get-init-forms (slot-options)
  (let* ((result (make-hash-table))
         (filtered-options (-> slot-options
                               (remove-if-not (lambda (slot-option)
                                                (eq :initform (second slot-option)))
                                              $)
                               (mapcar (lambda (slot-option)
                                         (list (first slot-option) (third slot-option)))
                                       $))))
    (dolist (option filtered-options)
      (setf (gethash (first option) result) (second option)))
    result))

(defmacro defclassf (name parents slots &rest slot-options)
  (when (not (symbolp name))
    (error "The name of the class must be a symbol."))
  (let* ((dont-serialize-slots (-> slot-options
                                   (remove-if-not (lambda (slot-option)
                                                    (eq :dont-serialize (second slot-option)))
                                                  $)
                                   (mapcar #'first $)))
         (serialize-slots (set-difference slots dont-serialize-slots))
         (initforms (get-init-forms slot-options)))
    (labels ((as-keyword (slot)
               (intern (symbol-name slot) "KEYWORD")))
      `(progn
         (defclass ,name ,parents
           ,(mapcar (lambda (slot-name)
                      (list slot-name
                            :accessor slot-name ;;(mksymb name '- slot-name)
                            :initarg (as-keyword slot-name)
                            :initform (gethash slot-name initforms)))
                    slots))
         (defun ,(mksymb 'make '- name) (&key ,@slots)
           (make-instance ',name ,@(mapcan (lambda (slot) (list (as-keyword slot) slot))
                                           slots)))
         (defun ,(mksymb 'mk '- name) ,slots
           (make-instance ',name ,@(mapcan (lambda (slot) (list (as-keyword slot) slot))
                                           slots)))
         (defmethod to-list ((instance ,name))
           (list :class ',name
                 ,@(mapcan (lambda (slot) (list (as-keyword slot)
                                                `(to-list (slot-value instance ',slot))))
                           serialize-slots)))
         (defmethod shallow-copy ((instance ,name))
           (make-instance ',name
                          ,@(mapcan (lambda (slot) (list (as-keyword slot)
                                                         `(slot-value instance ',slot)))
                                    slots)))
         (defmethod deep-equal ((instance1 ,name) (instance2 ,name))
           (every #'deep-equal
                  (list ,@(mapcar (lambda (slot)
                                    `(slot-value instance1 ',slot))
                                  serialize-slots))
                  (list ,@(mapcar (lambda (slot)
                                    `(slot-value instance2 ',slot))
                                  serialize-slots))))
         (defmethod diff ((instance1 ,name) (instance2 ,name))
           (unless (deep-equal instance1 instance2)
             (remove-if-not (lambda (elm) (third elm))
                            (list ,@(mapcar (lambda (slot)
                                              `(list :change ',slot
                                                     (diff
                                                      (slot-value instance1 ',slot)
                                                      (slot-value instance2 ',slot))))
                                            serialize-slots)))))))))

(defun grep-apropos (apropos-arg &rest grep-args)
  (let* ((apropos-result (with-output-to-string (str)
                           (let ((*standard-output* str))
                             (apropos apropos-arg))))
         (result-lines (split-sequence:split-sequence #\Newline apropos-result
                                                      :remove-empty-subseqs t)))
    (dolist (arg grep-args)
      (setf result-lines (remove-if-not (lambda (line) (search (string-upcase arg)
                                                               (string-upcase line)))
                                        result-lines)))
    (dolist (line result-lines)
      (print line))))

(defmacro print-all (stream &rest exprs)
  (let ((format-string (with-output-to-string (str)
                         (dolist (expr exprs)
                           (cond ((stringp expr)
                                  (format str "~~a"))
                                 (t
                                  (format str "~a: " expr)
                                  (format str "~~s")
                                  (format str "~%")))))))
    `(format ,stream ,format-string ,@exprs)))

(defmacro ematch (expr &body clauses)
  `(match ,expr
     ,@(append clauses
               '((_ (error "EMATCH: no clause matched."))))))

(defmacro aif (test then &optional (else nil))
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it ,@body)))

(defmacro bif ((var test) then &optional (else nil))
  `(let ((,var ,test))
     (if ,var ,then ,else)))

(defmacro bwhen ((var test) &body body)
  `(let ((,var ,test))
     (when ,var ,@body)))

(defmacro dohash (((key value) hash) &body body)
  `(maphash (lambda (,key ,value)
              (declare (ignorable ,key ,value))
              ,@body)
            ,hash))
