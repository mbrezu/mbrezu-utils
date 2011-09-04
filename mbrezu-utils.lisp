;;;; mbrezu-utils.lisp

(in-package #:mbrezu-utils)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (format nil "~{~a~}" args))

  (defun mksymb (&rest args)
    (intern (apply #'mkstr args))))

(defgeneric to-list (instance))

(defgeneric deep-equal (instance1 instance2))

(defun from-list (list)
  (cond ((atom list) list)
        ((consp list)
         (cond ((eq :atom (car list)) (cdr list))
               ((eq :class (car list))
                (apply #'make-instance
                       (second list)
                       (mapcar (lambda (elm)
                                 (cond ((keywordp elm) elm)
                                       (t (from-list elm))))
                               (nthcdr 2 list))))
               (t (mapcar #'from-list list))))))

(defmethod to-list ((instance t))
  (cond ((eq :class instance)
         (cons :atom :class))
        ((atom instance)
         instance)
        (t (error "Not an atom."))))

(defmethod to-list ((instance cons))
  (mapcar #'to-list instance))

(defmethod deep-equal ((instance1 t) (instance2 t))
  (equalp instance1 instance2))

(defmethod deep-equal ((instance1 sequence) (instance2 sequence))
  (every #'deep-equal instance1 instance2))

(defgeneric diff (instance1 instance2))

;; TBD -- longest matching subsequence etc.
(defmethod diff ((instance1 sequence) (instance2 sequence))
  )

(defmethod diff ((instance1 t) (instance2 t))
  (unless (deep-equal instance1 instance2)
    `(:old ,(to-list instance1)
           :new ,(to-list instance2))))

(defmacro defclassf (name parents slots)
  (when (not (symbolp name))
    (error "The name of the class must be a symbol."))
  (labels ((as-keyword (slot)
             (intern (symbol-name slot) "KEYWORD")))
    `(progn
       (defclass ,name ,parents
         ,(mapcar (lambda (slot-name)
                    (list slot-name
                          :accessor slot-name ;;(mksymb name '- slot-name)
                          :initarg (as-keyword slot-name)
                          :initform nil))
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
                         slots)))
       (defmethod deep-equal ((instance1 ,name) (instance2 ,name))
         (every #'deep-equal
                (list ,@(mapcar (lambda (slot)
                                  `(slot-value instance1 ',slot))
                                slots))
                (list ,@(mapcar (lambda (slot)
                                  `(slot-value instance2 ',slot))
                                slots))))
       (defmethod diff ((instance1 ,name) (instance2 ,name))
         (unless (deep-equal instance1 instance2)
           (-> (list ,@(mapcar (lambda (slot)
                                 `(list :change ',slot
                                        (diff
                                         (slot-value instance1 ',slot)
                                         (slot-value instance2 ',slot))))
                               slots))
               (remove-if-not (lambda (elm) (third elm)) $)))))))


