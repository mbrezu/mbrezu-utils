
(in-package :mup)

(defmacro with-connection ((connection
                            database user password host
                            &optional (port 5432) (use-ssl :no)) &body body)
  `(let ((,connection (cl-postgres:open-database ,database ,user ,password ,host ,port ,use-ssl)))
     (unwind-protect
          (progn
            ,@body)
       (cl-postgres:close-database ,connection))))

(defmacro with-transaction (connection &body body)
  (let ((g-ok (gensym "ok"))
        (g-connection (gensym "connection")))
    `(let (,g-ok (,g-connection ,connection))
       (cl-postgres:exec-query ,g-connection "START TRANSACTION ISOLATION LEVEL SERIALIZABLE")
       (unwind-protect
            (progn
              ,@body
              (setf ,g-ok t))
         (if ,g-ok
             (cl-postgres:exec-query ,g-connection "COMMIT")
             (cl-postgres:exec-query ,g-connection "ROLLBACK"))))))

(defun exec (connection query &rest parameters)
  (if (not parameters)
      (cl-postgres:exec-query connection query #'cl-postgres:alist-row-reader)
      (let ((name (-> "name" gensym symbol-name)))
        (cl-postgres:prepare-query connection name query)
        (cl-postgres:exec-prepared connection name parameters #'cl-postgres:alist-row-reader)
        (cl-postgres:exec-query connection (format nil "DEALLOCATE \"~a\"" name)))))

(defmacro retry-on-serialization-error (&body body)
  `(tagbody
    retry
      (handler-case
          (progn ,@body)
        (cl-postgres-error:serialization-failure () (go retry)))))