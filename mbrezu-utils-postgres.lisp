
(in-package :mup)

(defmacro with-connection ((connection
                            database user password host
                            &optional (port 5432) (use-ssl :no)) &body body)
  `(let ((,connection (cl-postgres:open-database ,database ,user ,password ,host ,port ,use-ssl)))
     (unwind-protect
          (progn
            ,@body)
       (cl-postgres:close-database ,connection))))

(defmacro with-transaction ((connection) &body body)
  (let ((g-ok (gensym "ok"))
        (g-connection (gensym "connection")))
    `(let (,g-ok (,g-connection ,connection))
       (cl-postgres:exec-query ,g-connection "START TRANSACTION ISOLATION LEVEL SERIALIZABLE")
       (unwind-protect
            (prog1
                ,@body
              (setf ,g-ok t))
         (if ,g-ok
             (cl-postgres:exec-query ,g-connection "COMMIT")
             (cl-postgres:exec-query ,g-connection "ROLLBACK"))))))

(defvar *default-row-reader* #'cl-postgres:alist-row-reader)

(defun exec (connection query &rest parameters)
  (if (not parameters)
      (cl-postgres:exec-query connection query *default-row-reader*)
      (let ((name (-> "name" gensym symbol-name)))
        (cl-postgres:prepare-query connection name query)
        (prog1
            (cl-postgres:exec-prepared connection name parameters
                                       *default-row-reader*)
          (cl-postgres:exec-query connection
                                  (format nil "DEALLOCATE \"~a\"" name))))))

(defun nillify (value)
  (if (eq value :null)
      nil
      value))

(defun exec-scalar (connection query &rest parameters)
  (-> (apply #'exec connection query parameters)
      first
      first
      nillify))

(defmacro retry-on-serialization-error (&body body)
  (let ((g-retry (gensym "retry"))
        (g-result (gensym "result")))
    `(let (,g-result)
       (tagbody
          ,g-retry
          (handler-case
              (setf ,g-result (progn ,@body))
            (cl-postgres-error:serialization-failure ()
              (go ,g-retry))))
       ,g-result)))
