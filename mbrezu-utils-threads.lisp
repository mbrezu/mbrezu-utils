
(in-package :mut)

(defvar *log* nil)

(defvar *lock* (make-lock))

(defun clear-log ()
  (with-lock-held (*lock*)
    (setf *log* nil)))

(defun log-message (line &rest args)
  (with-lock-held (*lock*)
    (push (apply #'format nil line args) *log*)))

(defun get-log ()
  (with-lock-held (*lock*)
    (reverse *log*)))

(defun log-sleep (time moment)
  (let ((moment-description (format nil "<<~a on ~a>>" moment (thread-name (current-thread)))))
    (log-message (format nil "Start sleep for ~a" moment-description))
    (sleep time)
    (log-message (format nil "End sleep for ~a" moment-description))))
