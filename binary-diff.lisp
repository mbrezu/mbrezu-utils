
(in-package :mabu)

(defvar *hash-base* 101)

(defmacro to-fixnum (value)
  `(logand most-positive-fixnum ,value))

(defun hash (bytes index block-size old-hash hash-exp &optional (remove-last t))
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum old-hash index block-size hash-exp))
  (let ((new-hash (the fixnum (+ (the fixnum (* old-hash (the fixnum *hash-base*)))
                                 (the fixnum (svref bytes index))))))
    (to-fixnum (if (or (not remove-last) (< index block-size))
                   new-hash
                   (- new-hash (the fixnum (* (the fixnum (aref bytes (- index block-size)))
                                             hash-exp)))))))

(defun extend-backwards (bytes match-1 match-2 block-size lower-target-limit
                         &optional (extra-length 0))
  (if (or (= 0 match-1) (= lower-target-limit match-2) (= 1 block-size))
      (values match-1 match-2 extra-length)
      (if (= (aref bytes (1- match-1))
             (aref bytes (1- match-2)))
          (extend-backwards bytes
                            (1- match-1) (1- match-2)
                            (1- block-size)
                            lower-target-limit
                            (1+ extra-length))
          (values match-1 match-2 extra-length))))

(defun extend-forwards (bytes loc-1 loc-2 &optional (extra-length 0))
  (if (or (= loc-1 (length bytes))
          (= loc-2 (length bytes))
          (not (= (aref bytes loc-1)
                  (aref bytes loc-2))))
      extra-length
      (extend-forwards bytes (1+ loc-1) (1+ loc-2) (1+ extra-length))))

(defstruct match source target length)

(defun verify-extend-match (bytes match-1 match-2 block-size lower-target-limit)
  (unless (equalp (subseq bytes match-1 (+ match-1 block-size))
                  (subseq bytes match-2 (+ match-2 block-size)))
    (return-from verify-extend-match nil))
  (let (start-match-1 start-match-2 extra-length extra-length-2)
    (setf (values start-match-1 start-match-2 extra-length)
          (extend-backwards bytes match-1 match-2 block-size lower-target-limit))
    (setf extra-length-2
          (extend-forwards bytes
                           (+ match-1 block-size)
                           (+ match-2 block-size)))
    (make-match :source start-match-1
                :target start-match-2
                :length (+ block-size extra-length extra-length-2))))

(defun hash-walk (bytes block-size source-size)
  (let ((result (make-array (length bytes)))
        (sub-strings (make-hash-table))
        (hash 0)
        matches
        last-match
        (hash-exp (to-fixnum (expt *hash-base* block-size))))
    (dotimes (i (length bytes))
      (when (and (>= i source-size)
                 (or (null last-match)
                     (> (- i block-size)
                        (+ (match-target last-match)
                           (match-length last-match)))))
        (let ((potential-match (gethash hash sub-strings))
              (lower-target-limit (if last-match
                                      (+ (match-target last-match)
                                         (match-length last-match)
                                         1)
                                      0)))
          (when potential-match
            (let ((verified-match (verify-extend-match bytes
                                                       potential-match (- i block-size)
                                                       block-size
                                                       lower-target-limit)))
              (when verified-match
                (push verified-match matches)
                (setf last-match verified-match))))))
      (when (and (= 0 (mod i block-size))
                 (< i source-size))
        (setf (gethash hash sub-strings) (- i block-size)))
      (setf hash (hash bytes i block-size hash hash-exp))
      (setf (aref result i) hash))
    (reverse matches)))

(defun make-commands (matches index offset target-bytes &optional acc)
  (if matches
      (let* ((match (first matches))
             (target-pos (- (match-target match) offset)))
        (if (< index target-pos)
            (make-commands matches target-pos offset
                           target-bytes
                           (cons (list :add (subseq target-bytes index target-pos))
                                 acc))
            (make-commands (rest matches)
                           (+ target-pos (match-length match))
                           offset
                           target-bytes
                           (cons (list :copy (match-source match) (match-length match))
                                 acc))))
      (if (= index (length target-bytes))
          (reverse acc)
          (reverse (cons (list :add (subseq target-bytes index))
                         acc)))))

(defun binary-diff (bytes-1 bytes-2)
  (let* ((concatenated (concatenate 'simple-vector bytes-1 bytes-2))
         (block-size (max 5 (floor (/ (length concatenated) 1000))))
         (matches (hash-walk concatenated block-size (length bytes-1)))
         result)
    (setf result (make-commands matches 0 (length bytes-1) bytes-2))
    (when (and (= 1 (length result))
               (eq :copy (-> result first first))
               (= 0 (-> result first second))
               (= (length bytes-1) (-> result first third)))
      (setf result nil))
    result))

(defun target-length (patches)
  (let ((result 0))
    (dolist (patch patches)
      (cond ((eq :copy (first patch))
             (incf result (third patch)))
            ((eq :add (first patch))
             (incf result (length (second patch))))
            (t (error "Broken patch."))))
    result))

(defun binary-patch (bytes-1 patches)
  (if (not patches)
      (copy-seq bytes-1)
      (let ((bytes-2 (make-array (target-length patches) :element-type '(mod 256)))
            (target-index 0))
        (dolist (patch patches)
          (cond ((eq :copy (first patch))
                 (setf (subseq bytes-2 target-index (+ target-index (third patch)))
                       (subseq bytes-1 (second patch) (+ (second patch) (third patch))))
                 (incf target-index (third patch)))
                ((eq :add (first patch))
                 (setf (subseq bytes-2 target-index (+ target-index (length (second patch))))
                       (second patch))
                 (incf target-index (length (second patch))))
                (t (error "Broken patch."))))
        bytes-2)))

