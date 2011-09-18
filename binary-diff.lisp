
(in-package :mabu)

(defvar *hash-base* 101)

(declaim (optimize debug))

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

;;;; Binary encoding of binary patches:
;;;;
;;;; Since Message Pack seems to be wasteful for such limited input a
;;;; special scheme is used.
;;;;
;;;; A patch is encoded as a series of commands. The commands are:
;;;;
;;;; 1. End-of-Patch - when this is read, the patch is finished. An
;;;; empty patch is encoded as a single End-of-Patch command.
;;;;
;;;; 2. Copy - has two arguments, the source address and the length of
;;;; the segment to copy.
;;;;
;;;; 3. Add - has one argument, a sequence of bytes. The sequence of
;;;; bytes is encoded by writing the length of the sequence first,
;;;; then the bytes themselves.
;;;;
;;;; The opcodes for the commands are start with one byte. The command
;;;; itself is encoded in the 2 MSB of the byte, as follows:
;;;;
;;;; 00 - end-of-patch
;;;; 01 - copy
;;;; 10 - add
;;;; 11 - reserved
;;;;
;;;; 01 - if the command is a copy, the two arguments of the copy
;;;; operations are encoded next. The LSB bits of the opcode byte are
;;;; organized as follows: SSSLLL. The 3 S bits encode the length in
;;;; bytes for the source address (add one to the value encoded in the
;;;; bytes to find the actual length of the source address: 000 means
;;;; 1 byte, 111 means 8 bytes). The 3 LLL bytes encode the length of
;;;; the 'length of segment' argument. The source address and segment
;;;; length are encoded in big-endian order.
;;;;
;;;; Examples:
;;;; 01000000 00000000 10000000 means (:copy 0 128)
;;;; 01001000 00000001 00000000 10000000 means (:copy 256 128)
;;;;
;;;; 10 - if the operation is an add, if the next bit after 01 is 0,
;;;; the last five bits of the opcode byte encode the length of the
;;;; sequence (minus one). If the next bit after 01 is 1, then the
;;;; last five bits encode the length in bytes (minus one) of the
;;;; encoding of the sequence length. If the sequence length is
;;;; encoded separately, then it follows the opcode byte and uses big
;;;; endian order. The sequence of bytes itself comes last.
;;;;
;;;; Examples:
;;;; 10000000 10000001 means (:add #(129))
;;;; 10100000 00001000 <four bytes> means (:add #(<four bytes>))
;;;; 10100001 00000001 00000000 <256 bytes> means (:add #(<256 bytes))

(defun get-int-length-in-bytes (int)
  (cond ((< int (expt 2 8)) 0)
        ((< int (expt 2 16)) 1)
        ((< int (expt 2 32)) 3)
        ((< int (expt 2 64)) 7)
        (t (error "Source address too large."))))

(defun byte-combine (&rest args)
  (let ((result 0)
        (groups (group args 2)))
    (dolist (g groups)
      (setf result (* result (expt 2 (first g))))
      (incf result (second g)))
    result))

(defmacro store-big-endian (number stream byte-count)
  (let ((g-number (gensym "number"))
        (g-stream (gensym "stream"))
        (g-i (gensym "i"))
        (g-byte-count (gensym "byte-count")))
    `(let ((,g-number ,number)
           (,g-stream ,stream))
       ,@(if (numberp byte-count)
             (loop for i from (1- byte-count) downto 0
                collect `(write-byte (ldb (byte 8 ,(* 8 i)) ,g-number) ,g-stream))
             `((let ((,g-byte-count ,byte-count))
                 (loop for ,g-i from (1- ,g-byte-count) downto 0
                    do (write-byte (ldb (byte 8 (* 8 ,g-i)) ,g-number) ,g-stream))))))))

(defmacro load-big-endian (stream byte-count)
  (let ((g-stream (gensym "stream")))
    `(let ((,g-stream ,stream)
           (result 0))
       ,@(if (numberp byte-count)
             (loop
                repeat byte-count
                collect `(setf result (+ (ash result 8)
                                         (read-byte ,g-stream))))
             `((loop
                  repeat ,byte-count
                  do (setf result (+ (ash result 8)
                                     (read-byte ,g-stream))))))
       result)))

(defun binary-patch-encode (patches stream)
  (dolist (patch patches)
    (ecase (first patch)
      ((:copy)
       (let ((sss-bits (get-int-length-in-bytes (second patch)))
             (lll-bits (get-int-length-in-bytes (third patch))))
         (write-byte (byte-combine 2 #b01 3 sss-bits 3 lll-bits) stream)
         (store-big-endian (second patch) stream (1+ sss-bits))
         (store-big-endian (third patch) stream (1+ lll-bits))))
      ((:add)
       (let ((length-bits (1- (length (second patch)))))
         (if (< length-bits 32)
             (write-byte (byte-combine 2 #b10 1 0 5 length-bits) stream)
             (let ((lllll-bits (get-int-length-in-bytes (length (second patch)))))
               (write-byte (byte-combine 2 #b10 1 1 5 lllll-bits)
                           stream)
               (store-big-endian (length (second patch)) stream (1+ lllll-bits))))
         (write-sequence (second patch) stream)))))
  (write-byte 0 stream))

(defun binary-patch-decode (stream &optional acc)
  (let ((byte (read-byte stream)))
    (ecase (ldb (byte 2 6) byte)
      ((#b00) (reverse acc))
      ((#b01)
       (let ((sss-bits (1+ (ldb (byte 3 3) byte)))
             (lll-bits (1+ (ldb (byte 3 0) byte))))
         (-> (cons (list :copy
                         (load-big-endian stream sss-bits)
                         (load-big-endian stream lll-bits))
                   acc)
             (binary-patch-decode stream $))))
      ((#b10)
       (let (array)
         (if (= 0 (ldb (byte 1 5) byte))
             (let* ((array-length (1+ (ldb (byte 5 0) byte))))
               (setf array (make-array array-length :element-type '(mod 256)))
               (read-sequence array stream))
             (let* ((lllll-bits (1+ (ldb (byte 5 0) byte)))
                    (length (load-big-endian stream lllll-bits)))
               (setf array (make-array length :element-type '(mod 256)))
               (read-sequence array stream)))
         (-> (cons (list :add array) acc)
             (binary-patch-decode stream $)))))))
