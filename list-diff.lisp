
(in-package :mabu)

(declaim (optimize debug))

(defun tree-length (list &optional (acc 0))
  (cond ((null list) acc)
        ((atom list) (+ acc 1))
        ((consp (car list)) (tree-length (cdr list) (+ acc (tree-length (car list)))))
        (t (tree-length (cdr list) (1+ acc)))))

(defun diff-score (instructions &optional (acc 0) (last nil))
  (cond ((null instructions) acc)
        (t (ematch (car instructions)
             ((list :keep arg) (diff-score (cdr instructions)
                                           (+ acc (if (eq last :keep) 0 1))
                                           :keep))
             ((list :drop arg) (diff-score (cdr instructions)
                                           (+ acc (if (eq last :drop) 0 1))
                                           :drop))
             ((list :add arg) (diff-score (cdr instructions)
                                          (+ acc (tree-length arg))
                                          :add))
             ((list :into arg) (diff-score (cdr instructions)
                                           (+ acc (diff-score arg))
                                           :into))
             (:drop-tail (diff-score (cdr instructions)
                                     (1+ acc)))
             (:keep-tail (diff-score (cdr instructions)
                                     (1+ acc)))
             ((list :replace-tail arg) (diff-score (cdr instructions)
                                                   (+ acc 1 (tree-length arg))))
             ((list :add-tail arg) (diff-score (cdr instructions)
                                               (+ acc (tree-length arg))))))))

(defun longest-subseq-impl (seq-1 seq-2 index-1 index-2 storage)
  (cond ((and (null seq-1) (null seq-2))
         nil)
        ((not (listp seq-1))
         (cond ((or (null seq-2) (consp seq-2))
                (cons :drop-tail (longest-subseq nil seq-2
                                                 (1+ index-1) index-2
                                                 storage)))
               ((equalp seq-1 seq-2)
                (cons :keep-tail (longest-subseq nil nil
                                                 (1+ index-1) (1+ index-2)
                                                 storage)))
               (t (cons (list :replace-tail seq-2)
                        (longest-subseq nil nil
                                        (1+ index-1) (1+ index-2)
                                        storage)))))
        ((not (listp seq-2))
         (cond ((null seq-1)
                (cons (list :add-tail seq-2) (longest-subseq nil nil
                                                             index-1 (1+ index-2)
                                                             storage)))
               (t (cons (list :drop 1) (longest-subseq (rest seq-1) seq-2
                                                       (1+ index-1) index-2
                                                       storage)))))
        ((null seq-1)
         (cons (list :add (list (car seq-2)))
               (longest-subseq nil (rest seq-2)
                               index-1 (1+ index-2)
                               storage)))
        ((null seq-2)
         (cons (list :drop 1)
               (longest-subseq (rest seq-1) nil
                               (1+ index-1) index-2
                               storage)))
        ((equalp (first seq-1) (first seq-2))
         (cons (list :keep 1) (longest-subseq (rest seq-1) (rest seq-2)
                                              (1+ index-1) (1+ index-2)
                                              storage)))
        (t
         (let ((candidates (list (cons (list :add (list (first seq-2)))
                                       (longest-subseq seq-1 (rest seq-2)
                                                       index-1 (1+ index-2)
                                                       storage))
                                 (cons (list :drop 1)
                                       (longest-subseq (rest seq-1) seq-2
                                                       (1+ index-1) index-2
                                                       storage)))))
           (if (and (consp (car seq-1))
                    (consp (car seq-2)))
               (push (cons (list :into (longest-subseq (car seq-1) (car seq-2)
                                                       0 0))
                           (longest-subseq (rest seq-1) (rest seq-2)
                                           (1+ index-1) (1+ index-2)
                                           storage))
                     candidates))
           (setf candidates (mapcar #'(lambda (diff) (cons (diff-score diff) diff))
                                    candidates))
           (setf candidates (sort candidates #'< :key #'car))
           (-> candidates first cdr)))))

(defun make-storage (size1 size2)
  (make-array (list size1 size2) :initial-element '(nil nil)))

(defun get-element (storage idx1 idx2)
  (values-list (aref storage idx1 idx2)))

(defun set-element (storage idx1 idx2 value)
  (setf (aref storage idx1 idx2) (list value t)))

(defun length2 (list)
  (cond ((null list) 0)
        ((atom list) 1)
        (t (1+ (length2 (cdr list))))))

(defun longest-subseq (seq-1 seq-2 index-1 index-2
                       &optional (storage (make-storage (1+ (length2 seq-1))
                                                        (1+ (length2 seq-2)))))
  (multiple-value-bind (value found) (get-element storage index-1 index-2)
    (unless found
      (setf value (longest-subseq-impl seq-1 seq-2
                                       index-1 index-2
                                       storage))
      (set-element storage index-1 index-2 value))
    value))

(defun group-by-impl (seq pred acc current-group)
  (cond ((null seq) (if current-group
                        (reverse (cons (reverse current-group) acc))
                        acc))
        ((and current-group (funcall pred (first seq) (first current-group)))
         (group-by-impl (rest seq) pred acc (cons (first seq) current-group)))
        ((and current-group (not (funcall pred (first seq) (first current-group))))
         (group-by-impl (rest seq) pred (cons (reverse current-group) acc) (list (first seq))))
        (t (group-by-impl (rest seq) pred acc (list (first seq))))))

(defun group-by (seq &key (predicate #'equal))
  (group-by-impl seq predicate nil nil))

(defun list-diff (list1 list2)
  (labels ((combine (seq)
             (ematch (car seq)
               ((list :keep _) (list :keep (reduce #'+ (mapcar #'second seq))))
               ((list :drop _) (list :drop (reduce #'+ (mapcar #'second seq))))
               ((list :add _) (list :add (apply #'append (mapcar #'second seq))))
               ((or (list :into _) (list :add-tail _) (list :replace-tail _)
                    :keep-tail  :drop-tail)
                (first seq))))
           (rle-compress (diff)
             (-> diff
                 (group-by $ :predicate (lambda (e1 e2)
                                          (cond ((and (consp e1)
                                                      (consp e2)
                                                      (-> e1 first (eq $ :into) not))
                                                 (eq (car e1) (car e2)))
                                                (t (equal e1 e2)))))
                 (mapcar #'combine $)
                 (mapcar (lambda (elem)
                           (ematch elem
                             ((list :into arg) (list :into (rle-compress (second elem))))
                             (_ elem)))
                         $))))
    (rle-compress (longest-subseq list1 list2 0 0))))

(defun list-patch (list diff)
  (cond ((null diff) nil)
        (t (ematch (car diff)
             ((list :keep arg) (append (subseq list 0 arg)
                                       (list-patch (subseq list arg)
                                                   (cdr diff))))
             ((list :add arg) (append arg (list-patch list
                                                      (cdr diff))))
             ((list :drop arg) (list-patch (nthcdr arg list)
                                           (cdr diff)))
             (:drop-tail nil)
             (:keep-tail list)
             ((or (list :replace-tail arg)
                  (list :add-tail arg)) arg)
             ((list :into arg) (cons (list-patch (car list)
                                                 (-> diff first second))
                                     (list-patch (cdr list) (cdr diff))))))))



