
(in-package :mabu)

(defun ls-score (instructions &optional (acc 0))
  (cond ((null instructions) acc)
        ((eq (car instructions) 'keep)
         (ls-score (cdr instructions) (+ 1 acc)))
        ((eq (caar instructions) 'drop)
         (ls-score (cdr instructions) (+ (* 4 (cadar instructions)) acc)))
        ((eq (caar instructions) 'add)
         (ls-score (cdr instructions) (+ (* 8 (length (cdar instructions)))
                                         acc)))
        (t (error "can't score"))))

(defun longest-subseq (seq1 seq2 &optional acc)
  (cond ((and (null seq1) (null seq2))
         (reverse acc))
        ((null seq1)
         (longest-subseq nil nil (cons (list 'add seq2)
                                       acc)))
        ((null seq2)
         (longest-subseq nil nil (cons (list 'drop (length seq1))
                                       acc)))
        ((equal (first seq1) (first seq2))
         (longest-subseq (rest seq1) (rest seq2) (cons 'keep acc)))
        (t
         (let ((v1 (longest-subseq seq1 (rest seq2) (cons (list 'add (list (first seq2)))
                                                          acc)))
               (v2 (longest-subseq (rest seq1) seq2 (cons (list 'drop 1)
                                                          acc))))
           (if (>= (ls-score v1) (ls-score v2))
               v2
               v1)))))

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

(defun combine (seq)
  (cond ((eq 'keep (car seq))
         (list 'keep (length seq)))
        ((and (consp (car seq))
              (eq (caar seq) 'add))
         (list 'add (apply #'append (mapcar #'second seq))))
        ((and (consp (car seq))
              (eq (caar seq) 'drop))
         (list 'drop (reduce #'+ (mapcar #'second seq))))))

;; (group-by (longest-subseq (coerce "Ana are mere." 'list) (coerce "Ana are paere." 'list)))

;; (group-by (longest-subseq (coerce "Ana are mere." 'list) (coerce "Ana are paere." 'list))
;;                    :predicate (lambda (e1 e2)
;;                                 (cond ((and (consp e1) (consp e2))
;;                                        (eq (car e1) (car e2)))
;;                                       (t (equal e1 e2)))))

;; (mapcar #'combine
;;         (group-by (longest-subseq (coerce "Ana are mere." 'list)
;;                                   (coerce "Ana are paere." 'list))
;;                   :predicate (lambda (e1 e2)
;;                                (cond ((and (consp e1) (consp e2))
;;                                       (eq (car e1) (car e2)))
;;                                      (t (equal e1 e2))))))