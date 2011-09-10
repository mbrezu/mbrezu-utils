
(in-package :mbrezu-utils-tests)

(declaim (optimize debug))

(def-suite mbrezu-utils-tests
    :description "Tests for mbrezu-utils.")

(in-suite mbrezu-utils-tests)

(mabu:defclassf point () (x y))

(mabu:defclassf rectangle () (ul-corner dr-corner))

(test basic-to-list
  "Test that converting a class to a list works."
  (is (equalp '(:CLASS POINT :X 1 :Y 2)
              (to-list (mk-point 1 2))))
  (is (equalp '(:CLASS RECTANGLE
                :UL-CORNER (:CLASS POINT :X 0 :Y 1)
                :DR-CORNER (:CLASS POINT :X 2 :Y 3))
              (to-list (mk-rectangle (mk-point 0 1) (mk-point 2 3)))))
  (is (equalp '((:CLASS POINT :X 1 :Y 2) (:CLASS POINT :X 2 :Y 3))
              (to-list (list (mk-point 1 2) (mk-point 2 3))))))

(test basic-from-list
  "Test that converting a class into a list and back into a class
works."
  (let ((tests (list (mk-point 1 2)
                     (list (mk-point 1 2) (mk-point 1 3))
                     (mk-rectangle (mk-point 1 2) (mk-point 1 3)))))
    (dolist (test tests)
      (is (deep-equal test (-> test to-list from-list))))))

(mabu:defclassf person () (name age gender))

(mabu:defclassf meeting () (people name mtime))

(mabu:defclassf schedule () (meetings author))

(test complex-to-list
  "Test that more complex classes are properly converted to lists and back."
  (let ((schedule
         (make-schedule :author (mk-person "John" 24 :male)
                        :meetings (list (make-meeting :people
                                                      (list (mk-person "John" 24 :male)
                                                            (mk-person "Jane" 22 :female)
                                                            (mk-person "Andrew" 30 :male))
                                                      :name "Meeting 1"
                                                      :mtime "Today")
                                        (make-meeting :people
                                                      (list (mk-person "John" 24 :male)
                                                            (mk-person "Anne" 28 :female)
                                                            (mk-person "Simon" 20 :male))
                                                      :name "Meeting 2"
                                                      :mtime "Tomorrow")))))
    (is (deep-equal schedule (from-list (to-list schedule))))))

(test corner-cases
  "Test that escaping for :class works."
  (let ((tests (list '(:class 1)
                     '(:class)
                     '((:atom :class))
                     '((:atom . :class)))))
    (dolist (test tests)
      (is (deep-equal test (-> test to-list from-list))))))

(test diff
  "Test that diffing fields works."
  (is (equalp '((:CHANGE DR-CORNER ((:CHANGE Y (:OLD 3 :NEW 4)))))
              (diff (mk-rectangle (mk-point 1 2) (mk-point 2 3))
                    (mk-rectangle (mk-point 1 2) (mk-point 2 4))))))

(test list-diff
  "Test that list diffing and patching works."
  (is (equalp "Ana are paere."
              (-> (list-diff (coerce "Ana are mere." 'list) (coerce "Ana are paere." 'list))
                  (list-patch (coerce "Ana are mere." 'list) $)
                  (coerce $ 'string)))))