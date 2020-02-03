;;; weak-ref-tests.el -- tests for weak-ref -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'weak-ref)

(defvar weak-ref-tests-hold nil
  "Strongly holds onto an object.")

(defun weak-ref-tests-create ()
  "Return a new weak reference with a freshly-allocated object.
The reference object is also stored in `weak-ref-tests-hold'."
  (weak-ref (setf weak-ref-tests-hold (list 1 2 3))))

(defun weak-ref-tests-present-p (ref)
  "Return t if the referened object is still alive."
  (not (null (weak-ref-deref ref))))

(ert-deftest weak-ref ()
  ;; Always use indirection when handling the weakly-referenced object
  ;; so that it doesn't wind up on the local function stack, where it
  ;; will be kept alive.
  (let ((ref (weak-ref-tests-create)))
    (should (weak-ref-tests-present-p ref))
    (setf weak-ref-tests-hold nil)
    (garbage-collect)
    (should (null (weak-ref-tests-present-p ref)))))

(provide 'weak-ref-tests)

;;; weak-ref-tests.el ends here
