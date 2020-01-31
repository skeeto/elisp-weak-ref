;;; weak-ref.el --- weak references for Emacs Lisp

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; Version: 1.0

;;; Commentary;

;; The Emacs Lisp environment supports weak references, but only for
;; hash table keys and values. This can be exploited to generalize
;; weak references into two convenient macros:

;;   * `weak-ref'   : create a weak reference to an object
;;   * `weak-deref' : access the object behind a weak reference

;; The weakness can be demonstrated like so:

;;     (setq ref (weak-ref (list 1 2 3)))
;;     (weak-deref ref) ; => (1 2 3)
;;     (garbage-collect)
;;     (weak-deref ref) ; => nil

;;; Code:

(defun weak-ref (thing)
  "Return a new weak reference to THING.
The referenced object will not be protected from garbage
collection by this reference."
  (let ((ref (make-hash-table :size 1 :weakness t :test 'eq)))
    (prog1 ref
      (puthash t thing ref))))

(defun weak-deref (ref)
  "Return the object referenced by REF.
Return NIL if the object no longer exists (garbage collected)."
  (gethash t ref))

(provide 'weak-ref)

;;; weak-ref.el ends here
