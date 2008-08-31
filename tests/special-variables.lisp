;;;; -*- lisp -*-

(in-package :cl-delico-test)

(defsuite* (test/special-variables :in test))

;;; special variable handling
(defun/cc lookup-special-in-defun/cc (stop)
  (declare (special the-spec-var))
  (when stop (let/cc k k))
  the-spec-var)

(defun/cc lookup-special-in-let/cc (stop)
  (let ((normal 0))
    (declare (special the-spec-var))
    (when stop (let/cc k k))
    the-spec-var))

(defun/cc lookup-special-in-let*/cc (stop)
  (let* ((normal 0))
    (declare (special the-spec-var))
    (when stop (let/cc k k))
    the-spec-var))

(defun lookup-special-in-lisp (&optional (variable-name 'the-spec-var))
  (symbol-value variable-name))

(defun/cc define-and-lookup-special-in-defun/cc (stop)
  (let ((the-spec-var 1))
    (declare (special the-spec-var))
    (when stop (let/cc k k))
    the-spec-var))

(defun/cc export-special-from-let/cc-and-lookup-in-defun/cc (stop)
  (let ((the-spec-var 1))
    (declare (special the-spec-var))
    (lookup-special-in-defun/cc stop)))

(defun/cc export-special-from-let/cc-and-lookup-in-let/cc (stop)
  (let ((the-spec-var 1))
    (declare (special the-spec-var))
    (lookup-special-in-let/cc stop)))

(defun/cc export-special-from-let/cc-and-lookup-in-let*/cc (stop)
  (let ((the-spec-var 1))
    (declare (special the-spec-var))
    (lookup-special-in-let*/cc stop)))

(defun/cc export-special-from-let/cc-and-lookup-in-lisp (stop)
  (let ((the-spec-var 1))
    (declare (special the-spec-var))
    (when stop (let/cc k k))
    (lookup-special-in-lisp)))

(defun/cc export-special-from-let*/cc-and-lookup-in-defun/cc (stop)
  (let* ((the-spec-var 1))
    (declare (special the-spec-var))
    (lookup-special-in-defun/cc stop)))

(defun/cc export-special-from-let*/cc-and-lookup-in-let/cc (stop)
  (let* ((the-spec-var 1))
    (declare (special the-spec-var))
    (lookup-special-in-let/cc stop)))

(defun/cc export-special-from-let*/cc-and-lookup-in-let*/cc (stop)
  (let* ((the-spec-var 1))
    (declare (special the-spec-var))
    (lookup-special-in-let*/cc stop)))

(defun/cc export-special-from-let*/cc-and-lookup-in-lisp (stop)
  (let* ((the-spec-var 1))
    (declare (special the-spec-var))
    (when stop (let/cc k k))
    (lookup-special-in-lisp)))

(defun export-special-from-lisp-and-lookup-in-defun/cc (stop)
  (let ((the-spec-var 1))
    (declare (special the-spec-var))
    (with-call/cc
      (declare (special the-spec-var)) ; this declaration tells with-call/cc that the-spec-var is a special var.
      (lookup-special-in-defun/cc stop))))

(defun export-special-from-lisp-and-lookup-in-let/cc (stop)
  (let ((the-spec-var 1))
    (declare (special the-spec-var))
    (with-call/cc
      (lookup-special-in-let/cc stop))))

(defun export-special-from-lisp-and-lookup-in-let*/cc (stop)
  (let ((the-spec-var 1))
    (declare (special the-spec-var))
    (with-call/cc
      (lookup-special-in-let*/cc stop))))

(defmacro test-special (name)
  (let ((body-without-stop `(,name nil))
        (body-with-stop `(,name t)))
    `(deftest ,(symbolicate 'test/special-variables/ name) ()
      (is (= 1 (with-call/cc ,body-without-stop)))
      (signals unbound-variable
        (with-call/cc ,body-without-stop (lookup-special-in-lisp)))
      (signals unbound-variable
        (with-call/cc ,body-without-stop (lookup-special-in-defun/cc nil)))
      ;; now stop once
      (is (= 1 (kall (with-call/cc ,body-with-stop))))
      (signals unbound-variable
        (kall (with-call/cc ,body-with-stop (lookup-special-in-lisp))))
      (signals unbound-variable
        (kall (with-call/cc ,body-with-stop (lookup-special-in-defun/cc nil)))))))

;; export and lookup in the same lexical environment
(test-special define-and-lookup-special-in-defun/cc)

;; export and lookup in cc code
(test-special export-special-from-let/cc-and-lookup-in-defun/cc)
(test-special export-special-from-let/cc-and-lookup-in-let/cc)
(test-special export-special-from-let/cc-and-lookup-in-let*/cc)
(test-special export-special-from-let*/cc-and-lookup-in-defun/cc)
(test-special export-special-from-let*/cc-and-lookup-in-let/cc)
(test-special export-special-from-let*/cc-and-lookup-in-let*/cc)

;; export from cc code and lookup in lisp code
(test-special export-special-from-let/cc-and-lookup-in-lisp)
(test-special export-special-from-let*/cc-and-lookup-in-lisp)

;; export from lisp code and lookup in cc code
(test-special export-special-from-lisp-and-lookup-in-defun/cc)
(test-special export-special-from-lisp-and-lookup-in-let/cc)
(test-special export-special-from-lisp-and-lookup-in-let*/cc)

(defparameter *special-variable* 42)

(deftest test/special-variables/export-special-from-lisp-and-lookup-in-lisp ()
  (is (= *special-variable* 42))
  ;; export in lisp code, let it go through some cc code and lookup in lisp code after continuing
  (is (= 2
         (let ((*special-variable* 1))
           (kall
            (let ((*special-variable* -1000))
              (multiple-value-prog1
                  (with-call/cc
                    (let/cc k k)
                    (incf *special-variable*)
                    (lookup-special-in-lisp '*special-variable*))
                (is (= -1000 *special-variable*)))))))))

(deftest test/special-variables/special-lisp-var-rebound-in/cc ()
  (is (= 42
         (with-call/cc
           *special-variable*)))
  (is (= 43
         (with-call/cc
           (let ((*special-variable* 43))
             *special-variable*))))
  (is (= 43
         (with-call/cc
           (let ((*special-variable* 43))
             (lookup-special-in-lisp '*special-variable*))))))
