;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.delico)

(defmacro if-bind (var test &body then/else)
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  `(when-bind it ,test ,@body))

(defmacro prog1-bind (var ret &body body)
  `(let ((,var ,ret))
    ,@body
    ,var))

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))

;; from arnesi
(defmacro dolist* ((iterator list &optional return-value) &body body)
  "Like DOLIST but destructuring-binds the elements of LIST.

If ITERATOR is a symbol then dolist* is just like dolist EXCEPT
that it creates a fresh binding."
  (if (listp iterator)
      (let ((i (gensym "DOLIST*-I-")))
        `(dolist (,i ,list ,return-value)
           (destructuring-bind ,iterator ,i
             ,@body)))
      `(dolist (,iterator ,list ,return-value)
         (let ((,iterator ,iterator))
           ,@body))))



;; from arnesi
;; TODO delme, use the stefil lambda walker
(defun extract-argument-names (lambda-list &key allow-specializers)
  "Returns a list of symbols representing the names of the
  variables bound by the lambda list LAMBDA-LIST."
  (mapcan (lambda (argument)
            (let ((vars '()))
              (dolist (slot-name '(hu.dwim.walker::name hu.dwim.walker::supplied-p-parameter))
                (awhen (and (slot-exists-p argument slot-name)
                            (slot-boundp   argument slot-name)
                            (slot-value    argument slot-name))
                  (push it vars)))
              (nreverse vars)))
          (walk-lambda-list lambda-list nil (make-walk-environment) :allow-specializers allow-specializers)))

(defun convert-to-generic-lambda-list (defmethod-lambda-list)
  (loop
     with generic-lambda-list = '()
     for arg in (walk-lambda-list defmethod-lambda-list
                                  nil (make-walk-environment)
                                  :allow-specializers t)
     do (etypecase arg
          ((or required-function-argument-form
               specialized-function-argument-form)
           (push (name-of arg) generic-lambda-list))
          (keyword-function-argument-form
           (pushnew '&key generic-lambda-list)
           (aif (keyword-name-of arg)
                (push (list (list it (name-of arg)))
                      generic-lambda-list)
                (push (list (name-of arg)) generic-lambda-list)))
          (rest-function-argument-form
           (push '&rest generic-lambda-list)
           (push (name-of arg) generic-lambda-list))
          (optional-function-argument-form
           (pushnew '&optional generic-lambda-list)
           (push (name-of arg) generic-lambda-list))
          (allow-other-keys-function-argument-form
           (unless (member '&key generic-lambda-list)
             (push '&key generic-lambda-list))
           (push '&allow-other-keys generic-lambda-list)))
     finally (return (nreverse generic-lambda-list))))

(defun clean-argument-list (lambda-list)
  (loop
     for head on lambda-list
     for argument = (car head)
     if (member argument '(&optional &key &rest &allow-other-keys))
       return (append cleaned head)
     else
       collect (if (listp argument)
                   (first argument)
                   argument)
       into cleaned
     finally (return cleaned)))
