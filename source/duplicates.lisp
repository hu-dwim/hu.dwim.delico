;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.delico)

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

(defun convert-to-generic-lambda-list (defmethod-lambda-list)
  (bind (((:values requireds optionals rest keywords allow-other-keys?)
          (parse-ordinary-lambda-list defmethod-lambda-list :allow-specializers t)))
    (append
     (mapcar (lambda (required)
               (if (consp required)
                   (first required)
                   required))
             requireds)
     (awhen (mapcar #'first optionals)
       `(&optional ,@it))
     (when rest
       `(&rest ,rest))
     (awhen (mapcar (lambda (keyword)
                      (list (first keyword)))
                    keywords)
       `(&key ,@it))
     (when allow-other-keys?
       `(&allow-other-keys)))))

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
