;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-delico)

(defun undefined-reference-handler (type name)
  (unless (member name '(call/cc))
    (cl-walker::undefined-reference-handler type name)))

(defun walk-form (form &optional parent (env (make-walk-environment)))
  (with-walker-configuration (:undefined-reference-handler 'undefined-reference-handler)
    (cl-walker:walk-form form parent env)))
