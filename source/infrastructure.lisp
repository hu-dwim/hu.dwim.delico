;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.delico)

(defun undefined-reference-handler (type name)
  (unless (member name '(call/cc))
    (hu.dwim.walker::undefined-reference-handler type name)))

(defun walk-form (form &optional parent (env (make-walk-environment)))
  (with-walker-configuration (:undefined-reference-handler 'undefined-reference-handler
                              :function-name? 'function-name?)
    (hu.dwim.walker:walk-form form parent env)))

(def (function e) continuationp (k)
  (and (consp k)
       (eql (car k) 'k-for-evaluate-progn/cc)))
