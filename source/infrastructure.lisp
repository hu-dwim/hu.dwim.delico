;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.delico)

(def layer delico ()
  ())

(def special-variable *cc-functions* (make-hash-table :test 'eq))

(def (function e) fmkunbound/cc (function-name)
  (remhash function-name *cc-functions*))

(def (function e) fdefinition/cc (function-name)
  (values-list (gethash function-name *cc-functions*)))

(def (function e) (setf fdefinition/cc) (closure-object function-name &optional (type 'defun/cc))
  (setf (gethash function-name *cc-functions*) (list closure-object type)))

(def layered-method hu.dwim.walker::handle-undefined-reference :in delico (type name)
     (unless (member name '(call/cc))
       (call-next-layered-method)))

(def layered-method hu.dwim.walker::function-name? :in delico :around (name)
  (or (call-next-layered-method)
      (gethash name *cc-functions*)))

(def (function e) continuationp (k)
  (and (consp k)
       (eql (car k) 'k-for-evaluate-progn/cc)))
