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
  ;; TODO: close enough, eh?
  (bind ((delico-package (find-package :hu.dwim.delico)))
    (or (and (symbolp k)
             (eq delico-package (symbol-package k)))
        (and (consp k)
             (symbolp (car k))
             (eq delico-package (symbol-package (car k)))))))

(def (type e) continuation ()
  '(satisfies continuationp))
