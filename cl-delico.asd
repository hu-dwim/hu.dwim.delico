;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:cl-delico.system)
    (defpackage #:cl-delico.system
      (:use :common-lisp :asdf))))

(in-package #:cl-delico.system)

(defsystem :cl-delico
  :depends-on (:alexandria)
  :components ((:static-file "cl-delico.asd")
               (:module "src"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:module "interpreted"
                                      :depends-on ("package" "duplicates")
                                      :serial t
                                      :components ((:file "interpreter")
                                                   (:file "handlers")
                                                   (:file "apply")
                                                   (:file "generic-functions")
                                                   (:file "common-lisp-cc")))))))

(defsystem :cl-delico-test
  :depends-on (:cl-delico :stefil :alexandria :metabang-bind)
  :components ((:module "tests"
                        :components ((:file "package")
                                     (:file "interpreted" :depends-on ("package"))))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :cl-delico))))
  (asdf:oos 'asdf:load-op :cl-delico-test)
  (in-package :cl-delico-test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-delico))))
  nil)
