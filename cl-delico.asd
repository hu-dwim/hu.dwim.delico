;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-syntax-sugar)

  (unless (find-package '#:cl-delico-system)
    (defpackage #:cl-delico-system
      (:use :common-lisp :asdf :cl-syntax-sugar)
      (:export
       #:*load-as-production-p*))))

(in-package #:cl-delico-system)

(defvar *load-as-production-p* t)

(defsystem :cl-delico
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "cl-delico::setup-readtable"
  :depends-on (:alexandria
               :metabang-bind
               :cl-def
               :cl-walker
               :cl-syntax-sugar
               :closer-mop
               )
  :components ((:static-file "cl-delico.asd")
               (:module "src"
                :components ((:file "package")
                             (:file "configuration" :depends-on ("package"))
                             (:file "duplicates" :depends-on ("configuration" "package"))
                             (:file "infrastructure" :depends-on ("configuration" "package" "duplicates"))
                             (:module "interpreted"
                                      :depends-on ("package" "duplicates" "infrastructure")
                                      :serial t
                                      :components ((:file "interpreter")
                                                   (:file "handlers")
                                                   (:file "apply")
                                                   (:file "generic-functions")
                                                   (:file "common-lisp-cc")))))))

(defclass delico-test-system (system-with-readtable)
  ())

(defsystem :cl-delico-test
  :class delico-test-system
  :default-component-class cl-source-file-with-readtable
  :setup-readtable-function "cl-delico::setup-readtable"
  :depends-on (:cl-delico ; and all its dependencies
               :stefil
               )
  :components ((:module "tests"
                        :components ((:file "package")
                                     (:file "interpreted" :depends-on ("package"))
                                     (:file "special-variables" :depends-on ("package"))))))

(defmethod asdf:perform :around ((op operation) (system delico-test-system))
  (progv
      (list (read-from-string "cl-delico:*call/cc-returns*"))
      (list nil)
    (call-next-method)))

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
