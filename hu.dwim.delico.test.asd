;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem ::hu.dwim.delico.test
  :class hu.dwim.test-system
  :depends-on (:hu.dwim.delico
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components ((:file "interpreted" :depends-on ("suite"))
                             (:file "package")
                             (:file "special-variables" :depends-on ("suite"))
                             (:file "suite" :depends-on ("package"))))))
