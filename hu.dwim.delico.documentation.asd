;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.delico.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.delico.test
               :hu.dwim.walker.documentation
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "delico" :depends-on ("package"))
                             (:file "package")))))
