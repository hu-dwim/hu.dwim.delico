;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.delico.documentation
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.documentation-system"
  :depends-on (:hu.dwim.delico.test
               :hu.dwim.walker.documentation
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "delico" :depends-on ("package"))
                             (:file "package")))))
