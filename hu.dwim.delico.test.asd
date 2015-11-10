;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.delico.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.def
               :hu.dwim.delico
               :hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.util
               :hu.dwim.util/temporary-files)
  :components ((:module "test"
                :components ((:file "interpreted" :depends-on ("suite"))
                             (:file "package")
                             (:file "special-variables" :depends-on ("suite"))
                             (:file "suite" :depends-on ("package"))))))
