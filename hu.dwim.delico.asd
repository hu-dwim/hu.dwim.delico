;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.delico
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :description "Delimited, interpreted shift-reset continuation."
  :depends-on (:contextl
               :hu.dwim.def+hu.dwim.common
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "infrastructure" :depends-on ("package" "duplicates"))
                             (:module "interpreter"
                                      :depends-on ("package" "duplicates" "infrastructure")
                                      :components ((:file "interpreter")
                                                   (:file "handler" :depends-on ("interpreter"))
                                                   (:file "apply" :depends-on ("interpreter"))
                                                   (:file "generic-function" :depends-on ("interpreter"))
                                                   (:file "common-lisp-cc" :depends-on ("apply" "generic-function"))))))))

(defsystem :hu.dwim.delico/test
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

(defsystem :hu.dwim.delico/documentation
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.documentation-system"
  :depends-on (:hu.dwim.delico/test
               :hu.dwim.walker/documentation
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "delico" :depends-on ("package"))
                             (:file "package")))))
