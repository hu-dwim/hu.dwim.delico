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
