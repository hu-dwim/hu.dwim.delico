;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.delico
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :description "Delimited, interpreted shift-reset continuation."
  :depends-on (:contextl
               :closer-mop
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
                                                   (:file "common-lisp-cc" :depends-on ("interpreter"))))))))
