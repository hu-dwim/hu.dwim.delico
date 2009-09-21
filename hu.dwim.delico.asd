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
  :licence "BSD / Public domain"
  :description "Delimited continuation"
  :depends-on (:hu.dwim.common-lisp
               :hu.dwim.def
               :hu.dwim.syntax-sugar+swank
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "configuration" :depends-on ("package"))
                             (:file "duplicates" :depends-on ("configuration" "package"))
                             (:file "infrastructure" :depends-on ("configuration" "package" "duplicates"))
                             (:module "interpreter"
                                      :depends-on ("package" "duplicates" "infrastructure")
                                      :components ((:file "interpreter")
                                                   (:file "handler" :depends-on ("interpreter"))
                                                   (:file "apply" :depends-on ("interpreter"))
                                                   (:file "generic-function" :depends-on ("interpreter"))
                                                   (:file "common-lisp-cc" :depends-on ("interpreter"))))))))
