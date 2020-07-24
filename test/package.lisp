;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.delico/test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.delico
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.walker)
  (:readtable-setup
   (setup-readtable/same-as-package :hu.dwim.delico)
   (hu.dwim.syntax-sugar:enable-string-quote-syntax)))
