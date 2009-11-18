;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def package :hu.dwim.delico.test
  (:use :hu.dwim.common
        :hu.dwim.delico
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.walker)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.delico)))
