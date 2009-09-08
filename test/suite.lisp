;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.delico.test)

(defun setup-readtable ()
  (enable-sharp-boolean-syntax))

(defsuite* (test :in root-suite))
