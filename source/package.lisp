;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.delico
  (:use :contextl
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.walker)
  (:export #:to-cps
           #:with-call/cc
           #:kall
           #:call/cc
           #:let/cc
           #:*call/cc-returns*
           #:invalid-return-from
           #:unreachable-code
           #:defun/cc
           #:defgeneric/cc
           #:defmethod/cc
           #:fmakun-cc
           #:*debug-evaluate/cc*
           #:*trace-cc*)
  (:readtable-setup (hu.dwim.util:enable-standard-hu.dwim-syntaxes)))
