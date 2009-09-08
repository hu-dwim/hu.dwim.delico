;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.delico
  (:use :hu.dwim.asdf
        :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.syntax-sugar
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

  (:shadow #:walk-form
           #:undefined-reference-handler))
