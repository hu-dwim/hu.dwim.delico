;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :cl-user)

(defpackage :cl-delico
  (:use #:common-lisp :cl-walker :alexandria :metabang-bind :cl-def :cl-syntax-sugar)
  (:export

   #:to-cps
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
   #:*trace-cc*

   )

  (:shadow
   #:walk-form
   #:undefined-reference-handler
   ))
