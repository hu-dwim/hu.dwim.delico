;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :cl-delico)

(defpackage :cl-delico-test
  (:use :common-lisp
        :cl-delico
        :alexandria
        :stefil
        :metabang-bind
        :cl-walker
        ))

(import
 '(kall)
 :cl-delico-test)

(in-package :cl-delico-test)

(in-root-suite)

(defsuite* test)
