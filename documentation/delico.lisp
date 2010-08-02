;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.delico.documentation)

(def project :hu.dwim.delico)

(def method make-project-tab-pages ((component project/detail/inspector) (project (eql (find-project :hu.dwim.delico))))
  (append (list (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "User guide"))
                  (make-value-inspector (find-book 'user-guide)))
                (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Dictionary"))
                  (make-value-inspector (mapcar 'find-dictionary '(continuation)))))
          (call-next-method)))

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "This is a library to do " (hyperlink/wikipedia "Continuation passing style" "CPS transformation") " on a delimited area of Common Lisp code."))
  (chapter (:title "History")
    (paragraph ()
      "The interpreted serializable continuation part of the codebase is based on code that was originally written by Marco Baringer in his Arnesi library. With his permission it was factored out into this standalone library and further developed.")
    (paragraph ()
      "Contains important contribution by several people, please consult the history of the source repository for the details."))
  (chapter (:title "Status")
    (paragraph ()
      "We have two backends planned: a closure based one, and an interpreter based one. The former is not even started, but the latter is rather complete passing numerous tests and provides serializable continuations. This code is used in production."))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "Works on more or less all Common Lisp implementations " (find-project :hu.dwim.walker) " works on.")))

(def dictionary continuation ()
  with-call/cc
  kall
  continuation?)
