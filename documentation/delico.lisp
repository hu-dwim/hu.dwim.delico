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
      "This is a portable library to do " (hyperlink/wikipedia "Continuation passing style" "CPS transformation") " on a delimited area of Common Lisp code at macroexpand time.")
    (paragraph ()
      "For further understanding you may refer to the wikipedia page on " (hyperlink/wikipedia "Delimited_continuation" "delimited continuations") ", but keep in mind that Delico provides a somewhat different API than the shift/reset operator proposed there."))
  (chapter (:title "History")
    (paragraph ()
      "The interpreted serializable continuation part of the codebase is based on code that was originally written by Marco Baringer in his Arnesi library. With his permission it was factored out into this standalone library and further developed.")
    (paragraph ()
      "Contains important contributions by several people, please consult the history of the source repository for the details."))
  (chapter (:title "Status")
    (paragraph ()
      "We have two backends planned: a closure based one, and an interpreter based one. The former is not even started, but the latter is rather complete passing numerous tests and provides serializable continuations. This code is used in production."))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "Works on more or less all Common Lisp implementations " (find-project :hu.dwim.walker) " works on."))
  (chapter (:title "Examples")
    (chapter (:title "Serializing continuations")
      ;; TODO add code formatting primitive and use it here
      (paragraph ()
        "CL-USER> (load-system :hu.dwim.serializer)
T
CL-USER> (load-system :hu.dwim.delico)
T
CL-USER> (in-package :hu.dwim.delico)
#<PACKAGE \"HU.DWIM.DELICO\">
DELICO> (defparameter *k* (with-call/cc
                            (let ((a (let/cc k k)))
                              (+ a 4))))
*K*
DELICO> (defparameter *ser* (hu.dwim.serializer:serialize *k*))
*SER*
DELICO> (hu.dwim.serializer:deserialize *ser*)
(K-FOR-EVALUATE-LET/CC A NIL NIL
                       (#<FREE-APPLICATION-FORM !(+ #<WALKED-LEXICAL-VARIABLE-REFERENCE-FORM !A {100A4FEB53}> #<CONSTANT-FORM !4 {100A4FEC43}>) {100A4FBDF3}>)
                       NIL NIL (TOPLEVEL-K))
DELICO> (kall (hu.dwim.serializer:deserialize *ser*) 42)
46
DELICO>")
      (paragraph ()
        "You can find an example of how to cut out the continuation from the rest of the heap and how to reconnect in hu.dwim.perec:")
      (system-relative-pathname :hu.dwim.perec "source/persistence/export.lisp"))))

(def dictionary continuation ()
  with-call/cc
  kall
  continuation?)
