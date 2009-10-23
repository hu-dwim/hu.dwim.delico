;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.delico)

(def constant +defun-stub-error-message+ "This is only a compile-time stub, normally you shouldn't be able to call it")

(defmacro defun/cc (name arguments &body body)
  `(progn
     (eval-when (:compile-toplevel)
       #+sbcl(sb-c:%compiler-defun ',name nil t))
     (eval-when (:load-toplevel :execute)
       (setf (fdefinition/cc ',name 'defun/cc)
             (make-closure/cc (walk-form '(lambda ,arguments
                                           (block ,name
                                             (locally ,@body)))))))))

(defmacro defgeneric/cc (name args &rest options)
  "Trivial wrapper around defgeneric designed to alert readers that these methods are cc methods."
  (assert (not (find :method options :key #'first)) () "FIXME: defgeneric/cc does not walk the :method entries yet, use standalone defmethod/cc's for now")
  `(progn
     (defgeneric ,name ,args
       ,@options
       (:method-combination cc-standard))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (fdefinition/cc ',name 'defmethod/cc) t))))

; for emacs:  (setf (get 'defmethod/cc 'common-lisp-indent-function) 'lisp-indent-defmethod)

(defmacro defmethod/cc (&whole whole &environment lexenv name &rest args)
  (let ((qualifiers (list (if (and (symbolp (first args))
                                   (not (null (first args))))
                              (pop args)
                              :primary))))
    (bind ((arguments (car args))
           (body (cdr args))
           ((:values body declarations documentation) (parse-body body :documentation #t :whole whole)))
      `(progn
         (unless (eq 'defmethod/cc (nth-value 1 (fdefinition/cc ',name)))
           (setf (fdefinition/cc ',name 'defmethod/cc) t)
           (defgeneric/cc ,name ,(if arguments
                                     (convert-to-generic-lambda-list arguments)
                                     '())))
         (defmethod ,name ,@qualifiers ,arguments
           ,@(when documentation
               (list documentation))
           ;; the walked code will not reference the arguments because this defmethod will be used
           ;; as a colsure/cc factory, so make them all ignored.
           ,(when arguments
             `(declare (ignore ,@(extract-argument-names arguments :allow-specializers t))))
           (make-closure/cc
            ;; TODO make sure that the compile-time walked forms are not modified anywhere
            ,(walk-form `(lambda ,(clean-argument-list arguments)
                           (block ,name
                             (locally
                                 ,@declarations
                               ,@body)))
                        nil (make-walk-environment lexenv))))))))

;;;; CC-STANDARD (standard-combination for cc methods)

(defun closure-with-nextmethod (closure next)
  (make-closure/cc (code-of closure) (register (environment-of closure) :next-method t next)))

(defun closure-with-befores (closure befores)
  (make-closure/cc (walk-form `(lambda (&rest args)
                                 ,@(loop
                                      :for before :in befores
                                      :collect `(apply ,before args))
                                 (apply ,closure args)))))

(defun closure-with-afters (closure afters)
  (make-closure/cc (walk-form `(lambda (&rest args)
                                 (prog1
                                     (apply ,closure args)
                                   ,@(loop
                                        :for after :in afters
                                        :collect `(apply ,after args)))))))

(define-method-combination cc-standard
    (&key (around-order :most-specific-first)
          (before-order :most-specific-first)
          (primary-order :most-specific-first)
          (after-order :most-specific-last))
  ((around (:around))
   (before (:before))
   (primary (:primary) :required t)
   (after (:after)))

  (labels ((effective-order (methods order)
             (ecase order
               (:most-specific-first methods)
               (:most-specific-last (reverse methods))))
           (primary-wrap (methods &optional nextmethod)
             (case (length methods)
               (1 `(closure-with-nextmethod
                    (call-method ,(first methods))
                    ,nextmethod))
               (t `(closure-with-nextmethod
                    (call-method ,(first methods))
                    ,(primary-wrap (cdr methods) nextmethod)))))
           (call-methods (methods)
             `(list ,@(loop
                         for m in methods
                         collect `(call-method ,m)))))
    (let* (;; reorder the methods based on the -order arguments
           (around  (effective-order around around-order))
           (before  (effective-order before before-order))
           (primary (effective-order primary primary-order))
           (after   (effective-order after after-order))
           (form    (primary-wrap primary)))
      (when after
        (setf form `(closure-with-afters ,form ,(call-methods after))))
      (when before
        (setf form `(closure-with-befores ,form ,(call-methods before))))
      (when around
        (setf form (primary-wrap around form)))
      form)))
