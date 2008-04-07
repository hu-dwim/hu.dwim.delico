;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-delico)

;;;; ** Functions, Generic Functions, Methods and standard-combination

;;;; DEFUN/CC

(defmacro defun/cc (name arguments &body body)
  `(progn
     (setf (fdefinition/cc ',name 'defun/cc)
           (make-instance 'closure/cc
                          :code (walk-form '(lambda ,arguments
                                             (block ,name
                                               (locally ,@body))))
                          :env nil))
     #+nil ;; TODO delme probably
     (defun ,name ,arguments
       (declare (ignore ,@(extract-argument-names arguments)))
       (error "Sorry, /CC function are not callable outside of with-call/cc."))
     ))

;;;; DEFGENERIC/CC

(defmacro defgeneric/cc (name args &rest options)
  "Trivial wrapper around defgeneric designed to alert readers that these methods are cc methods."
  (assert (not (find :method options :key #'first)) () "TODO: defgeneric/cc does not walk the :method entries yet, use standalone defmethod/cc's for now")
  `(progn
     (defgeneric ,name ,args
       ,@options
       (:method-combination cc-standard))
     (setf (fdefinition/cc ',name 'defmethod/cc) t)))

;;;; DEFMETHOD/CC

; for emacs:  (setf (get 'defmethod/cc 'common-lisp-indent-function) 'lisp-indent-defmethod)

(defmacro defmethod/cc (name &rest args)
  (let ((qlist (when (and (symbolp (car args))
                          (not (null (car args))))
                 (list (pop args)))))
    (let ((arguments (car args))
	  (body (cdr args)))
      `(progn
	 (unless (eq 'defmethod/cc (second (multiple-value-list (fdefinition/cc ',name))))
           (setf (fdefinition/cc ',name 'defmethod/cc) t)
           (defgeneric/cc ,name ,(if arguments
                                     (convert-to-generic-lambda-list arguments)
                                     '())))
	 (defmethod ,name ,@qlist ,arguments
           ;; the walked code will not reference the arguments because this defmethod will be used
           ;; as a colsure/cc factory, so make them all ignored.
           ,(when arguments
	     `(declare (ignore ,@(extract-argument-names arguments :allow-specializers t))))
           ;; TODO use parse-body
	   ,@(when (stringp (first body))
              (list (pop body)))
	   (make-instance 'closure/cc
                          ;; TODO why is it walking at runtime?
                          ;; TODO lexenv is ignored
			  :code (walk-form '(lambda ,(clean-argument-list arguments)
                                              (block ,name
                                                (locally ,@body)))
                                           nil (make-walkenv))
			  :env nil))))))

;;;; CC-STANDARD (standard-combination for cc methods)

(defun closure-with-nextmethod (closure next)
  (make-instance 'closure/cc
		 :code (code closure)
		 :env (register (env closure) :next-method t next)))

(defun closure-with-befores (closure befores)
  (make-instance 'closure/cc
		 :code (walk-form `(lambda (&rest args)
				     ,@(loop
					  for before in befores
					  collect `(apply ,before args))
				     (apply ,closure args)))
		 :env nil))

(defun closure-with-afters (closure afters)
  (make-instance 'closure/cc
		 :code (walk-form `(lambda (&rest args)
				     (prog1
					 (apply ,closure args)
				       ,@(loop
					    for after in afters
					    collect `(apply ,after args)))))
		 :env nil))

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
