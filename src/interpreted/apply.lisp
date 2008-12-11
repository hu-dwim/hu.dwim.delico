;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-delico)

(defmethod evaluate/cc ((node free-function-object-form) lex-env dyn-env k)
  (declare (ignore lex-env dyn-env))
  (bind ((name (name-of node))
         ((:values definition cc-boundp) (fdefinition/cc name)))
    (if cc-boundp
        (kontinue k definition)
        (if (fboundp name)
            (kontinue k (fdefinition name))
            (error "Unbound function ~S." name)))))

(defmethod evaluate/cc ((node walked-lexical-function-object-form) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (kontinue k (lookup lex-env :flet (name-of node) :error-p t)))

(defclass closure/cc ()
  ((code :accessor code-of :initarg :code)
   (env :accessor environment-of :initarg :environment))
  (:metaclass closer-mop:funcallable-standard-class))

(def constructor closure/cc
  (closer-mop:set-funcallable-instance-function
    -self-
    (lambda (&rest args)
      (drive-interpreter/cc
       (apply-lambda/cc -self-
                   args
                   '()
                   +toplevel-k+)))))

(def (function i) make-closure/cc (code &optional (environment (list)))
  (make-instance 'closure/cc :code code :environment environment))

;;;; LAMBDA

(defmethod evaluate/cc ((lambda lambda-function-form) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (kontinue k (make-closure/cc lambda lex-env)))

;;;; APPLY and FUNCALL

(defk k-for-call/cc (k)
    (value other-values)
  (if *call/cc-returns*
      (kontinue k value other-values)
      (throw 'done (values-list (cons value other-values)) )))

;;;; apply'ing a free (global) function

(defmethod evaluate/cc ((node free-application-form) lex-env dyn-env k)
  (bind ((operator (operator-of node))
         (arguments (arguments-of node)))
    (cond
      ((eql 'call/cc operator)
       (evaluate/cc (make-instance 'free-application-form
                                   :operator 'funcall
                                   :arguments (list (first arguments)
                                                    (make-instance 'constant-form :value k :source k))
                                   :source (source-of node))
                    lex-env dyn-env `(k-for-call/cc ,k)))

      ((eql 'kall operator)
       (evaluate-arguments-then-apply (lambda (args)
                                        (trace-statement "KALL'ing ~S on ~S" (first args) (rest args))
                                        (apply #'kontinue (first args) (cdr args)))
                                      arguments '()
                                      lex-env dyn-env))

      ((and (eql 'call-next-method operator)
            (nth-value 1 (lookup lex-env :next-method t)))
       (aif (lookup lex-env :next-method t)
            (evaluate-arguments-then-apply (lambda (args)
                                             (apply-lambda/cc it args dyn-env k))
                                           arguments '() lex-env dyn-env)
            (error "no next method")))

      ((and (eql 'next-method-p operator)
            (nth-value 1 (lookup lex-env :next-method t)))
       (kontinue k (lookup lex-env :next-method t)))

      ((eql 'funcall operator)
       (evaluate-funcall/cc arguments lex-env dyn-env k))

      ((eql 'apply operator)
       (evaluate-apply/cc arguments '() lex-env dyn-env k))

      ((and (symbolp operator)
            (eql 'defun/cc (nth-value 1 (fdefinition/cc operator))))
       (evaluate-arguments-then-apply (lambda (args)
                                        (trace-statement "Calling cc function ~S with arguments ~S" operator args)
                                        (apply-lambda/cc (fdefinition/cc operator) args dyn-env k))
                                      arguments '()
                                      lex-env dyn-env))

      ((and (symbolp operator)
            (eql 'defmethod/cc (nth-value 1 (fdefinition/cc operator))))
       (evaluate-arguments-then-apply (lambda (args)
                                        (trace-statement "Calling cc method ~S with arguments ~S" operator args)
                                        (apply-lambda/cc (apply operator args) args dyn-env k))
                                      arguments '()
                                      lex-env dyn-env))

      (t
       (evaluate-arguments-then-apply (lambda (args)
                                        (bind (((:values vars vals) (export-specials dyn-env)))
                                          (progv vars vals
                                            (trace-statement "Calling function ~S with arguments ~S" operator args)
                                            (apply #'kontinue k (multiple-value-list
                                                                 (apply (fdefinition operator) args))))))
                                      arguments '()
                                      lex-env dyn-env)))))

;; returns a list of variables and values from the dynamic environment that should be exported
;; these variables will be visible in normal lisp code that is called from cc code
(defun export-specials (dyn-env)
  ;; TODO: here we could check each special whether it has to be exported or not
  ;;       this could be based on something like (declare (export var)) in the cc code
  (let ((dyn-env (remove-duplicates dyn-env
                                    :test (lambda (x y) (eq (second x) (second y)))
                                    :from-end t)))
    (values (mapcar 'second dyn-env)
            (mapcar 'cddr dyn-env))))

(defmethod evaluate/cc ((node walked-lexical-application-form) lex-env dyn-env k)
  (evaluate-arguments-then-apply
   (lambda (args)
     (apply-lambda/cc (lookup lex-env :flet (operator-of node) :error-p t) args dyn-env k))
   (arguments-of node) '()
   lex-env dyn-env))

(defmethod evaluate/cc ((node unwalked-lexical-application-form) lex-env dyn-env k)
  (error "Calling an unwalked lexical function is not yet supported by the call/cc interpreter"))

;;;; apply'ing a lambda

(defmethod evaluate/cc ((node lambda-application-form) lex-env dyn-env k)
  (evaluate-funcall/cc (cons (operator-of node) (arguments-of node)) lex-env dyn-env k))

;;;; Utility methods which do the actual argument evaluation, parsing
;;;; and control transfer.

(defun evaluate-funcall/cc (arguments lex-env dyn-env k)
  (evaluate-apply/cc (append (butlast arguments)
                             (list (make-instance 'free-application-form
                                                  :operator 'list
                                                  :source `(list ,(source-of (car (last arguments))))
                                                  :arguments (last arguments))))
                     '()
                     lex-env dyn-env k))

(defk k-for-apply/cc (remaining-arguments evaluated-arguments lex-env dyn-env k)
    (value)
  (evaluate-apply/cc (cdr remaining-arguments) (cons value evaluated-arguments)
                     lex-env dyn-env k))

(defun evaluate-apply/cc (remaining-arguments evaluated-arguments lex-env dyn-env k)
  (if remaining-arguments
      (evaluate/cc (car remaining-arguments) lex-env dyn-env
                   `(k-for-apply/cc ,remaining-arguments ,evaluated-arguments ,lex-env ,dyn-env ,k))
      (let ((arg-list (apply #'list* (reverse evaluated-arguments))))
        (apply-lambda/cc (first arg-list) (rest arg-list) dyn-env k))))

;;;; Finally this is the function which, given a closure/cc object and
;;;; a list of (evaluated) arguments parses them, setup the
;;;; environment and transfers control.

(defgeneric apply-lambda/cc (operator effective-arguments dyn-env k))

(defmethod apply-lambda/cc ((operator closure/cc) effective-arguments dyn-env k)
  (trace-statement "Applying cc closure ~S to ~S" (source-of (code-of operator)) effective-arguments)
  (bind ((lex-env (environment-of operator))
         (remaining-arguments effective-arguments)
         (remaining-parameters (arguments-of (code-of operator))))
    ;; in this code ARGUMENT refers to the values passed to the
    ;; function. PARAMETER refers to the lambda of the closure
    ;; object. we walk down the parameters and put the arguments in
    ;; the environment under the proper names.

    ;; first the required arguments
    (loop
       while remaining-parameters
       for parameter = (first remaining-parameters)
       do (typecase parameter
            (required-function-argument-form
             (if remaining-arguments
                 (setf lex-env (register lex-env :let (name-of parameter) (pop remaining-arguments)))
                 (error "Missing required arguments, expected ~S, got ~S."
                        (arguments-of (code-of operator)) effective-arguments))
             (pop remaining-parameters))
            (t (return))))

    ;; handle special variables
    (setf dyn-env (import-specials (code-of operator) dyn-env))

    ;; now we start the chain optional->keyword->evaluate-body. We do
    ;; this because optional and keyword parameters may have default
    ;; values which may use call/cc.
    (apply-lambda/cc/optional operator
                              remaining-parameters remaining-arguments
                              lex-env dyn-env k)))

(defun apply-lambda/cc/optional (operator remaining-parameters remaining-arguments lex-env dyn-env k)
  (flet ((done (remaining-parameters)
           (return-from apply-lambda/cc/optional
             (apply-lambda/cc/keyword
              operator remaining-parameters remaining-arguments lex-env dyn-env k))))
    (loop
       for head on remaining-parameters
       for parameter = (first head)
       do
       (etypecase parameter
         (rest-function-argument-form
          (setf lex-env (register lex-env :let (name-of parameter) remaining-arguments)))
         (optional-function-argument-form
          (if remaining-arguments
              (progn
                (setf lex-env (register lex-env :let (name-of parameter) (pop remaining-arguments)))
                (when (supplied-p-parameter parameter)
                  (setf lex-env (register lex-env :let (supplied-p-parameter parameter) t))))
              (return-from apply-lambda/cc/optional
                ;; we need to evaluate a default-value, since this may
                ;; contain call/cc we need to setup the continuation
                ;; and let things go from there (hence the return-from)
                (evaluate/cc (default-value-of parameter) lex-env dyn-env
                             `(k-for-apply/cc/optional-argument-default-value
                               ;; remaining-arguments is, by
                               ;; definition, NIL so we needn't pass
                               ;; it here.
                               ,operator ,head ,lex-env ,dyn-env ,k)))))
         ((or keyword-function-argument-form allow-other-keys-function-argument-form)
          ;; done with the optional args
          (done head)))
       finally (done head))))

(defk k-for-apply/cc/optional-argument-default-value
    (operator remaining-parameters lex-env dyn-env k)
    (value)
  (apply-lambda/cc/optional
   operator (cdr remaining-parameters)
   ;; nb: if we're evaluating the default value of an optional
   ;; arguments then we can't have anything left in the arguments
   ;; list.
   nil
   (register lex-env :let (name-of (first remaining-parameters)) value)
   dyn-env
   k))

(defun apply-lambda/cc/keyword (operator remaining-parameters remaining-arguments lex-env dyn-env k)
  ;; now any keyword parameters
  (loop
     for head on remaining-parameters
     for parameter = (first head)
     do (typecase parameter
          (keyword-function-argument-form
           (assert (evenp (length remaining-arguments))
                   (remaining-arguments)
                   "Odd number of arguments in ~S being applied to ~S."
                   remaining-arguments
                   (source-of (code-of operator)))
           (bind ((keyword-name (effective-keyword-name-of parameter))
                  (value (getf remaining-arguments keyword-name parameter)))
             (if (eql parameter value)
                 ;; no such keyword. need to evaluate the default value
                 (return-from apply-lambda/cc/keyword
                   (evaluate/cc (default-value-of parameter) lex-env dyn-env
                                `(k-for-apply-lambda/cc/keyword-default-value
                                  ,operator ,head ,remaining-arguments
                                  ,lex-env ,dyn-env ,k)))
                 ;; keyword passed in explicitly.
                 (progn
                   (let ((value (getf remaining-arguments keyword-name)))
                     (remf remaining-arguments keyword-name)
                     (setf lex-env (register lex-env :let (name-of parameter) value))
                   (awhen (supplied-p-parameter parameter)
                     (setf lex-env (register lex-env :let it t))))))))
          (allow-other-keys-function-argument-form
           (when (cdr remaining-parameters)
             (error "Bad lambda list: ~S" (arguments-of (code-of operator))))
           (return))
          (t (unless (null remaining-parameters)
               (error "Bad lambda list: ~S" (arguments-of (code-of operator)))))))
  (evaluate-progn/cc (body-of (code-of operator)) lex-env dyn-env k))

(defk k-for-apply-lambda/cc/keyword-default-value
    (operator remaining-parameters remaining-arguments lex-env dyn-env k)
    (value)
  (apply-lambda/cc/keyword operator
                           (cdr remaining-parameters) remaining-arguments
                           (register lex-env :let (name-of (first remaining-parameters)) value)
                           dyn-env
                           k))

(defmethod apply-lambda/cc ((operator function) effective-arguments dyn-env k)
  "Method used when we're applying a regular, non cc, function object."
  (declare (ignore dyn-env))
  (trace-statement "Applying function ~S to ~S" operator effective-arguments)
  (apply #'kontinue k (multiple-value-list (apply operator effective-arguments))))

(defmethod apply-lambda/cc ((operator symbol) effective-arguments dyn-env k)
  "Method used when we're applying a regular, non cc, function object."
  (apply-lambda/cc (symbol-function operator) effective-arguments dyn-env k))

;;;; Small helper function

(defk k-for-evaluate-arguments-then-apply (handler remaining-arguments evaluated-arguments lex-env dyn-env)
    (value)
  (evaluate-arguments-then-apply
   handler
   remaining-arguments (cons value evaluated-arguments)
   lex-env dyn-env))

(defun evaluate-arguments-then-apply (handler remaining-arguments evaluated-arguments lex-env dyn-env)
  (if remaining-arguments
      (evaluate/cc (car remaining-arguments) lex-env dyn-env
                    `(k-for-evaluate-arguments-then-apply ,handler ,(cdr remaining-arguments)
                                                          ,evaluated-arguments ,lex-env ,dyn-env))
      (funcall handler (reverse evaluated-arguments))))
