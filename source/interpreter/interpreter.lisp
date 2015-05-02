;;;; -*- lisp -*-

(in-package :hu.dwim.delico)

;;;; * A Common Lisp interpreter with support for continuations.

;;;; Notes:

;;;; This interpreter is dependent on the object tree built up by the
;;;; code walker in walk.lisp.

;;;; One of the, final, goals of this interpeter was to allow
;;;; continuations to be serializable. Due to this constraint we
;;;; represent continuations as regular lists which, when the cdr
;;;; (which must be clos objects or literals) is applied to the car
;;;; (which must be a symbol) the actual contiunation (a regular
;;;; common lisp function) is returned.

(def (special-variable e) *call/cc-returns* #f)

(def (function o) lookup (environment type name &key (error-p nil) (default-value nil))
  (loop
     :for (.type .name . data) :in environment
     :when (and (eq .type type) (eq .name name))
       :return (values data t)
     :finally
       (if error-p
           (error "Sorry, No value for ~S of type ~S in environment ~S found."
                  name type environment)
           (values default-value nil))))

(def (function o) (setf lookup) (value environment type name &key (error-p nil))
  (loop
     :for env-piece :in environment
     :when (and (eq (first env-piece)  type)
                (eq (second env-piece) name))
       :do (setf (cddr env-piece) value) and
       :return value
     :finally
       (when error-p
         (error "Sorry, No value for ~S of type ~S in environment ~S found."
                name type environment))))

(def (function io) register (environment type name datum &rest other-datum)
  (cons (if other-datum
            (list* type name datum other-datum)
            (list* type name datum))
        environment))

(def (macro e) with-call/cc (&environment lexenv &body body)
  "Execute BODY with delimited partial continuations.

Within the code of BODY almost all common lisp forms maintain their normal semantics. The following special forms are allowed:

\(call/cc LAMBDA) - LAMBDA, a one argument function, will be passed a continuation. This object may then be passed to the function KALL which will cause execution to resume around the call/cc form."
  (bind ((walkenv (make-walk-environment lexenv))
         (evaluate-env '())
         (walked-form (walk-form/delico `(locally ,@body) :environment walkenv)))
    (check-type walked-form locally-form)
    (dolist (reference (collect-variable-references walked-form))
      (when (typep reference 'unwalked-lexical-variable-reference-form)
        (bind ((name (name-of reference)))
          (push (list 'list
                      :lexical-let
                      `(quote ,name)
                      ;; NB: this makes the environment, and therefore
                      ;; continuations, unserializable. we would need to
                      ;; change this to a regular :let and not allow the
                      ;; setting of lexical variables.
                      ;; TODO install a configuration that requires serializable closures and issue a warning in this case here
                      `(lambda () ,name)
                      (with-unique-names (v)
                        `(lambda (,v) (setf ,name ,v))))
                evaluate-env))))
    (setf evaluate-env `(list ,@(nreverse evaluate-env)))
    `(bind ((walked-form ,walked-form))
       (drive-interpreter/cc
        (evaluate/cc walked-form
                     ,evaluate-env
                     (import-specials walked-form nil)
                     +toplevel-k+)))))

(def (function e) kall (k &optional (primary-value nil primary-value-p)
                          &rest other-values)
  "Continue the continuation K.

This function can be used within the lexical scope of
with-call/cc and outside."
  (drive-interpreter/cc
   (lambda ()
     (let ((k (apply (car k) (cdr k))))
       (cond
         (other-values (apply k primary-value other-values))
         (primary-value-p (funcall k primary-value))
         (t (funcall k nil)))))))

(def (special-variable e) *debug-evaluate/cc* #f
  "When true the evaluator will print, at each evaluation step, what it's evaluating and the value passed in from the previous step.

If set to :FULL then at each step we print the form, the environment and the continuation. If set to T we just print the form being evaluated.")

;;;; Implementation

(defun drive-interpreter/cc (code)
  (catch 'done
    (loop for thunk = code then (funcall thunk))))

(def layered-method hu.dwim.walker::function-name? :in delico (name)
  (or (member name '(call/cc) :test #'eq)
      (call-next-layered-method)))

(def (macro e) let/cc (k &body body)
  `(call/cc (lambda (,k) ,@body)))

(defmacro klambda ((&optional (value (gensym) valuep) (other-values (gensym) other-values-p))
                   &body body)
  (cond
    (other-values-p `(lambda (&optional ,value &rest ,other-values)
                       (lambda ()
                         ,@body)))
    (valuep `(lambda (&optional ,value &rest ,other-values)
               (declare (ignore ,other-values))
               (lambda ()
                 ,@body)))
    (t `(lambda (&optional ,value &rest ,other-values)
          (declare (ignore ,value ,other-values))
          (lambda ()
            ,@body)))))

;; TODO use a proper logging lib
(def (special-variable e) *trace-cc* #f
  "Variable which controls the tracing of WITH-CALL/CC code.

When not NIL the interepreter will report what code it is
evaluating and what it returns.")

(defmacro trace-statement (format-control &rest format-args)
  `(when *trace-cc*
     (format *trace-output* ,(concatenate 'string "~&" format-control "~%") ,@format-args)))

(defun kontinue (k &optional (primary-value nil primary-value-p) &rest other-values)
  (trace-statement "Got ~S~{; ~S~}" primary-value other-values)
  (let ((k (apply (car k) (cdr k))))
    (cond
      (other-values (apply k primary-value other-values))
      (primary-value-p (funcall k primary-value))
      (t (funcall k)))))

(defmacro defk (name args k-args &body body)
  `(defun ,name ,args
     (declare (ignorable ,@args))
     (klambda ,k-args
       (when *debug-evaluate/cc*
         (format *debug-io* "~&(~S~{~^ ~S~}) Got (values~{~^ ~S~}).~%" ',name (list ,@args) (list ,@k-args)))
       ,@body)))

(defgeneric evaluate/cc (form lexical-environment dynamic-environment k))

(defmethod evaluate/cc ((form t) lex-env dyn-env k)
  (declare (ignore lex-env dyn-env k))
  (error "No EVALUATE/CC method defined for ~S." form))

(defmethod evaluate/cc :around ((form walked-form) lex-env dyn-env k)
  (declare (ignore lex-env dyn-env k))
  (trace-statement "Evaluating ~S." (source-of form))
  (call-next-method))

(defun print-debug-step (form lex-env dyn-env k)
  (let ((*print-pretty* nil))
    (ecase *debug-evaluate/cc*
      (:full
       (format *debug-io*
               "~&Evaluating: ~S~%~3TLex Env: ~S~%~3TDyn Env: ~S~%~3TK: ~S~%"
               form lex-env dyn-env k))
      ((t)
       (format *debug-io* "~&Evaluating: ~S~%" form))
      ((nil) ;; do nothing
       nil))))

(defmethod evaluate/cc :before (form lex-env dyn-env k)
  (when *debug-evaluate/cc*
    (print-debug-step form lex-env dyn-env k)))

(defun toplevel-k ()
  (klambda ((value nil valuep) other-values)
    (throw 'done (if valuep
                     (values-list (cons value other-values))
                     (values)))))

(define-constant +toplevel-k+ '(toplevel-k) :test #'equal)
