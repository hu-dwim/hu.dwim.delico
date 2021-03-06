;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.delico)

;;;; ** Handlres for common-lisp special operators

;;;; Variable References

(defmethod evaluate/cc ((node walked-lexical-variable-reference-form) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (kontinue k (lookup lex-env :let (name-of node) :error-p t)))

(defmethod evaluate/cc ((node unwalked-lexical-variable-reference-form) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (kontinue k (funcall (first (lookup lex-env :lexical-let (name-of node) :error-p t)))))

(defmethod evaluate/cc ((node special-variable-reference-form) lex-env dyn-env k)
  (declare (ignore lex-env))
  (multiple-value-bind (value foundp)
      (lookup dyn-env :let (name-of node))
    (if foundp
        (kontinue k value)
        (kontinue k (symbol-value (name-of node))))))

;;;; Constants

(defmethod evaluate/cc ((node constant-form) lex-env dyn-env k)
  (declare (ignore lex-env dyn-env))
  (kontinue k (value-of node)))

;;;; BLOCK/RETURN-FROM

(defmethod evaluate/cc ((node block-form) lex-env dyn-env k)
  (evaluate-progn/cc (body-of node)
                     (register lex-env :block (name-of node) k)
                     dyn-env k))

(defmethod evaluate/cc ((node return-from-form) lex-env dyn-env k)
  (declare (ignore k))
  (evaluate/cc (result-of node)
               lex-env dyn-env
               (lookup lex-env :block (name-of (target-block-of node)) :error-p t)))

;;;; CATCH/THROW

(defmethod evaluate/cc ((node catch-form) lex-env dyn-env k)
  (evaluate/cc (tag-of node) lex-env dyn-env
               `(catch-tag-k ,node ,lex-env ,dyn-env ,k)))

(defk catch-tag-k (catch lex-env dyn-env k)
    (tag)
  (evaluate-progn/cc (body-of catch) lex-env (register dyn-env :catch tag k) k))

(defmethod evaluate/cc ((node throw-form) lex-env dyn-env k)
  (evaluate/cc (tag-of node) lex-env dyn-env
               `(throw-tag-k ,node ,lex-env ,dyn-env ,k)))

(defk throw-tag-k (throw lex-env dyn-env k)
    (tag)
  (evaluate/cc (value-of throw) lex-env dyn-env
               (lookup dyn-env :catch tag :error-p t)))

;;;; FLET/LABELS

(defmethod evaluate/cc ((node flet-form) lex-env dyn-env k)
  (bind ((new-env lex-env))
    (dolist (binding (bindings-of node))
      (setf new-env (register new-env :flet (name-of binding) (make-closure/cc binding lex-env))))
    (evaluate-progn/cc (body-of node) new-env dyn-env k)))

(defmethod evaluate/cc ((node labels-form) lex-env dyn-env k)
  (bind ((closures '()))
    (dolist (binding (bindings-of node))
      (bind ((closure (make-closure/cc binding)))
        (setf lex-env (register lex-env :flet (name-of binding) closure))
        (push closure closures)))
    (dolist (closure closures)
      (setf (environment-of closure) lex-env))
    (evaluate-progn/cc (body-of node) lex-env dyn-env k)))

;;;; LET/LET*

;; Returns a dynamic environment that holds the special variables imported for LET.
;; These variables are captured from the caller normal lisp code and stored within
;; the continuation. The mixin might be a lexibinder-form and implicit-progn-with-declarations-mixin
(defun import-specials (mixin dyn-env)
  (dolist (declaration (declarations-of mixin))
    (let ((name (name-of declaration)))
      (when (and (typep declaration 'special-variable-declaration-form)
                 (or (not (typep mixin 'binder-form-mixin))
                     (not (find name (bindings-of mixin) :key #'name-of)))
                 (not (lookup dyn-env :let name)))
        (setf dyn-env (register dyn-env :let name (symbol-value name))))))
  dyn-env)

(defmethod evaluate/cc ((node let-form) lex-env dyn-env k)
  (evaluate-let/cc (bindings-of node) nil (body-of node) lex-env (import-specials node dyn-env) k))

(defk k-for-evaluate-let/cc (var remaining-bindings evaluated-bindings body lex-env dyn-env k)
    (value)
  (evaluate-let/cc remaining-bindings
                   (cons (cons var value) evaluated-bindings)
                   body lex-env dyn-env k))

(defun evaluate-let/cc (remaining-bindings evaluated-bindings body lex-env dyn-env k)
  (if remaining-bindings
      (bind ((binding (car remaining-bindings))
             (var (name-of binding))
             (initial-value (initial-value-of binding)))
        (evaluate/cc
         initial-value
         lex-env dyn-env
         `(k-for-evaluate-let/cc
           ,var
           ,(cdr remaining-bindings)
           ,evaluated-bindings
           ,body
           ,lex-env ,dyn-env ,k)))
      (dolist* ((var . value) evaluated-bindings
                (evaluate-progn/cc body lex-env dyn-env k))
        (if (special-variable? var (parent-of (first body)))
            (setf dyn-env (register dyn-env :let var value))
            (setf lex-env (register lex-env :let var value))))))

(defun special-variable? (var declares-mixin)
  (or (find-if (lambda (declaration)
                 (and (typep declaration 'special-variable-declaration-form)
                      (eq (name-of declaration) var)))
               (declarations-of declares-mixin))
      (special-variable-name? var)))

(defmethod evaluate/cc ((node let*-form) lex-env dyn-env k)
  (evaluate-let*/cc (bindings-of node) (body-of node) lex-env (import-specials node dyn-env) k))

(defk k-for-evaluate-let*/cc (var bindings body lex-env dyn-env k)
    (value)
  (if (special-variable? var (parent-of (first body)))
      (evaluate-let*/cc bindings body
                        lex-env
                        (register dyn-env :let var value)
                        k)
      (evaluate-let*/cc bindings body
                        (register lex-env :let var value)
                        dyn-env
                        k)))

(defun evaluate-let*/cc (bindings body lex-env dyn-env k)
  (if bindings
      (bind ((binding (car bindings)))
        (evaluate/cc (initial-value-of binding) lex-env dyn-env
                     `(k-for-evaluate-let*/cc ,(name-of binding) ,(cdr bindings) ,body ,lex-env ,dyn-env ,k)))
      (evaluate-progn/cc body lex-env dyn-env k)))

;;;; IF

(defk k-for-evaluate-if/cc (then else lex-env dyn-env k)
    (value)
  (if value
      (evaluate/cc then lex-env dyn-env k)
      (evaluate/cc else lex-env dyn-env k)))

(defmethod evaluate/cc ((node if-form) lex-env dyn-env k)
  (evaluate/cc (condition-of node) lex-env dyn-env
               `(k-for-evaluate-if/cc ,(then-of node) ,(else-of node) ,lex-env ,dyn-env ,k)))

;;;; LOCALLY

(defmethod evaluate/cc ((node locally-form) lex-env dyn-env k)
  (evaluate-progn/cc (body-of node) lex-env dyn-env k))

;;;; MACROLET

(defmethod evaluate/cc ((node macrolet-form) lex-env dyn-env k)
  ;; since the walker already performs macroexpansion there's nothing
  ;; left to do here.
  (evaluate-progn/cc (body-of node) lex-env dyn-env k))

;;;; multiple-value-call

(defk k-for-m-v-c (remaining-arguments evaluated-arguments lex-env dyn-env k)
    (value other-values)
  (evaluate-m-v-c
   remaining-arguments (append evaluated-arguments (list value) other-values)
   lex-env dyn-env k))

(defun evaluate-m-v-c (remaining-arguments evaluated-arguments lex-env dyn-env k)
  (if remaining-arguments
      (evaluate/cc (car remaining-arguments) lex-env dyn-env
                   `(k-for-m-v-c  ,(cdr remaining-arguments) ,evaluated-arguments ,lex-env ,dyn-env ,k))
      (destructuring-bind (function &rest arguments)
          evaluated-arguments
        (etypecase function
          (closure/cc (apply-lambda/cc function arguments dyn-env k))
          (function (apply #'kontinue k (multiple-value-list
                                         (multiple-value-call function (values-list arguments)))))))))

(defmethod evaluate/cc ((node multiple-value-call-form) lex-env dyn-env k)
  (evaluate-m-v-c (list* (function-designator-of node) (arguments-of node)) '() lex-env dyn-env k))

;;;; PROGN

(defmethod evaluate/cc ((node progn-form) lex-env dyn-env k)
  (evaluate-progn/cc (body-of node) lex-env dyn-env k))

(defk k-for-evaluate-progn/cc (rest-of-body lex-env dyn-env k)
    ()
  (evaluate-progn/cc rest-of-body lex-env dyn-env k))

(defun evaluate-progn/cc (body lex-env dyn-env k)
  (cond
    ((cdr body)
      (evaluate/cc (first body) lex-env dyn-env
                    `(k-for-evaluate-progn/cc ,(cdr body) ,lex-env ,dyn-env ,k)))
    (body
     (evaluate/cc (first body) lex-env dyn-env k))
    (t
     (kontinue k nil))))

;;;; MULTIPLE-VALUE-PROG1

(defmethod evaluate/cc ((node multiple-value-prog1-form) lex-env dyn-env k)
  (cond ((and (null (first-form-of node)) (null (other-forms-of node)))
         (kontinue k nil))
        ((null (other-forms-of node))
         (evaluate/cc (first-form-of node) lex-env dyn-env k))
        (t
         (evaluate/cc (first-form-of node) lex-env dyn-env
                      `(k-for-multiple-value-prog1/cc-other-forms ,(other-forms-of node) ,lex-env ,dyn-env ,k)))))

(defk k-for-multiple-value-prog1/cc-other-forms (other-forms lex-env dyn-env k)
    (value other-values)
  (evaluate-progn/cc other-forms lex-env dyn-env
                     `(k-for-multiple-value-prog1/cc-result ,value ,other-values ,lex-env ,dyn-env ,k)))

(defk k-for-multiple-value-prog1/cc-result (value other-values lex-env dyn-env k)
    ()
  (apply #'kontinue k value other-values))

;;;; SETQ

(defk k-for-walked-lexical-setq (var lex-env dyn-env k)
    (value)
  (setf (lookup lex-env :let var :error-p t) value)
  (kontinue k value))

(defk k-for-unwalked-lexical-setq (var lex-env dyn-env k)
    (value)
  (funcall (second (lookup lex-env :lexical-let var :error-p t)) value)
  (kontinue k value))

(defk k-for-free-setq (var lex-env dyn-env k)
    (value)
  (setf (symbol-value var) value)
  (kontinue k value))

(defmethod evaluate/cc ((node setq-form) lex-env dyn-env k)
  (bind ((variable (variable-of node))
         (variable-name (name-of variable)))
    (macrolet ((if-found (&key in-env of-type kontinue-with)
                 `(multiple-value-bind (value foundp)
                      (lookup ,in-env ,of-type variable-name)
                    (declare (ignore value))
                    (when foundp
                      (return-from evaluate/cc
                        (evaluate/cc (value-of node) lex-env dyn-env
                                     `(,',kontinue-with ,variable-name ,lex-env ,dyn-env ,k)))))))
      (etypecase variable
        (walked-lexical-variable-reference-form
         (if-found :in-env lex-env
                   :of-type :let
                   :kontinue-with k-for-walked-lexical-setq)
         (error "What?! Couldn't find the lexical variable ~S in the cc evaluator's environment?!" variable-name))
        (special-variable-reference-form
         (if-found :in-env dyn-env
                   :of-type :let
                   :kontinue-with k-for-special-setq)
         (evaluate/cc (value-of node)
                      lex-env dyn-env
                      `(k-for-free-setq ,variable-name ,lex-env ,dyn-env ,k)))
        (unwalked-lexical-variable-reference-form
         (if-found :in-env lex-env
                   :of-type :lexical-let
                   :kontinue-with k-for-unwalked-lexical-setq)
         (error "What?! Couldn't find the lexical variable ~S in the cc evaluator's environment?!" variable-name))))))

;;;; SYMBOL-MACROLET

(defmethod evaluate/cc ((node symbol-macrolet-form) lex-env dyn-env k)
  ;; like macrolet the walker has already done all the work needed for this.
  (evaluate-progn/cc (body-of node) lex-env dyn-env k))

;;;; TAGBODY/GO

(defk tagbody-k (k)
    ()
  (kontinue k nil))

(defmethod evaluate/cc ((node tagbody-form) lex-env dyn-env k)
  (evaluate-progn/cc (body-of node)
                     (register lex-env :tag node k) dyn-env
                     `(tagbody-k ,k)))

(defmethod evaluate/cc ((node go-tag-form) lex-env dyn-env k)
  (declare (ignore node lex-env dyn-env))
  (kontinue k nil))

(defmethod evaluate/cc ((node go-form) lex-env dyn-env k)
  (declare (ignore k))
  (evaluate-progn/cc (jump-target-of node) lex-env dyn-env
                     (lookup lex-env :tag (enclosing-tagbody-of node) :error-p t)))

;;;; THE

(defmethod evaluate/cc ((node the-form) lex-env dyn-env k)
  (evaluate/cc (value-of node) lex-env dyn-env k))

;;;; LOAD-TIME-VALUE

(defmethod evaluate/cc ((node load-time-value-form) lex-env dyn-env k)
  (declare (ignore lex-env dyn-env))
  (kontinue k (value-of node)))
