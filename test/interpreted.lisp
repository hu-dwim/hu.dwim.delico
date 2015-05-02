;;;; -*- lisp -*-

(in-package :hu.dwim.delico.test)

(def macro with-delico-test-compile-time-env (&body body)
  `(bind ((*package* (find-package :hu.dwim.delico.test)))
     (hu.dwim.def:setup-readtable/same-as-package :hu.dwim.delico.test)
     ,@body))

(def macro with-call/cc/test (&body form-as-string)
  (assert (length= 1 form-as-string)) ; &body for indenting only...
  `(eval
    (with-delico-test-compile-time-env
      (read-from-string (string+ "(with-call/cc " ,(first form-as-string) ")")))))

(def function eval-from-string (form-as-string)
  (with-delico-test-compile-time-env
    (eval (read-from-string form-as-string))))

(def function k-contains-closure? (k)
  (map/tree (lambda (el)
              (when (typep el 'function)
                (return-from k-contains-closure? #t)))
            k)
  #f)

(def suite* (test/interpreted :in test))

(def test test/interpreted/constant ()
  (is (= 4     (with-call/cc/test "4")))
  (is (eql :a  (with-call/cc/test ":a")))
  (is (eql 'a  (with-call/cc/test "'a")))
  (is (eql #'+ (with-call/cc/test "#'+"))))

(def test test/interpreted/progn ()
  (is (null (with-call/cc/test "")))
  (is (= 1 (with-call/cc/test "1")))
  (is (= 2 (with-call/cc/test "1 2")))
  (is (= 1 (kall (with-call/cc/test "(let/cc k k) 1"))))
  (is (= 2 (kall (with-call/cc/test "(let/cc k k) 1 2")))))

(def test test/interpreted/multiple-value-prog1 ()
  (is (= 1 (with-call/cc/test
             ｢(multiple-value-prog1
                  1
                2)｣)))
  (eval-from-string
   ｢(bind ((run #f)
           (k (with-call/cc
                (multiple-value-prog1
                    (values (let/cc k k) "second value" "third-value")
                  (length "foo")
                  (setf run #t)))))
      (is (not run))
      (is (continuation? k))
      (is (equalp '(42 "second value" "third-value")
                  (multiple-value-list (kall k 42))))
      (is run))｣))

(def test test/interpreted/let ()
  (is (= 1 (with-call/cc/test
             ｢(let ()
                1)｣)))
  (is (= 1 (with-call/cc/test
             ｢(let ((a 1))
                a)｣)))
  (is (= 1 (with-call/cc/test
             ｢(let ((a 1))
                (let ((a nil)
                      (b a))
                  (declare (ignore a))
                  b))｣)))
  (with-call/cc/test
    ｢(let ((a 1))
       (let ((a 2))
         (is (= 2 a)))
       (is (= 1 a)))｣)

  (let ((cont nil))
    (setf cont
          (with-call/cc/test
            ｢(let ((a (let/cc k k)))
               (+ a 4))｣))
    (is (= 9 (kall cont 5)))
    (is (= 12 (kall cont 8)))))

(def test test/interpreted/let-in-cc ()
  (let ((k (with-call/cc/test
             ｢(let ((a (let/cc k k)))
                (+ a 1))｣)))
    (is (= 1 (kall k 0)))
    (is (= 2 (kall k 1)))))

(def test test/interpreted/setq ()
  (is (= 1 (with-call/cc/test
             ｢(let ((a nil))
                (setq a 1))｣)))
  (is (= 2 (with-call/cc/test
             ｢(let ((a 1))
                (setq a (1+ a)))｣))))

(def test test/interpreted/let* ()
  (with-call/cc/test
    ｢(let* ((a 1)
            (b a))
       (is (= 1 a))
       (is (= 1 b)))｣)
  (with-call/cc/test
   ｢(let ((a 0)
          (b 1))
      (declare (ignore a))
      (let* ((a b)
             (b a))
        (is (= a 1))
        (is (= b 1))
        (setq a 47)
        (is (= a 47))))｣))

(def test test/interpreted/apply ()
  (is (= 0 (with-call/cc/test ｢(+)｣)))
  (is (= 1 (with-call/cc/test ｢(+ 1)｣)))
  (is (= 2 (with-call/cc/test ｢(+ 1 1)｣)))
  (is (= 3 (with-call/cc/test ｢(+ 1 (+ 1 (+ 1 (+))))｣))))

(def test test/interpreted/if ()
  (is (= 1 (with-call/cc/test ｢(if t 1)｣)))
  (is (= 1 (with-call/cc/test ｢(if nil 0 1)｣)))
  (is (null (with-call/cc/test ｢(if nil 1)｣))))

(def test test/interpreted/block/return-from ()
  (is (= 1
         (with-call/cc/test
           ｢(block foo
              nil
              (return-from foo 1)
              nil)｣)))
  (is (eql t
           (with-call/cc/test
             ｢(block foo
                (return-from foo t)
                nil)｣))))

(def function reached-unreachable-code ()
  (is nil "Somehow we reached unreachable code in a tagbody!"))

(def test test/interpreted/tagbody ()
  (macrolet ((test (expected form)
               `(with-call/cc/test
                  ,(bind ((*print-readably* #t))
                     (prin1-to-string
                      `(let ((path '()))
                         (flet ((pass (marker)
                                  (appendf path (list marker))
                                  path))
                           (is (equal ',expected
                                      ,form)))))))))
    (test (a)
          (tagbody
             (go a)
             (reached-unreachable-code)
           a
             (pass 'a)))
    (test (a b c)
          (tagbody
             (go a)
             (reached-unreachable-code)
           b
             (pass 'b)
             (go c)
             (reached-unreachable-code)
           a
             (pass 'a)
             (go b)
             (reached-unreachable-code)
           c
             (pass 'c))))
  (with-call/cc/test
    ｢(let ((counter 0))
       (dotimes (i 5)
         (incf counter))
       (is (= 5 counter)))｣)
  (with-call/cc/test
    ｢(let ((i 0))
       (tagbody
        a
          (incf i)
          (is (= 1 i))
        b
          (incf i)
          (is (= 2 i))
        c
          (is (= 2 i))))｣))

(def test test/interpreted/flet ()
  (with-call/cc/test
    ｢(flet ((foo () 'x))
       (is (eql 'x (foo))))
     (is (= 4 (funcall (let ((a 4))
                         (flet ((foo () a))
                           #'foo)))))
     (flet ((foo ()
              'outer-foo))
       (flet ((foo ()
                'inner-foo)
              (bar ()
                (foo)))
         (is (eql 'outer-foo (bar)))))｣))

(def test test/interpreted/labels ()
  (with-call/cc/test
    ｢(labels ((foo () 'x))
       (is (eql 'x (foo))))
     (labels ((foo () 'outer-foo))
       (labels ((bar () (foo))
                (foo () 'inner-foo))
         (is (eql 'inner-foo (bar)))))｣)
  (finishes
    ｢(with-call/cc/test
       (labels ((rec (x) x))
         #'rec
         (is (= 1 (funcall #'rec 1)))
         (is (= 1 (apply #'rec (list 1)))))
       (flet ((f () 1))
         (is (= 1 (f)))
         (is (= 1 (funcall #'f)))
         (is (= 1 (apply #'f '())))))｣)
  (let ((cont (with-call/cc/test
                ｢(labels ((rec (n)
                            (if (zerop n)
                                0
                                (+ (rec (1- n))
                                   (let/cc k k)))))
                   (rec 2))｣)))
    (is (= 5 (kall (kall cont 2) 3)))))

(let ((value 0))
  (defun test-funcall.0 ()
    value)
  (defun (setf test-funcall.0) (new-value)
    (setf value new-value)))

(def test test/interpreted/setf-funcall ()
  (setf (test-funcall.0) 0)
  (is (= 0 (with-call/cc/test ｢(test-funcall.0)｣)))
  (is (= 1 (with-call/cc/test ｢(setf (test-funcall.0) 1)｣)))
  (is (= 2 (with-call/cc/test ｢(funcall #'(setf test-funcall.0) 2)｣))))

(def test test/interpreted/lambda-requried-arguments ()
  (with-call/cc/test
    ｢(is (eql t (funcall (lambda () t))))
     (is (eql t (funcall (lambda (x) x) t)))｣)
  (signals error
    (with-call/cc/test
      ｢(funcall (lambda (x) x))｣)))

(def test test/interpreted/lambda-optional-arguments ()
  (with-call/cc/test
    ｢(is (eql t (funcall (lambda (&optional a) a) t)))
     (is (eql t (funcall (lambda (&optional (a t)) a))))｣)

  (let ((cont (with-call/cc/test
                ｢(funcall (lambda (&optional (a (let/cc k k)))
                            (+ a 1)))｣)))
    (is (= 1 (kall cont 0)))))

(def test test/interpreted/lambda-keyword-arguments ()
  (with-call/cc/test
    ｢(is (eql 'a   (funcall (lambda (&key a) a) :a 'a)))
     (is (eql 'b   (funcall (lambda (&key (a 'b)) a))))
     (is (eql t    (funcall (lambda (&optional a &key (b (not a))) b))))
     (is (eql nil  (funcall (lambda (&optional a &key (b (not a)))
                              b)
                            t)))
     (is (eql 42 (funcall (lambda (&optional a &key (b (not a)))
                            b)
                          t :b 42)))｣))

(defun/cc test-defun/cc1 ()
  (let/cc k k))

(defun/cc test-defun/cc2 (arg1)
  (let/cc k k)
  arg1)

(defun/cc test-defun/cc3 (a &key (b 1))
  (+ a b))

(def test test/interpreted/defun/cc ()
  (let ((cont nil))
    (setf cont (with-call/cc/test ｢(test-defun/cc1)｣))
    (is (eql nil (kall cont nil)))

    (setf cont (with-call/cc/test ｢(test-defun/cc2 'foo)｣))
    (is (eql 'foo (kall cont)))
    (is (eql 'foo (kall cont nil)))

    (with-call/cc/test
      ｢(is (= 1 (test-defun/cc3 0)))
       (is (= 2 (test-defun/cc3 1)))｣)))

(defgeneric/cc test-generic/cc1 (a &key v))

(defmethod/cc test-generic/cc1 ((a symbol) &key (v 3))
  v)

(defmethod/cc test-generic/cc1 ((a string) &key (v 5))
  v)

(def test test/interpreted/generic-method/1 ()
  (with-call/cc/test
    ｢(is (= 3 (test-generic/cc1 'a)))
     (is (= 0 (test-generic/cc1 'a :v 0)))
     (is (= 5 (test-generic/cc1 "a")))
     (is (= 0 (test-generic/cc1 "a" :v 0)))｣))

(defmethod/cc test-generic/cc2 (a)
  'primary)

(defmethod/cc test-generic/cc2 :before (a)
  (let/cc k 'before))

(def test test/interpreted/generic-method/2 ()
  (with-call/cc/test
    ｢(is (eql 'before (test-generic/cc2 t)))｣))

(defmethod/cc test-generic/cc3 (a)
  (let/cc k (cons 'primary k))
  a)

(defmethod/cc test-generic/cc3 :before (a)
  (let/cc k (cons 'before k)))

(defmethod/cc test-generic/cc3 :around (a)
  (let/cc k (cons 'around k))
  ;; TODO simply (call-next-method) should work
  (call-next-method a))

(defmethod/cc test-generic/cc3 :after (a)
  (let/cc k (cons 'after k)))

(def test test/interpreted/generic-method/3 ()
  (eval-from-string
   ｢(bind (((value . cont) (with-call/cc
                             (test-generic/cc3 32))))
      (is (eql 'around value))
      (bind (((value . cont) (with-call/cc
                               (kall cont))))
        (is (eql 'before value))
        (bind (((value . cont) (with-call/cc
                                 (kall cont))))
          (is (eql 'primary value))
          (bind (((value . cont) (with-call/cc
                                   (kall cont))))
            (is (eql 'after value))
            (is (eql 32 (kall cont)))))))｣))

(def test test/interpreted/loop ()
  (let ((cont (with-call/cc/test
                ｢(loop
                   :repeat 2
                   :sum (let/cc k k) :into total
                   :finally (return (values total total)))｣)))
    (multiple-value-bind (a b)
        (kall (kall cont 1) 2)
      (is (= 3 a))
      (is (= 3 b))))

  (let ((cont (with-call/cc/test
                ｢(block done
                   (loop
                     :for how-many = (let/cc k k)
                     :do (loop
                           :repeat how-many
                           :sum (let/cc k k) :into total
                           :finally (return-from done total))))｣)))
    (is (= 26 (kall (kall (kall cont 2) 13) 13)))))

(def test test/interpreted/common-lisp ()
  (eval-from-string
   ｢(let (cont value)
      (setf cont (with-call/cc
                   (mapcar (lambda (x)
                             (+ x (let/cc k k)))
                           (list 1 2 3))))
      (setf cont (with-call/cc
                   (kall cont -1))
            cont (with-call/cc
                   (kall cont -2))
            value (with-call/cc
                    (kall cont -3)))
      (is (equal (list 0 0 0) value)))｣))

(defun/cc throw-something (something)
  (throw 'done something))

(def test test/interpreted/catch ()
  (with-call/cc/test
    ｢(is (eql t
              (catch 'whatever
                (throw 'whatever t)
                (throw 'whatever nil)
                'something-else)))
     (is (eql t
              (catch 'whatever
                t)))
     (is (eql t
              (flet ((throw-it (it)
                       (throw 'done it)))
                (catch 'done
                  (throw-it t)
                  (throw 'done 'bad-bad-bad)))))
     (is (eql t
              (catch 'done
                (throw-something t)
                nil)))｣))

(def test test/interpreted/multiple-value-call ()
  (with-call/cc/test
    ｢(is (= 1 (multiple-value-call
                  #'identity
                (values 1))))｣)
  (with-call/cc/test
    ｢(is (= 3 (length (multiple-value-call
                          #'list
                        (values 1)
                        (values 1)
                        (values 1)))))｣)

  (with-call/cc/test
    ｢(is (= 3 (multiple-value-call
                  (lambda (a b)
                    (+ a b))
                (values 1 2))))｣)

  (with-call/cc/test
    ｢(is (= 3 (multiple-value-call
                  (lambda (&rest numbers)
                    (reduce #'+ numbers))
                (values -1 1)
                (values 1)
                (values -1)
                (values 1 2))))｣))

(def test test/interpreted/values-through-call/cc ()
  (is (equal nil (multiple-value-list
                  (with-call/cc/test
                    ｢(values)｣))))
  (is (equal (list 1 2 3)
             (multiple-value-list
              (with-call/cc/test
                ｢(call/cc (lambda (k)
                            (values 1 2 3)))｣)))))

(def test test/interpreted/load-time-value ()
  (is (equal (list 1 2 3)
             (with-call/cc/test
               ｢(load-time-value (list 1 2 3) t)｣))))

(def test test/interpreted/bug/macrolet-saves-closure-into-fasl ()
  (with-temporary-files ((lisp-filename)
                         (fasl-filename))
    (write-string-into-file "(in-package :hu.dwim.delico.test)
                               (with-call/cc
                                 (macrolet ((foo (x)
                                              `(+ 1 ,x)))
                                   (foo 42)))"
                            lisp-filename :if-exists :supersede)
    (is (not (nth-value 2 (compile-file lisp-filename :output-file fasl-filename))))))

(def test test/interpreted/bug/closure-in-k ()
  (is (not (k-contains-closure? (eval '(with-call/cc
                                        (let ((a (call/cc (lambda (k) k))))
                                          (+ 5 a)))))))
  (with-expected-failures
    (is (not (k-contains-closure? (eval
                                   '(with-call/cc
                                     (+ 5 (call/cc (lambda (k) k))))))))))

(def test test/interpreted/kall ()
  (is (= 42 (with-call/cc/test
              ｢(let ((x 21))
                 (let/cc k
                   (+ (kall k) (kall k)))
                 x)｣)))
  (is (= 42 (with-call/cc/test
              ｢(let ((x 21))
                 (let/cc k
                   (+ (kall k)
                      (let/cc k
                        (kall k x))))
                 x)｣))))
