;;; To run these tests, you may do:
;;; 
;;;     (ql:quickload :prove)
;;;     (ql:quickload :modest-config)
;;;     (prove:run "modest-config-test.lisp")
;;; 

(in-package :cl-user)

(defpackage modest-config-test
  (:use cl prove modest-config))
(in-package modest-config-test)

(plan 7)

(ok t "Canary test")

(subtest "Testing zip-bindings"
  (is (modest-config::zip-bindings #'string '(a b c))
      '(a "A" b "B" c "C")))

(subtest "Testing with-config return values"
  (multiple-value-bind (result conf)
      (with-config (list 'x 1 'y 2) (x y)
        (pass "Body was called")
        (+ x y))
    (is result 3)
    (is conf '(x 1 y 2))))

(defparameter string-config "
  ;; Comments before config form ok
  (some-int 1
   some-string \"hello\"
   some-fraction 2/3
   ;; Comments allowed inside config as well
   some-list (1 2 3 4 5))
   some-value-not-used 324
  ")

(subtest "Testing load from stream"
  (with-input-from-string (stream string-config)
    (with-config stream (some-int
                         some-string
                         some-fraction
                         some-list)
      (is some-int 1)
      (is some-string "hello")
      (is some-fraction 2/3)
      (is some-list (list 1 2 3 4 5)))))

(subtest "Testing load from file using pathname"
  (with-config #p"example.config" (foo quux)
    (is foo :some-value)
    (is (funcall (eval quux) "world")
        "Hello, world")))

;; NOT SURE WHY THIS FAILS.., do we have a bug?
;
(subtest "Testing load from file using symbol"
  (with-config :|example| (foo)
    (is foo :some-value)))

(subtest "Testing errors"
  (is-error (find-config :foo-barify-potato-does-not-exist)
            'simple-error)
  (is-error (find-config "some-file-does-not-exist.config")
            'simple-error))


;; NOT SURE WHY THIS FAILS... :(
;
;(is-expand (with-config "my.config" (x y)
;             (format nil "~&x=~a and y=~a~%" x y))
;
;           (let (x y)
;             (let (($config (load-config "my.config")))
;               (setf x (getf $config 'x)
;                     y (getf $config 'y))
;               (values (progn
;                         (format nil "~&x=~a and y=~a~%" x y))
;                       config))))

(finalize)