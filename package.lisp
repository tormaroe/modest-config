;;;; package.lisp

(defpackage #:modest-config
  (:nicknames #:modest)
  (:use #:cl)
  (:export #:find-config
           #:load-config
           #:with-config))

