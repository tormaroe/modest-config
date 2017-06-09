
(asdf:defsystem #:modest-config-test
  :description "Test system for modest-config"
  :author "Torbjørn Marø"
  :license "MIT License"
  :depends-on (#:modest-config
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :serial t
  :components ((:test-file "modest-config-test"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))