;;;; modest-config.asd

(asdf:defsystem #:modest-config
  :description "A modest config file loader library"
  :author "Torbjørn Marø <torbjorn.maro@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "modest-config")))

