
(in-package #:modest-config)

(defun assert-config-exist (pathname)
  (if (probe-file pathname)
    pathname
    (error "Config file ~a does not exist." pathname)))

(defun assert-one-exist (pathnames)
  (let ((match (find-if #'probe-file pathnames)))
    (if match
      match
      (error "Can't locate any config file candidate: ~s." pathnames))))

(defun find-by-conventions ()
  "Will probe for a file in this order:
   - current-package-name.config
   - modest.config
   - ~/current-package-name.config"
  (let ((p (package-name *package*)))
    (assert-one-exist 
      (mapcar #'parse-namestring
        (list (format nil "~a.config" p)
              "modest.config"
              (format nil "~~/~a.config" p))))))

(defun find-by-symbol (symbol)
  "Will probe for a file in this order (given symbol 'foo or :foo):
   - FOO.config
   - ~/FOO.config"
  (assert-one-exist
    (mapcar #'parse-namestring
      (list (format nil "~a.config" symbol)
            (format nil "~~/~a.config" symbol)))))

(defun find-config (&optional identifier)
  "Finds the config filename based on identifier and some conventions.
   Identifier may be nil, a symbol, or a filename (with or without path).
   If config file can't be found a condition is raised."
  (ctypecase identifier
    (null (find-by-conventions))
    (pathname (assert-config-exist identifier))
    (string (assert-config-exist (parse-namestring identifier)))
    (symbol (find-by-symbol identifier))))

(defun load-config (&optional identifier)
  "Simply reads the first element from config file,
   assumed to be a property list, and returns it.
   If identifier already is a list, it's assumed to be the config and 
   modestly returned."
  (typecase identifier
    (list identifier)
    (stream (read identifier t))
    (otherwise
      (let ((filespec (find-config identifier)))
        (with-open-file (stream filespec :direction :input)
          (read stream t))))))
  
(defun zip-bindings (map-f bindings)
  (reduce #'append 
    (mapcar #'list bindings (mapcar map-f bindings))))

(defmacro with-config (identifier bindings &body body)
  "Example where there exists a file foobar.config that contains a plist
   with properties HOST and PORT:

   (with-config :foobar (host port)
     (format t \"Host is ~a~%\" host)
     (format t \"Port is ~a~%\" port))
   
   Identifier may be nil to load file based on modest conventions:

   (with-config nil (host port)
     (format t \"Host is ~a~%\" host)
     (format t \"Port is ~a~%\" port))"
  (let* ((sym-config (gensym "config"))
         (set-bindings (zip-bindings (lambda (b)
                                       `(getf ,sym-config (quote ,b)))
                                     bindings)))
    `(let ,bindings
       (let ((,sym-config (load-config ,identifier)))
         (setf ,@set-bindings)
         (values (progn ,@body)
                 ,sym-config)))))