***MODEST-CONFIG*** is a simple configuration file parser for Common Lisp. Config files contain a single Common Lisp property list, and the parser provides a convenient way to extract the properties.

Given a config file:

```
(
  ;; This sets property FOO
  foo :some-value

  ;; Here is another property
  bar "This is a string"

  zot (list 1 2 3 4 5)

  ;; Config may also contain functions
  ;; ... if you are willing to trust your config maintainer
  quux (lambda (wombat)
         (format nil "Hello, ~a" wombat))
)
```

You may load and extract properties like this:

```
(with-config "example.config" (zot bar quux)
  (format t "The value of zot is ~s~%" zot)
  (format t "~a says bar~%" bar)
  (format t "(quux \"World\") => ~s~%" (funcall (eval quux) "World")))
```

Output:

```
The value of zot is (1 2 3 4 5)
This is a string says bar
(quux "World") => "Hello, World"
```

## Loading modest-config

```
(ql:quickload :modest-config)
```

The system only contain the package `modest-config`, which also has the nickname `modest`. There are no external dependencies.

## API

**find-config** &optional identifier => pathname

**load-config** &optional identifier => plist

**with-config** identifier bindings &body body => result

`identifier` may be a symbol, a string, a pathname, or nil.

## Locating the config file

There are several options for how to locate the config file. If you specify a pathname or a string (which will be turned into a pathname) as the identifier, that file will be used.

If you rather specify a symbol (or a keyword) as the identifier - lets say `:foobar` - modest will look for a file called `foobar.config`, first in the current directory and secondly in the current users home directory.

If the identifier is nil these three locations will be tried is sequence:

1. package.config in the current directory, where package is the name of the current package.
1. modest.config in the current directory.
1. package.config in the home directory of the current user.

All functions may invoke an error condition if a configuration file can't be located.
