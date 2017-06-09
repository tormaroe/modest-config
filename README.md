***MODEST-CONFIG*** is a simple configuration file parser for Common Lisp. Config files contain a single Common Lisp property list, and the parser provides a convenient way to extract the properties.

![Modest](https://media.giphy.com/media/l0HlvLn1tHxNNZQuA/giphy.gif)

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

**with-config** identifier bindings &body body => result, plist

`identifier` may be a symbol, a string, a pathname, or nil.

Note that with-config returns the result of evaluating the last expression of `body` as it's first value, and the entire configuration property list as it's second value.

## Locating the config file

COnfiguration will be loaded based on the following rules:

| Identifier type       | What will happen?                                                                                                                                                                                                                                                               |
|-----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| pathname              | File designated by pathname is assumed to contain configuration.                                                                                                                                                                                                                |
| string                | Will be turned into a pathname and handled as such.                                                                                                                                                                                                                             |
| symbol                | If identifier is `'foobar` or `:foobar` modest-config will look for a file called `foobar.config`, first in the current directory and secondly in the current users home directory.                                                                                                          |
| list                  | Is assumed to be a property list containing the actual configuration. This will fail if passed to `find-config`.                                                                                                                                                                |
| stream                | Config will be read from stream. This will fail if passed to `find-config`.                                                                                                                                                                                                     |
| null                  | If the identifier is nil these three locations will be tried is sequence: 1) Package.config in the current directory, where package is the name of the current package. 2) modest.config in the current directory. 3) package.config in the home directory of the current user. |

All functions may invoke an error condition if a configuration file can't be located.

## License

Copyright 2017 Torbjørn Marø

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.