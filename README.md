MODEST-CONFIG is a simple config file parser for Common Lisp. Config files contain a single Common Lisp property list, and the parser provides a convenient way to extract the properties.

Given a config file:

```
(
  ;; This sets property FOO
  foo :some-value

  ;; Here is another property
  bar "This is a string"

  zot (list 1 2 3 4 5)
)
```

You may load and extract properties like this:

```
(with-config "my.config" (zot bar)
  (format t "The value of zot is ~s~%" zot)
  (format t "~a says bar~%" bar))
```

Output:

```
The value of zot is (1 2 3 4 5)
This is a string says bar
```

I will add more details about how config file is located and about other exported functions later...