;;; export of syntax referring to reexported syntax binding
(import reexport-m4)
(print (baz))

;;; reexport of renamed syntax
(import reexport-m6)
(f:s1)                ; expands to s2, which is reexported and refers to "s2", which is also visible in this context as "f:s2"
(f:s2)
