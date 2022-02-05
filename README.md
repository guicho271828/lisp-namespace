## LISP-NAMESPACE

This is a utility for creating, accessing, and managing custom namespaces in Common Lisp, currently implemented in terms of [In Nomine](https://github.com/phoe/in-nomine).

This version has the macros `NAMESPACE-LET` and `NSLET` removed, as they were incomplete and buggy in the previous version.

## Manual

A namespace is a second-class concept in Common Lisp and refers to concept that allows to associate names of some sort with objects of some sort.

Common Lisp has a lot of namespaces whose keys can come in various shapes:

* variables and symbol macros (symbols),
* functions anc macros (symbols),
* compiler macros (symbols),
* classes, conditions, and types (symbols),
* slot names (symbols),
* method combinations (symbols),
* block names (symbols),
* tagbody tags (symbols and integers)
* catch tags (any Lisp objects),
* restarts (symbols),
* packages (strings),
* modules (strings),
* logical pathname hosts (strings),
* reader macros (characters),
* character codes (characters),
* character names (strings),
* characters *(only true on some implementations)* (integers),
* ...
* ASDF systems (lowercase strings),
* ... (people can make new ones!)

This system is a utility to bring a first-class implementation of the concept of namespaces along with utilities to customize and manage them.

The heart of the facility is the `DEFINE-NAMESPACE` macro, which generates functions for accessing the namespace, a condition signaled whenever an access to an unbound name is attempted, a type which denotes the values permissible in a namespace, and documentation types, as well as two variables for hash tables storing bindings and documentation.

Namespace names are symbols and compared via `EQ`. This behavior is consistent with the way Common Lisp names variables and classes.

```lisp
LISP-NAMESPACE> (define-namespace thing)
#<NAMESPACE THING (0 bindings)>

LISP-NAMESPACE> (setf (symbol-thing 'foo) 42
                      (symbol-thing 'bar) :keyword 
                      (symbol-thing 'baz) *readtable*)
#<READTABLE {1000022CA3}>

LISP-NAMESPACE> (mapcar #'symbol-thing '(foo bar baz))
(42 :KEYWORD #<READTABLE {1000022CA3}>)
```

`LISP-NAMESPACE` provides documentation types with the same names as namespace names.

```lisp
LISP-NAMESPACE> (setf (documentation 'foo 'thing) "The best thing ever.")
"The best thing ever."

LISP-NAMESPACE> (documentation 'foo 'thing)
"The best thing ever."
```

In addition, `LISP-NAMESPACE` hooks into implementation-defined `CL:DESCRIBE` in order to provide information about namespace bindings.

```lisp
LISP-NAMESPACE> (describe 'foo)
LISP-NAMESPACE::FOO
  [symbol]

Symbol FOO is bound in namespace THING:
  Value: 42
  (undocumented)
; No value
```

## Compatibility with `IN-NOMINE`

This version of `LISP-NAMESPACE` is implemented via In Nomine, which implements a more configurable namespacing facility. The main differences between the two are:

* Package `LISP-NAMESPACE` exports only two symbols, `DEFINE-NAMESPACE` and `CLEAR-NAMESPACE`,
* `LISP-NAMESPACE:DEFINE-NAMESPACE` supports only the short form of In Nomine's `DEFINE-NAMESPACE`,
* `LISP-NAMESPACE:DEFINE-NAMESPACE` generates no makunbound symbol,
* `LISP-NAMESPACE:DEFINE-NAMESPACE` sets default values for generated hash table variables - for a namespace named `FOO` these are:
  * `*FOO-TABLE*` for a binding table,
  * `*FOO-DOC-TABLE*` for a documentation table.

## API

### Packages

#### Package `LISP-NAMESPACE`

Loaded via `(asdf:load-system :lisp-namespace)`.

Utilities for defining additional namespaces in Common Lisp.

Common Lisp is a Lisp-N, which means that it has a different namespaces for variables, functions, types, and so on. Users can also define their own namespaces, and `LISP-NAMESPACE` is a toolkit for making that process easier.

Implemented via In Nomine.

### Namespace definition and management

#### Macro `DEFINE-NAMESPACE`

Defines a new namespace object in the global namespace namespace along with
a series of functions, types, conditions, and type proclamations for accessing
this namespace.

The syntax of this macro is the following:
* `(DEFINE-NAMESPACE NAME &OPTIONAL VALUE-TYPE BINDING DOCUMENTATION)`
  * `NAME` - a symbol naming the namespace,
  * `VALUE-TYPE` - a type specifier for values bound in this namespace,
  * `BINDING` - deprecated; must be `NIL` when provided,
  * `DOCUMENTATION` - documentation string for the namespace object.
* For name `FOO`, the following are generated:
  * Accessor functions `SYMBOL-FOO` and `(SETF SYMBOL-FOO)`,
  * Boundp function `FOO-BOUNDP`,
  * Type proclamations for the four functions above,
  * Condition type `UNBOUND-FOO`,
  * Type `FOO-TYPE` denoting the specified `VALUE-TYPE`,
  * Documentation methods with documentation type specialized on `(EQL 'FOO)`,
  * Variable `*FOO-TABLE*` whose value is the binding table,
  * Variable `*FOO-DOC-TABLE*` whose value is the documentation table.

#### Function `CLEAR-NAMESPACE`

Removes all bindings in the namespace with the given name.

## License

* Copyright (c) 2015 [Masataro Asai](guicho2.71828@gmail.com)
* Copyright (c) 2022 [Michał "phoe" Herda](phoe@disroot.org)

Licensed under the LLGPL License.
