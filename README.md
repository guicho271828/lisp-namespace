## In Nomine

> The beginning of wisdom is to call things by their proper name.
> 
> -- Confucius

This is a utility for creating, accessing, and managing custom namespaces in Common Lisp. Originally started as a fork of [`LISP-NAMESPACE`](https://github.com/guicho271828/lisp-namespace), it became its own piece of software that is *somewhat* backwards compatible with it.

## Manual

A namespace is a second-class concept in Common Lisp and refers to concept that allows to associate names of some sort with objects of some sort.

Common Lisp has a lot of namespaces:
* 1) variables/symbol macros
* 2) functions/macros
* 3) classes/conditions/types
* 4) method combinations
* 5) block names
* 6) catch tags
* 7) tagbody tags
* 8) restarts
* 9) packages
* 10) compiler macros
* 11) slot names 
* ...
* n) ASDF systems
* n+1) ... (people can make new ones!)

This system is a utility to bring a first-class implementation of the concept of namespaces along with utilities to customize and manage them.

The heart of the facility is the `DEFINE-NAMESPACE` macro, which comes in two forms: short (syntax-compatible with [`LISP-NAMESPACE:DEFINE-NAMESPACE`](https://github.com/guicho271828/lisp-namespace) and with mostly compatible effects) and long form (allowing for greater behavior customization). `DEFINE-NAMESPACE`, by default, generates functions for accessing the namespace, a condition signaled whenever an access to an unbound name is attempted, a type which denotes the values permissible in a namespace, and documentation types.

By default, names are symbols and compared via `EQ`. This behavior is consistent with the way Common Lisp names variables and classes.

```lisp
IN-NOMINE> (define-namespace thing)
#<NAMESPACE THING (0 bindings)>

IN-NOMINE> (setf (symbol-thing 'foo) 42
                 (symbol-thing 'bar) :keyword 
                 (symbol-thing 'baz) *readtable*)
#<READTABLE {1000022CA3}>

IN-NOMINE> (mapcar #'symbol-thing '(foo bar baz))
(42 :KEYWORD #<READTABLE {1000022CA3}>)
```

It is possible to customize this behavior, though, and get e.g. a namespace in which names are non-negative numbers.

```lisp
IN-NOMINE> (define-namespace player
             ;; Use numbers as hash table keys
             :name-type unsigned-byte
             ;; Numbers are EQL-comparable
             :hash-table-test eql
             :accessor player-no)
#<NAMESPACE PLAYER (0 bindings)>

IN-NOMINE> (setf (player-no 8) :jerry 
                 (player-no 0) :thomas 
                 (player-no 2) :michael)
:MICHAEL

IN-NOMINE> (player-no 8)
:JERRY

IN-NOMINE> (player-no 1)
;; Error: Name 1 is unbound in namespace PLAYER.
;;     [Condition of type UNBOUND-PLAYER]
```

It is possible to utilize different name types along with all four standard hash table keys and produce namespaces with different possible name values. Examples:
* `EQ` for symbols,
* `EQL` for numbers and characters,
* `EQUAL` for strings or lists,
* `EQUALP` for strings without case sensitivity.

In Nomine by default provides documentation types with the same names as namespace names.

```lisp
IN-NOMINE> (setf (documentation 8 'player) "The best player ever.")
"The best player ever."

IN-NOMINE> (documentation 8 'player)
"The best player ever."
```

In addition, In Nomine hooks into implementation-defined `CL:DESCRIBE` in order to provide information about namespace bindings.

```lisp
IN-NOMINE> (describe 'foo)
IN-NOMINE::FOO
  [symbol]

Symbol FOO is bound in namespace THING:
  Value: 42
  (undocumented)
; No value

IN-NOMINE> (describe 8)
8
  [fixnum]

8 is bound in namespace PLAYER:
  Value: :JERRY
  Documentation:
    The best player ever.
; No value
```

## API

### Packages

#### Package `IN-NOMINE`

Utilities for defining additional namespaces in Common Lisp.

Common Lisp is a Lisp-N, which means that it has a different namespaces for variables, functions, types, and so on. Users can also define their own namespaces, and IN-NOMINE is a toolkit for making that process easier.

### Namespace definition and management

#### Macro `DEFINE-NAMESPACE`

Defines a new namespace object in the global namespace namespace along with
a series of functions, types, conditions, and type proclamations for accessing
this namespace.

Two forms of this macro are provided:
* short form:
  * `(DEFINE-NAMESPACE NAME &OPTIONAL VALUE-TYPE BINDING DOCUMENTATION)`
    * `NAME` - a symbol naming the namespace,
    * `VALUE-TYPE` - a type specifier for values bound in this namespace,
    * `BINDING` - deprecated, only present for syntax compatibility with [`LISP-NAMESPACE`](https://github.com/guicho271828/lisp-namespace); must be `NIL` when provided,
    * `DOCUMENTATION` - documentation string for the namespace object.
  * For name `FOO`, the following are generated:
    * Accessor functions `SYMBOL-FOO` and `(SETF SYMBOL-FOO)`,
    * Makunbound function `FOO-MAKUNBOUND`,
    * Boundp function `FOO-BOUNDP`,
    * Type proclamations for the four functions above,
    * Condition type `UNBOUND-FOO`,
    * Type `FOO-TYPE` denoting the specified `VALUE-TYPE`,
    * Documentation methods with documentation type specialized on `(EQL 'FOO)`.
* long form:
  * `(DEFINE-NAMESPACE NAME &KEY NAME-TYPE VALUE-TYPE ACCESSOR CONDITION-NAME TYPE-NAME MAKUNBOUND-SYMBOL BOUNDP-SYMBOL DOCUMENTATION-TYPE ERROR-WHEN-NOT-FOUND-P ERRORP-ARG-IN-ACCESSOR-P DEFAULT-ARG-IN-ACCESSOR-P HASH-TABLE-TEST DOCUMENTATION)`
    * `NAME` - a symbol naming the namespace,
    * `NAME-TYPE` - a type specifiers for keys bound in this namespace,
    * `VALUE-TYPE` - a type specifier for values bound in this namespace,
    * `ACCESSOR` - a symbol naming the accessor functions, or `NIL` if no such accessor should be defined,
    * `CONDITION-NAME` - a symbol naming the condition type signaled when an attempt is made to access an unbound name, or `NIL` if no such accessor should be defined,
    * `TYPE-NAME` - a symbol naming the type for the namespace values, or `NIL` if no such type should be defined,
    * `MAKUNBOUND-SYMBOL` - symbol naming the namespace makunbound function, or `NIL` if no such function should be defined,
    * `BOUNDP-SYMBOL` - a symbol naming the namespace boundp function, or `NIL` if no such function should be defined,
    * `DOCUMENTATION-TYPE` - a symbol naming the documentation type for the namespace values, or `NIL` if no such documentation should be defined,
    * `ERROR-WHEN-NOT-FOUND-P` - a boolean stating whether a reader function should signal an error if it attempts to access an unbound name,
    * `ERRORP-ARG-IN-ACCESSOR-P` - a boolean stating whether accessor functions should have an optional `ERRORP` argument for stating whether an unbound condition should be signaled when an attempt is made to access an unbound name,
    * `DEFAULT-ARG-IN-ACCESSOR-P` - a boolean stating whether accessor functions should have an optional `DEFAULT` argument for automatic setting of unbound values,
    * `HASH-TABLE-TEST` - a symbol naming the hash table test of the binding and documentation hash tables of the namespace,
    * `DOCUMENTATION` - documentation string for the namespace object.

The consequences are undefined if a namespace is redefined in an incompatible
way with the previous one.

#### Function `SYMBOL-NAMESPACE`

Returns a namespace object with the given global name. Signals `UNBOUND-NAMESPACE` unless `ERRORP` is set.

#### Function `CLEAR-NAMESPACE`

Removes all bindings in the namespace with the given name.

#### Function `NAMESPACE-MAKUNBOUND`

Makes the name globally unbound as a namespace regardless of whether the name was previously bound.

#### Function `NAMESPACE-BOUNDP`

Returns true if a namespace object with the provided name is globally bound, false otherwise.

#### Condition Type `UNBOUND-NAMESPACE`

A subtype of `CELL-ERROR` signaled when there is an attempt to access a namespace object that does not exist.

### Namespace class and accessors

#### Class `NAMESPACE`

A class of namespace objects which represent a Common Lisp namespace.

#### Function `NAMESPACE-NAME`

Returns the symbol naming a namespace.

#### Function `NAMESPACE-NAME-TYPE`

Returns the type of names that are possible to bind in a namespace.

#### Function `NAMESPACE-VALUE-TYPE`

Returns the type of values that are possible to bind in a namespace.

#### Function `NAMESPACE-ACCESSOR`

Returns the symbol naming the namespace accessor, or `NIL` if no such accessor is defined.

#### Function `NAMESPACE-CONDITION-NAME`

Returns the symbol naming the condition type signaled when an attempt is made to access an unbound name, or `NIL` if no such condition type is defined

#### Function `NAMESPACE-TYPE-NAME`
Returns the symbol naming the type for the namespace values, or `NIL` if no such type is defined.

#### Function `NAMESPACE-MAKUNBOUND-SYMBOL`
Returns the symbol naming the namespace makunbound function, or `NIL` if no such function exists.

#### Function `NAMESPACE-BOUNDP-SYMBOL`
Returns the symbol naming the namespace boundp function, or `NIL` if no such function exists.

#### Function `NAMESPACE-DOCUMENTATION-TYPE`

Returns the symbol naming the documentation type for the namespace values, or `NIL` if no such documentation type exists.

#### Function `NAMESPACE-ERROR-WHEN-NOT-FOUND-P`

Returns a boolean stating whether a reader function should signal an error if it attempts to access an unbound name.

#### Function `NAMESPACE-ERRORP-ARG-IN-ACCESSOR-P`

Returns a boolean stating whether accessor functions should have an optional `ERRORP` argument for stating whether an unbound condition should be signaled when an attempt is made to access an unbound name.

#### Function `NAMESPACE-DEFAULT-ARG-IN-ACCESSOR-P`

Returns a boolean stating whether accessor functions should have an optional `DEFAULT` argument for automatic setting of unbound values.

#### Function `NAMESPACE-HASH-TABLE-TEST`

Returns the symbol naming the hash table test of the binding and documentation hash tables of the namespace.

#### Function `NAMESPACE-BINDING-TABLE`

Returns the binding hash table, or `NIL` if no binding mechanism is defined.

#### Function `NAMESPACE-DOCUMENTATION-TABLE`

Returns the documentation hash table, or `NIL` if no documentation type is defined.

### Namespaces

#### Namespace `NAMESPACE`

A namespace for managing namespaces.

## License

* Copyright (c) 2015 [Masataro Asai](guicho2.71828@gmail.com)
* Copyright (c) 2022 [Michał "phoe" Herda](phoe@disroot.org)

Licensed under the LLGPL License.

> A man that should call everything by its right name, would hardly pass the streets without being knocked down as a common enemy.
>
> -- Lord Halifax
