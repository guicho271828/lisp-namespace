;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

(setf
 ;; Package documentation
 (documentation (find-package :in-nomine) 't)
 "Utilities for defining additional namespaces in Common Lisp.

Common Lisp is a Lisp-N, which means that it has a different namespaces for
variables, functions, types, and so on. Users can also define their own
namespaces, and IN-NOMINE is a toolkit for making that process easier."

 ;; Defining and clearing namespaces
 (documentation 'define-namespace 'function)
 "Defines a new namespace object in the global namespace namespace along with
a series of functions, types, conditions, and type proclamations for accessing
this namespace.

Two forms of this macro are provided:
* short form:
  * (DEFINE-NAMESPACE NAME &OPTIONAL VALUE-TYPE BINDING DOCUMENTATION)
    * NAME - a symbol naming the namespace,
    * VALUE-TYPE - a type specifier for values bound in this namespace,
    * BINDING - deprecated, only present for syntax compatibility with
                LISP-NAMESPACE; must be NIL when provided,
    * DOCUMENTATION - documentation string for the namespace object.
* long form:
  * (DEFINE-NAMESPACE NAME
       &KEY NAME-TYPE VALUE-TYPE ACCESSOR CONDITION-NAME TYPE-NAME
            MAKUNBOUND-SYMBOL BOUNDP-SYMBOL DOCUMENTATION-TYPE
            ERROR-WHEN-NOT-FOUND-P ERRORP-ARG-IN-ACCESSOR-P
            DEFAULT-ARG-IN-ACCESSOR-P HASH-TABLE-TEST
            DOCUMENTATION)
    * NAME - a symbol naming the namespace,
    * NAME-TYPE - a type specifiers for keys bound in this namespace,
    * VALUE-TYPE - a type specifier for values bound in this namespace,
    * ACCESSOR - a symbol naming the accessor functions, or NIL if no such
                 accessor should be defined,
    * CONDITION-NAME - a symbol naming the condition type signaled when an
                       attempt is made to access an unbound name, or NIL if no
                       such accessor should be defined,
    * TYPE-NAME - a symbol naming the type for the namespace values, or NIL if
                  no such type should be defined,
    * MAKUNBOUND-SYMBOL - symbol naming the namespace makunbound function, or
                          NIL if no such function should be defined,
    * BOUNDP-SYMBOL - a symbol naming the namespace boundp function, or NIL if
                      no such function should be defined,
    * DOCUMENTATION-TYPE - a symbol naming the documentation type for the
                           namespace values, or NIL if no such documentation
                           should be defined,
    * ERROR-WHEN-NOT-FOUND-P - a boolean stating whether a reader function
                               should signal an error if it attempts to access
                               an unbound name,
    * ERRORP-ARG-IN-ACCESSOR-P - a boolean stating whether accessor functions
                                 should have an optional ERRORP argument for
                                 stating whether an unbound condition should be
                                 signaled when an attempt is made to access an
                                 unbound name,
    * DEFAULT-ARG-IN-ACCESSOR-P - a boolean stating whether accessor functions
                                  should have an optional DEFAULT argument for
                                  automatic setting of unbound values,
    * HASH-TABLE-TEST - a symbol naming the hash table test of the binding and
                        documentation hash tables of the namespace,
    * DOCUMENTATION - documentation string for the namespace object."
 (documentation 'clear-namespace 'function)
 "Get rid of all values bound in the given namespace."

 ;; Namespace class and accessors
 (documentation 'namespace 'type)
 "A namespace object, representing a Common Lisp namespace."
 (documentation 'namespace-name 'function)
 "Returns the symbol naming a namespace."
 (documentation 'namespace-name-type 'function)
 "Returns the type of names that are possible to bind in a namespace."
 (documentation 'namespace-value-type 'function)
 "Returns the type of values that are possible to bind in a namespace."
 (documentation 'namespace-accessor 'function)
 "Returns the symbol naming the namespace accessor, or NIL if no such accessor
is defined."
 (documentation 'namespace-condition-name 'function)
 "Returns the symbol naming the condition type signaled when an attempt is made
to access an unbound name, or NIL if no such condition type is defined"
 (documentation 'namespace-type-name 'function)
 "Returns the symbol naming the type for the namespace values, or NIL if no such
type is defined."
 (documentation 'namespace-makunbound-symbol 'function)
 "Returns the symbol naming the namespace makunbound function, or NIL if no such
function exists."
 (documentation 'namespace-boundp-symbol 'function)
 "Returns the symbol naming the namespace boundp function, or NIL if no such
function exists."
 (documentation 'namespace-documentation-type 'function)
 "Returns the symbol naming the documentation type for the namespace values, or
NIL if no such documentation type exists."
 (documentation 'namespace-error-when-not-found-p 'function)
 "Returns a boolean stating whether a reader function should signal an error if
it attempts to access an unbound name."
 (documentation 'namespace-errorp-arg-in-accessor-p 'function)
 "Returns a boolean stating whether accessor functions should have an optional
ERRORP argument for stating whether an unbound condition should be signaled when
an attempt is made to access an unbound name."
 (documentation 'namespace-default-arg-in-accessor-p 'function)
 "Returns a boolean stating whether accessor functions should have an optional
DEFAULT argument for automatic setting of unbound values."
 (documentation 'namespace-hash-table-test 'function)
 "Returns the symbol naming the hash table test of the binding and documentation
hash tables of the namespace."
 (documentation 'namespace-binding-table 'function)
 "Returns the binding hash table, or NIL if no binding mechanism is defined."
 (documentation 'namespace-documentation-table 'function)
 "Returns the documentation hash table, or NIL if no documentation type is
defined."

 ;; Metanamespace
 (documentation 'namespace 'namespace)
 "A namespace for managing namespaces.")
