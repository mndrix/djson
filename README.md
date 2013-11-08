# Synopsis

    :- use_module(library(djson)).

    :- multifile djson:json//1.
    djson:json(point(X,Y)) -->
        json({ x: X, y: Y }).

    ?- json_term(Json, point(1,2)).
    Json = '{"x":1, "y":2}'.

    ?- json_term('{"y":2, "x":1}', Term).
    Term = {y:2, x:1} ;
    Term = json([y=2, x=1]) ;
    Term = point(1, 2) ;
    false.

    ?- json_term( '{"name":{"first":"John","last":"Doe"},"age":27}'
                , { name: { first: Name } }
                ).
    Name = 'John' ;
    false.

# Description

Easy conversion between JSON atoms and Prolog terms.  Because the notation is nearly identical to JSON, one can copy and paste examples from API documentation, add variables and immediately have working code.  The library also makes it easy to describe complex, nested JSON objects.

We also recognize the inherent ambiguity of JSON serialization.  Because JSON rarely indicates an object's type, we can't be certain which Prolog term represents it best.  Instead of making you choose one, library(djson) bactracks over possible representations.  This allows the context inherent in your code to clarify which representation is wanted.

It's common for web services to return JSON with a great deal of extraneous information.  We rarely care about all of it.  This library makes it easy to extract the pieces you want.  The last example in the Synopsis above extracts a person's name from inside a nested JSON object.  This pattern allows one to reuse predicates across different JSON documents, as long as they share the same structure.

# See Also

Both library(http/json) and library(http/json_convert) facilitate conversion between JSON and Prolog terms.

# Changes in this Version

  * Support nested arrays

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(djson).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/djson

@author Michael Hendricks <michael@ndrix.org>
@license BSD
