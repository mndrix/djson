:- use_module(library(djson)).
:- use_module(library(delay)).

:- multifile delay:mode/1.
delay:mode(system:atom(nonvar)).

:- multifile djson:json//1.
djson:json(point(X,Y)) -->
    json({ x: X, y: Y }).
djson:json(person(First,Last)) -->
    { delay(atom(First)) },
    { delay(atom(Last)) },
    json([First, Last]).

:- use_module(library(tap)).

'serializing JSON object (native)' :-
    json_term( Json, {x:1, y:2} ),
    Json == '{"x":1, "y":2}'.

'serializing JSON object (http_json)' :-
    json_term( Json, json([x=1,y=2]) ),
    Json == '{"x":1, "y":2}'.

'serializing JSON object (compound term)' :-
    json_term( Json, point(4,3) ),
    Json == '{"x":4, "y":3}'.


'serializing JSON array (native)' :-
    json_term(Json, [a,2,c,4.0]),
    Json == '["a", 2, "c", 4.0 ]'.

'serializing JSON array (compound term)' :-
    json_term(Json, person(john, doe)),
    Json == '["john", "doe" ]'.
