:- module(djson, [ json//1
                 , json_term/2
                 ]
         ).

:- use_module(library(apply), [maplist/3]).
:- use_module(library(dcg/basics)).
:- use_module(library(delay)).
:- use_module(library(http/json)).
:- use_module(library(list_util)).


:- multifile delay:mode/1.
delay:mode(system:atom(nonvar)).
delay:mode(system:integer(nonvar)).
delay:mode(system:float(nonvar)).
delay:mode(apply:maplist(_,list,_)).
delay:mode(apply:maplist(_,_,list)).

delay:mode(list_util:xfy_list(_,ground,_)).
delay:mode(list_util:xfy_list(_,_,list)).


json_term(Json, Term) :-
    ( ground(Json) ->
        atom_json_term(Json, JsonTerm, []),
        json(Term, JsonTerm, _)
    ; true -> % assume Term is ready to convert
        json(Term, JsonTerm, _),
        finalize(JsonTerm),
        is_json_term(JsonTerm, []),
        !,
        atom_json_term(Json, JsonTerm, [as(atom)])
    ).


% convert difference lists into proper lists, recursively inside a
% library(http/json) term.
finalize(X) :-
    var(X),
    !,
    X=[].
finalize(json(J)) :-
    !,
    finalize(J).
finalize([_Key=Value|T]) :-
    !,
    finalize(Value),
    finalize(T).
finalize([H|T]) :-
    !,
    finalize(H),
    finalize(T).
finalize(_).


% term format that looks more like JSON notation
json({}, json([]), _).
json({Pairs}, json(J0), json(J)) :-
    ( var(Pairs) ->
        maplist(eq_colon,J0,Pairs0),
        xfy_list(',', Pairs, Pairs0),
        J = []
    ; Pairs=(Key:Value0) ->
        once(select(Key=Value, J0, J)),
        json(Value0, Value, _)
    ; Pairs=(Key:Value0,Rest) ->
        once(select(Key=Value,J0,J1)),
        json(Value0, Value, _),
        json({Rest}, json(J1), json(J))
    ).
json(X,X,_).


eq_colon(K=V0,K:V) :-
    json(V, V0, _),
    once(is_pretty_json(V)).


% true if argument is a term that looks like JSON-notation
is_pretty_json([]).
is_pretty_json({}).
is_pretty_json([H|T]) :-
    is_pretty_json(H),
    is_pretty_json(T).
is_pretty_json({_:V}) :-
    is_pretty_json(V).
is_pretty_json({_:V, Rest}) :-
    is_pretty_json(V),
    is_pretty_json({Rest}).
is_pretty_json(json(_)) :-
    !,
    fail.
is_pretty_json(Atom) :-
    atom(Atom).
is_pretty_json(Number) :-
    number(Number).
