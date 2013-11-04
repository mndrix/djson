:- module(djson, [ json//1
                 , json_term/2
                 , is_like_json/1
                 ]
         ).

:- use_module(library(apply), [maplist/3]).
:- use_module(library(http/json)).
:- use_module(library(list_util)).


%% json_term(+Json:atom, -Term) is multi.
%% json_term(-Json:atom, +Term) is det.
%
%  True if Json is a serialization of Term in JSON notation. For single
%  use, Term can be a Prolog term with JSON-like notation. For
%  example,
%
%      Term = {
%          name: Name,
%          occupation: Job
%      }
%
%  json_term/2 considers it acceptable for Json to have extraneous
%  fields that are not present in Term. This allows one to pattern match
%  against a Json document without having to specify every imagineable
%  key.
%
%  When dealing repeatedly with the same terms and JSON structures, it's
%  most convenient to declare additional clauses for json//1.
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


%% json(?Term)//
%
%  Multifile hook for declaring a JSON-Prolog relation. When adding
%  clauses to this predicate, one typically calls json//1 recursively
%  with a JSON-like argument to describe the desired structure.
%
%  For example,
%
%      :- multifile djson:json//1.
%      djson:json(person(Name,Age)) -->
%          json({ name: Name, age: Age }).
:- multifile json//1.
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
    is_like_json(V).


%% is_like_json(+Term) is semidet.
%
%  True if Term is JSON-like. A JSON-like term is one that looks the
%  same as JSON notation.  For example,
%
%      ?- is_like_json({ hi: world }).
%      true.
%      ?- is_like_json([ a, b, 73 ]).
%      true.
%      ?- is_like_json(foo(_,_)).
%      false.
is_like_json(Atom) :-
    atom(Atom),
    !.
is_like_json(Number) :-
    number(Number),
    !.
is_like_json([]).
is_like_json({}).
is_like_json([H|T]) :-
    is_like_json(H),
    is_like_json(T).
is_like_json({_:V}) :-
    !,
    is_like_json(V).
is_like_json({_:V, Rest}) :-
    is_like_json(V),
    is_like_json({Rest}).
is_like_json(json(_)) :-
    !,
    fail.
