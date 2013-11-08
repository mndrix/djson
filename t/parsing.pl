:- use_module(library(djson)).

:- multifile djson:json//1.
djson:json(point(X,Y)) -->
    json({ x: X, y: Y }).

djson:json(city(Name,MayorName,MayorAge)) -->
    json({ name: Name
         , mayor: { name: MayorName
                  , age: MayorAge
                  }
         }).

djson:json(person(Name,Age)) -->
    json({ name: Name
         , age: Age
         }).

djson:json(city(Name,Mayor)) -->
    { Mayor = person(_,_) },
    json({ name: Name
         , mayor: Mayor
         }).

:- use_module(library(tap)).

'parsing compact JSON object' :-
    setof( X
         , json_term('{"x":1,"y":2}', X)
         , Xs
         ),
    Xs == [ json([x=1,y=2])
          , {x:1, y:2}
          , point(1,2)
          ].


'parsing prettier JSON object' :-
    setof( X
         , json_term('{ "x": 1\n, "y" : 2 }\n', X)
         , Xs
         ),
    Xs == [ json([x=1,y=2])
          , {x:1, y:2}
          , point(1,2)
          ].


'parsing nested JSON object' :-
    setof( X
         , json_term('{"name":"Burg","mayor":{"name":"John","age":53}}', X)
         , Xs
         ),
    Xs == [ json([name='Burg',mayor=json([name='John',age=53])])
          , { name:'Burg'
            , mayor: { name: 'John'
                     , age: 53
                     }
            }
          , city('Burg', person('John',53))
          , city('Burg', 'John', 53)
          ].


'parsing JSON object with extra keys' :-
    setof( X
         , json_term('{ "z":3, "y":2, "x":1 }', X)
         , Xs
         ),
    Xs == [ json([z=3,y=2,x=1])
          , { z:3, y:2, x:1 }
          , point(1, 2)    % by ignore the "z" key
          ].

'parsing compact JSON array' :-
    setof( X
         , json_term('["a",2,"c",4.0]', X)
         , Xs
         ),
    Xs == [ [a,2,c,4.0]
          ].


'parsing prettier JSON array' :-
    setof( X
         , json_term('[ "a", 2, "c", 4.0 ]', X)
         , Xs
         ),
    Xs == [ [a,2,c,4.0]
          ].

'parsing nested JSON array' :-
    setof( X
         , json_term('[{"x":1, "y":2}, {"x":3, "y":4}]', X)
         , Xs
         ),

    % there are dozens of legitimate combinations.
    % check a few of the most important ones.
    memberchk([json([x=1,y=2]),json([x=3,y=4])], Xs),
    memberchk([{x:1,y:2},{x:3,y:4}], Xs),
    memberchk([point(1,2), point(3,4)], Xs).
