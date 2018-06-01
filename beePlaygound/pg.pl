:- module('pg', [ pg/2 ]).
:- use_module('./bee/bApplications/auxs/auxRunExpr',[runExprMax/5, decodeInt/2]).


pg(Instanace, Solution) :-
    runExprMax(Instanace, Solution,
        pg:encode,
        pg:decode,
        pg:verify).

encode(X, Map, M, [new_int(X, 1, 4), new_int(M, 1, 4)]) :-
    Map = map(X, M).

decode(map(X, M), Solution) :-
    decodeInt(X, Solution),
    decodeInt(M, Solution).

verify(Instance, Sol).
% verify(Instance, Sol) :-
%     writeln(Instance),
%     writeln(Sol),
%     Sol < 5,
%     Sol > 5.

