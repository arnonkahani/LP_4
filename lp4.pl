:- module('lp4', [ kakuroSolve/2, schedulingSolve/2 ]).
:- use_module('./beePlaygound/bee/bApplications/auxs/auxRunExpr',[runExprMin/5,runExpr/5, decodeIntArray/2]).
# :- use_module('/home/niv/Programs/plsatsolver_src/satsolver/satsolver').

%kakuroVerify(Instance, Solution)

% We assume that the instance and solution are ordered in the same manner.

kakuroVerify([],[]).

kakuroVerify([Hint = VarBlock |Ti], [Hint = SolBlock|Ts]) :-
    length(VarBlock,N),
    length(SolBlock,N),
    sum_list(SolBlock,Hint),
    sort(SolBlock,NoDup),
    length(NoDup,N), 
    VarBlock = SolBlock,
    kakuroVerify(Ti,Ts).

%kakuroEncode(Instance,Map,Constraints).

build_sum_constraint([Hint = VarBlock],int_array_plus(VarBlock,Hint)).

build_all_diff_constraint([_ = VarBlock],int_array_allDiff(VarBlock)).

build_all_blocks([],[]).

build_all_blocks([Mh|Mt],[new_int(Mh,1,9)|Cs]) :-
    build_all_blocks(Mt,Cs).


get_vars_no_dups(I,Vars) :-
    flatten(I,[],F), % flatten vars
    list_to_set(F,Vars). % remove duplicates from flatten list of vars

flatten([],F,F).

flatten([_=I|R],F,Fn) :-
    append(I,F,Ft),
    flatten(R,Ft,Fn).

build_map([],[],[]).

build_map([Hint = VarBlock | Ti],[Hint = VarBlock | Tm],Cs) :-
    build_sum_constraint([Hint = VarBlock],C1),
    build_all_diff_constraint([Hint = VarBlock],C2),
    append([C1],[C2],C3),
    build_map(Ti,Tm,C4),
    append(C4,C3,Cs).

kakuroEncode(Instance,Map,Constraints) :-
    get_vars_no_dups(Instance,Vars),
    build_all_blocks(Vars,C0),
    build_map(Instance,Map,C1),
    append(C0,C1,Constraints).

% kakuroDecode(Map,Solution)


kakuroDecode(Map,Solution) :-
    decodeK(Map,Solution).

decodeK([],[]).

decodeK([Hint = Mblock | Tm],[Hint = Sdecoded | Ts]) :-
    bDecode:decodeIntArray(Mblock,Sdecoded),
    decodeK(Tm,Ts).


% kakuroSolve(Instance,Solution)

kakuroSolve(Instance,Solution) :-
    runExpr(Instance,Solution,
        lp4:kakuroEncode,
        lp4:kakuroDecode,
        lp4:kakuroVerify).
    
%% schedulingVerify(Instance, Solution) 

verify_conflicts([],_).

verify_conflicts([c(I,J)|T],Solution) :-
    I < J,
    nth1(I,Solution,T1),
    nth1(J,Solution,T2),
    T1 \== T2,
    verify_conflicts(T,Solution).

schedulingVerify(Instance, Solution) :-
    Instance = schedule(NExams, Conflicts),
    NExams > 0,
    length(Solution,NExams),
    verify_conflicts(Conflicts,Solution).


% schedulingEncode(Instance,Map,Constraints).

build_all_timeslots(_,[],[]).

build_all_timeslots(N,[Mh|Mt],[new_int(Mh,1,N)|Cs]) :-
    build_all_timeslots(N,Mt,Cs).

build_conflict_pairs(_,[],[]).

build_conflict_pairs(Timeslots,[c(I,J)|Tc],[C|Cs]) :-
    I<J,
    nth1(I,Timeslots,T1),
    nth1(J,Timeslots,T2),
    C = int_neq(T1,T2),
    build_conflict_pairs(Timeslots,Tc,Cs).

schedulingEncode(Instance,Map,Constraints) :-
    Instance = schedule(NExams, Conflicts),
    NExams > 0,
    length(Timeslots,NExams),
    Map = Timeslots,                                %Create the map
    build_all_timeslots(NExams,Timeslots,C0),       %All timeslots are numbers from 1 to Nexams
    build_conflict_pairs(Timeslots,Conflicts,C1),
    append(C0,C1,Constraints).

%schedulingDecode(Map,Solution)

schedulingDecode(Map,Solution) :-
    bDecode:decodeIntArray(Map,Solution).

%schedulingSolve(Instance,Solution)

schedulingSolve(Instance,Solution) :-
    runExpr(Instance,Solution,
        lp4:schedulingEncode,
        lp4:schedulingDecode,
        lp4:schedulingVerify).