:- module('lp4', [ kakuroSolve/2 ]).
:- use_module('./beePlaygound/bee/bApplications/auxs/auxRunExpr',[runExprMin/5,runExpr/5, decodeIntArray/2]).
% Testing

%should pass
test_instance(1,Instance,Sol) :-
    Sol = [10=[6,4],9=[3,6]],
    Instance = [10=[I1,I2],9=[I3,I4]].

test_instance(5,Instance,Solution) :-
    Solution = [1,2,3,4,5,6],
    Instance = schedule(6,[c(1,2),c(4,5)]).

%should fail
test_instance(2,Instance,Sol) :-
    Sol = [10=[6,4],9=[3,2]],
    Instance = [10=[I1,I2],9=[I3,I1]].

test_instance(3,Instance,Sol) :-
    Sol = [10=[5,5],9=[3,6]],
    Instance = [10=[I1,I2],9=[I3,I1]].

test_instance(4,Instance,Sol) :-
    Sol = [10=[4,6],9=[3,6]],
    Instance = [10=[I1,I2],9=[I3,I1]].

test_instance(6,Instance,Solution) :-
    Solution = [1,1,3,4,5,6],
    Instance = schedule(6,[c(1,2),c(4,5)]).

% test set
test_set(1) :-
    test_instance(1,I1,S1),
    kakuroVerify(I1,S1),
    test_instance(2,I2,S2),
    \+ kakuroVerify(I2,S2),
    test_instance(3,I3,S3),
    \+ kakuroVerify(I3,S3),
    test_instance(4,I4,S4),
    \+ kakuroVerify(I4,S4).

test_set(2) :-
    test_instance(5,I1,S1),
    schedulingVerify(I1,S1),
    test_instance(6,I2,S2),
    \+ schedulingVerify(I2,S2).

% test_run

test_run(1,S) :-
    schedulingSolve(schedule(4,[c(1,2)]),S).

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
    writef('%w,\n',[Instance]),flush_output,
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
    Map = map(Timeslots),
    build_all_timeslots(NExams,Timeslots,C0),
    build_conflict_pairs(Timeslots,Conflicts,C1),
    writef('%w,\n',[C0]),flush_output,
    writef('%w,\n',[C1]),flush_output,
    append(C0,C1,Constraints).


%schedulingDecode(Map,Solution) 

schedulingDecode(Map,Solution) :-
    bDecode:decodeIntArray(Map,Solution).

%schedulingSolve(Instance,Solution)

schedulingSolve(Instance,Solution) :-
    writef('%w,\n',[Instance]),flush_output,
    runExpr(Instance,Solution,
        lp4:schedulingEncode,
        lp4:schedulingDecode,
        lp4:schedulingVerify).