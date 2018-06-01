:- module('lp4', [ kakuroSolve/2 ]).
:- use_module('./beePlaygound/bee/bApplications/auxs/auxRunExpr',[runExpr/5, decodeIntArray/2]).
% Testing

%should pass
test_instance(1,Instance,Sol) :-
    Sol = [10=[6,4],9=[3,6]],
    Instance = [10=[I1,I2],9=[I3,I4]].

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
    writef('build_map_1: %w,\n',[C1]),flush_output,
    build_all_diff_constraint([Hint = VarBlock],C2),
    writef('build_map_2: %w,\n',[C2]),flush_output,
    append([C1],[C2],C3),
    build_map(Ti,Tm,C4),
    writef('build_map_3: %w,\n',[C4]),flush_output,
    append(C4,C3,Cs).

kakuroEncode(Instance,Map,Constraints) :-
    get_vars_no_dups(Instance,Vars),
    build_all_blocks(Vars,C0),
    build_map(Instance,Map,C1),
    writef('encode_3: %w,\n',[Map]),flush_output,
    writef('encode_4: %w,\n',[C0]),flush_output,
    writef('encode_5: %w,\n',[C1]),flush_output,
    append(C0,C1,Constraints).

% kakuroDecode(Map,Solution)


kakuroDecode(Map,Solution) :-
    writef('decode: %w,\n',[Map]),flush_output,
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

    

    
