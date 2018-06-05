% We used this file to test our code during implementation

%should pass
test_instance(1,Instance,Sol) :-
    Sol = [10=[6,4],9=[3,6]],
    Instance = [10=[I1,I2],9=[I3,I4]].

test_instance(5,Instance,Solution) :-
    Solution = [1,2,3,4,5,6],
    Instance = schedule(6,[c(1,2),c(4,5)]).

test_instance(7,Instance,Solution) :-
    Solution = [3,1,1,1,2,1],
    Instance = schedule(10,[c(1,3),c(5,6),c(1,6),c(1,5)]).

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
