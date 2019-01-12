range(Low, Low, _).
range(Out,Low,High) :-
    NewLow is Low+1,
    NewLow < High,
    range(Out, NewLow, High).

%Znajduje w liście stanów struktury typu clear(), które spełniają warunek - move(X,Y,Z), diff(Z,X/on(X,Y))
find_clear_elements([], _, []).
find_clear_elements([clear(Y)|State], move(A,_,_), [clear(Y)|OutList]) :-
    A \= Y, !,
    find_clear_elements(State, move(A,_,_), OutList).
find_clear_elements([_|State], move(A,_,_), OutList) :-
    find_clear_elements(State, move(A,_,_), OutList).

member1(X,[X|_]).
member1(X,[_|Rest]) :-
    member1(X,Rest).

intersect([], _, []).
intersect([Elem|RestL], L, [Elem|Result]) :-
	member1(Elem, L), !,
	intersect(RestL, L, Result).
intersect([_|RestL], L, Result) :-
    intersect(RestL, L, Result).

substract1(_,[],[]).
substract1(X,[X|Rest],Result) :-
    substract1(X,Rest,Result).
substract1(X,[Y|Rest],[Y|Result]) :-
    X \= Y,
    substract1(X,Rest,Result).

substract_all([],OldList,OldList).
substract_all([X|Rest],OldList,NewList) :-
    substract1(X,OldList,Temp),
    substract_all(Rest,Temp,NewList).

%Dla celów, gdzie X i Y są już ukonkretnione (bez warunków), sprawdzamy czy nie są strukturą (compound)
goal_achieved(on(X,Y), State) :-
    \+ compound(Y),
    member1(on(X,Y), State).
goal_achieved(clear(X), State) :-
    \+ compound(X),
    member1(clear(X), State).
goal_achieved(clear(X/on(X,Y)), State) :-
    goal_achieved(on(X,Y), State),
    member1(clear(X), State).
%Zmienna Z, czyli miejsce, gdzie przenosimy, nie może mieć warunków
goal_achieved(on(X,Y/on(Y,Z)), State) :-
    \+ compound(Z),
    goal_achieved(on(Y,Z), State),
    member1(on(X,Y), State).
goal_achieved(diff(Z,X/on(X,Y)), State) :-
    \+ compound(Z),
    goal_achieved(on(X,Y), State),
    Z \= X.

%jeśli nie ma żadnych celów, to znaczy, że są spełnione
goals_achieved([], _).
%sprawdzamy każdy element listy celów, czy znajduje się też w liście stanu, chcemy uniknąć powtórzeń w liście celów
goals_achieved([X|Goals], State) :-
    goals_achieved(Goals, State),
    goal_achieved(X,State),
    \+member(X,Goals).

%wybieramy element który należy listy celów, ale nie należy do stanu początkowego
choose_goal(Goal, Goals, RestGoals,InitState) :-
    select(Goal,Goals,RestGoals),
    \+goal_achieved(Goal,InitState).

achieves(on(X,Z),move(X,Y/on(X,Y),Z)).
achieves(clear(Y),move(X/on(X,Y),Y,_)).

requires(move(X,Y/on(X,Y),Z),[clear(X),clear(Z)],[on(X,Y)]).
requires(move(X/on(X,Y),Y,Z),[clear(X/on(X,Y))],[diff(Z,X/on(X,Y)), clear(Z)]).

inst_action(Action, Conditions, State1, InstAction, 0) :-
    change_structures_to_simple_var(Action,InstAction),
    goals_achieved(Conditions,State1),
    write('Akcja '), write(Action), write(' ukonkretniona: '), write(InstAction).

inst_action(Action, Conditions, State1, move(X,Y,Z), 1) :-
    change_structures_to_simple_var(Action,InstAction),
    goals_achieved(Conditions,State1),
    InstAction = move(X,Y,_),
    write('move('), write(X), write(','), write(Y), write(',?)'), nl,
    find_clear_elements(State1, InstAction, OutList), write(OutList), nl,
    read(Z), nl,
    write(move(X,Y,Z)), nl, nl.

change_structures_to_simple_var(move(X/on(_,_),Y/on(_,_),Z),move(X,Y,Z)).
change_structures_to_simple_var(move(X/on(_,_),Y,Z),move(X,Y,Z)).
change_structures_to_simple_var(move(X,Y/on(_,_),Z),move(X,Y,Z)).

%Sprawdza czy akcja nie zniszczyła już osiągniętego celu. Każda akcja powoduje usunięcie dwóch elementów stanu, więc sprawdzamy czy nie są na liście celów osiągniętych.
%Można użyć member ponieważ akcja i cele są w pełni ukonkretnione.
%Procedura uruchamiana jedynie przy trybie wykonania 0,
check_action(move(X,Y,Z),AchievedGoals) :-
    \+ member1(clear(Z),AchievedGoals),
    \+ member1(on(X,Y),AchievedGoals).
check_action(move(X,Y,Z),AchievedGoals) :-
    member1(clear(Z),AchievedGoals),
    write('Akcja '), write(move(X,Y,Z)), write(' odrzucona, ponieważ niszczy osiągnięty cel: '), write(clear(Z)), nl,
    1 \= 1.
check_action(move(X,Y,Z),AchievedGoals) :-
    member1(on(X,Y),AchievedGoals),
    write('Akcja '), write(move(X,Y,Z)), write(' odrzucona, ponieważ niszczy osiągnięty cel: '), write(on(X,Y)), nl,
    1 \= 1.

perform_action(State1,move(X,Y,Z),[clear(Y),on(X,Z) | State2]) :-
    substract_all([clear(Z),on(X,Y)],State1,State2).

conc([],L2,L2).
conc([X|Rest1],L2,[X|Rest2]) :-
    conc(Rest1,L2,Rest2).

%wariant wywołania procedury plan dla użytkownika, w którym nie musi wyznaczyć celów spełnionych AchievedGoals, ponieważ robi to za niego program
%wyznaczamy cele, które są już spełnione w stanie początkowym (czyli jest przecięcie zbiorów InitState i Goals)
plan(InitState, Goals, Limit, Plan, FinalState, ExecutionMode) :-
    plan(InitState, Goals, [], Limit, Plan, FinalState, ExecutionMode, 0), !.
plan(InitState, Goals, Limit, Plan, FinalState, ExecutionMode) :-
    NewLimit is Limit + 1,
    nl, write('Zwiększamy limit do '), write(NewLimit), nl,
    plan(InitState, Goals, NewLimit, Plan, FinalState, ExecutionMode).

plan(State, Goals, _, _, [], State, _, RecurentionLevel) :-
    goals_achieved(Goals, State),
    write('Cele '), write(Goals), write(' spełnione w stanie '), write(State), rec(RecurentionLevel).

plan(_, _, _, 0, _, _, _, RecurentionLevel) :-
    write('Cele niespełnione i limit wynosi 0. Nastąpi nawrót. Poziom rekurencji: '), rec(RecurentionLevel), 1 \= 1.

plan(InitState, Goals, AchievedGoals, Limit, Plan, FinalState, ExecutionMode, RecurentionLevel) :-
    Limit > 0,
    NewRecurentionLevel is RecurentionLevel + 1,
    write('Procedura plan wykonuje się z limitem '), write(Limit), write(', Stan początkowy: '), write(InitState),
    write(',Cele: '), write(Goals), write(',Osiągnięte cele: '), write(AchievedGoals), rec(RecurentionLevel),
    range(LimitPre,0,Limit),
    choose_goal(Goal, Goals, RestGoals, InitState),
    write('Wybrano cel: '), write(Goal), write(', Reszta celów: '), write(RestGoals), rec(RecurentionLevel),
    achieves(Goal, Action),
    write('Cel '), write(Goal), write(' osiąga akcja '), write(Action), rec(RecurentionLevel),
    requires(Action, CondGoals, Conditions),
    write('Akcja '), write(Action), write(' musi spełniać warunki: '), write(Conditions), rec(RecurentionLevel),
    write('Preplan wywołany z limitem '), write(LimitPre), rec(RecurentionLevel),
    plan(InitState, CondGoals, AchievedGoals, LimitPre, PrePlan, State1, ExecutionMode, NewRecurentionLevel),
    inst_action(Action, Conditions, State1, InstAction, ExecutionMode), rec(RecurentionLevel),
    check_action(InstAction,AchievedGoals), !,
    perform_action(State1, InstAction, State2),
    write('Po wykonaniu akcji '), write(Action), write(' osiągnięto stan: '), write(State2), rec(RecurentionLevel),
    LimitPost is Limit-LimitPre-1,
    write('Postplan wywołany z limitem '), write(LimitPost), rec(RecurentionLevel),
    plan(State2, RestGoals, [Goal|AchievedGoals], LimitPost, PostPlan, FinalState, ExecutionMode, NewRecurentionLevel),
    conc(PrePlan, [InstAction | PostPlan], Plan),
    write('Nowy plan: '), write(Plan), rec(RecurentionLevel).

rec(RecLev) :- write(', Poziom rekurencji: '), write(RecLev), nl.