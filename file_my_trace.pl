member1(X,[X|_]).
member1(X,[_|Rest]) :-
    member1(X,Rest).

intersect([], _, []).
intersect([Elem|RestL], L, [Elem|Result]) :-
	member1(Elem, L), !,
	intersect(RestL, L, Result).
intersect([Elem|RestL], L, Result) :-
    intersect(RestL, L, Result).

substract1(_,[],[]).
substract1(X,[X|Rest],Rest).
substract1(X,[Y|Rest],[Y|Result]) :-
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
	my_trace(1,choose_goal,1,[]),
	my_trace(2,choose_goal,1,select),
	select(Goal,Goals,RestGoals), 
	my_trace(3,choose_goal,1,select,['Goal'/Goal,'RestGoals'/RestGoals]),
	my_trace(2,choose_goal,1,goal_achieved),
	\+goal_achieved(Goal,InitState),
	my_trace(3,choose_goal,1,goal_achieved,[]),
	my_trace(4,choose_goal,1,[]).

achieves(on(X,Z),move(X,Y/on(X,Y),Z)) :-
    my_trace(1,achieves,1,[]),
    my_trace(4,achieves,1,[]).
achieves(clear(Y),move(X/on(X,Y),Y,_)) :-
    my_trace(1,achieves,2,[]),
    my_trace(4,achieves,2,[]).

requires(move(X,Y/on(X,Y),Z),[clear(X),clear(Z)],[on(X,Y)])  :-
    my_trace(1,requires,1,[]),
    my_trace(4,requires,1,[]).
requires(move(X/on(X,Y),Y,Z),[clear(X/on(X,Y))],[diff(Z,X/on(X,Y)), clear(Z)]) :-
    my_trace(1,requires,1,[]),
    my_trace(4,requires,1,[]).

inst_action(Action, Conditions, State1, InstAction) :-
    change_structures_to_simple_var(Action,InstAction),
    goals_achieved(Conditions,State1).

change_structures_to_simple_var(move(X/on(_,_),Y/on(_,_),Z),move(X,Y,Z)).
change_structures_to_simple_var(move(X/on(_,_),Y,Z),move(X,Y,Z)).
change_structures_to_simple_var(move(X,Y/on(_,_),Z),move(X,Y,Z)).

%Sprawdza czy akcja nie zniszczyła już osiągniętego celu. Każda akcja powoduje usunięcie dwóch elementów stanu, więc sprawdzamy czy nie są na liście celów osiągniętych.
%Można użyć member ponieważ akcja i cele są w pełni ukonkretnione.
check_action(move(X,Y,Z),AchievedGoals) :-
    \+ member1(clear(Z),AchievedGoals),
    \+ member1(on(X,Y),AchievedGoals).

perform_action(State1,move(X,Y,Z),[clear(Y),on(X,Z) | State2]) :-
    substract_all([clear(Z),on(X,Y)],State1,State2).

conc([],L2,L2).
conc([X|Rest1],L2,[X|Rest2]) :-
    conc(Rest1,L2,Rest2).

%wariant wywołania procedury plan dla użytkownika, w którym nie musi wyznaczyć celów spełnionych AchievedGoals, ponieważ robi to za niego program 
%wyznaczamy cele, które są już spełnione w stanie początkowym (czyli jest przecięcie zbiorów InitState i Goals)
plan(InitState, Goals, Limit, Plan, FinalState) :-
    intersect(InitState,Goals,AchievedGoals),
    plan(InitState, Goals, AchievedGoals, Limit, Plan, FinalState,1).

plan(State, Goals, AchievedGoals, Limit, [], State, Level) :-
    my_trace_rec(1,plan,1,Level,['State'/State,'Goals'/Goals,'AchievedGoals'/AchievedGoals,'Limit'/Limit]),
    Limit > 0,
    goals_achieved(Goals, State),
    my_trace_rec(4,plan,1,Level,['Plan'/[]]).

plan(InitState, Goals, AchievedGoals, Limit, .Plan, FinalState, Level) :-
    my_trace_rec(1,plan,2,Level,['InitState'/InitState,'Goals'/Goals,'AchievedGoals'/AchievedGoals,'Limit'/Limit]),

    NewLevel is Level + 1 ,
    Limit > 0,
    LimitPre is Limit//2 ,

    my_trace_rec(2,plan,2,Level,choose_goal),
    choose_goal(Goal, Goals, RestGoals, InitState),
    my_trace_rec(3,plan,2,choose_goal,Level,['Goal'/Goal,'RestGoals'/RestGoals]),

    my_trace_rec(2,plan,2,Level,achieves),
    achieves(Goal, Action),
    my_trace_rec(3,plan,2,Level,achieves,['Action'/Action]),

    my_trace_rec(2,plan,2,Level,requires),
    requires(Action, CondGoals, Conditions),
    my_trace_rec(3,plan,2,Level,requires,['CondGoals'/CondGoals, 'Conditions'/Conditions]),

    plan(InitState, CondGoals, AchievedGoals, LimitPre, PrePlan, State1, NewLevel),

    my_trace_rec(2,plan,2,Level,inst_action),
    inst_action(Action, Conditions, State1, InstAction),
    my_trace_rec(3,plan,2,Level,inst_action,['InstAction'/InstAction]),

    my_trace_rec(2,plan,2,Level,check_action),
    check_action(InstAction,AchievedGoals),
    my_trace_rec(3,plan,2,Level,check_action,[]),

    my_trace_rec(2,plan,2,Level,perform_action),
    perform_action(State1, InstAction, State2),
    my_trace_rec(3,plan,2,Level,perform_action,['State2'/State2]),

    LimitPost is Limit-LimitPre - 1 ,
    plan(State2, RestGoals, [Goal|AchievedGoals], LimitPost, PostPlan, FinalState, NewLevel),

    my_trace_rec(2,plan,2,Level,conc),
    conc(PrePlan, [InstAction | PostPlan], Plan),
    my_trace_rec(3,plan,2,Level,conc,['Plan'/Plan]),

    my_trace_rec(4,plan,2,Level,['Plan'/Plan]).