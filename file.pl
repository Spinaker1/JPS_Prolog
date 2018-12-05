member1(X,[X|_]).
member1(X,[_|Rest]) :- member1(X,Rest).

substract1(_,[],[]).
substract1(X,[X|Rest],Rest).
substract1(X,[Y|Rest],[Y|Result]) :- substract1(X,Rest,Result).

substract_all([],OldList,OldList).
substract_all([X|Rest],OldList,NewList) :- substract1(X,OldList,Temp),substract_all(Rest,Temp,NewList).

goal_achieved(on(X,Y), State) :- \+ compound(Y) , member1(on(X,Y), State).
goal_achieved(clear(X), State) :- \+ compound(X) , member1(clear(X), State).
goal_achieved(clear(X/on(X,Y)), State) :- goal_achieved(on(X,Y), State), member1(clear(X), State).
goal_achieved(on(X,Y/on(Y,Z)), State) :- goal_achieved(on(Y,Z), State), member1(on(X,Y), State).
goal_achieved(diff(Z,X/on(X,Y)), State) :- goal_achieved(on(X,Y), State), Z \= X.

%jeśli nie ma żadnych celów, to znaczy, że są spełnione
goals_achieved([], _). 
%sprawdzamy każdy element listy celów, czy znajduje się też w liście stanu, chcemy uniknąć powtórzeń w liście celów 
goals_achieved([X|Goals], State) :- goals_achieved(Goals, State), goal_achieved(X,State), \+member(X,Goals). 

%wybieramy element który należy listy celów, ale nie należy do stanu początkowego
choose_goal(Goal, Goals, RestGoals,InitState) :- select(Goal,Goals,RestGoals), \+goal_achieved(Goal,InitState). 

achieves(on(X,Z),move(X,Y/on(X,Y),Z)). 
achieves(clear(Y),move(X/on(X,Y),Y,_)). 

requires(move(X,Y/on(X,Y),Z),[clear(X),clear(Z)],[on(X,Y)]).
requires(move(X/on(X,Y),Y,Z),[clear(X/on(X,Y))],[diff(Z,X/on(X,Y)), clear(Z)]).

inst_action(Action, Conditions, State1, InstAction) :- dupa(Action,InstAction),goals_achieved(Conditions,State1).

dupa(move(X/on(_,_),Y/on(_,_),Z),move(X,Y,Z)).
dupa(move(X/on(_,_),Y,Z),move(X,Y,Z)).
dupa(move(X,Y/on(_,_),Z),move(X,Y,Z)).

perform_action(State1,move(X,Y,Z),[clear(Y),on(X,Z) | State2]) :-  substract_all([clear(Z),on(X,Y)],State1,State2).

conc([],L2,L2).
conc([X|Rest1],L2,[X|Rest2]) :- conc(Rest1,L2,Rest2).

plan(State, Goals, [], State) :- goals_achieved(Goals, State).

plan(InitState, Goals, Plan, FinalState) :-
choose_goal(Goal, Goals, RestGoals, InitState),
achieves(Goal, Action),
requires(Action, CondGoals, Conditions),
plan(InitState, CondGoals, PrePlan, State1),
inst_action(Action, Conditions, State1, InstAction),
perform_action(State1, InstAction, State2),
plan(State2, RestGoals, PostPlan, FinalState),
conc(PrePlan, [InstAction | PostPlan], Plan).
