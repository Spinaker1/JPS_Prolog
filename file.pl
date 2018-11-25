goal_achieved(on(X,Y), State) :- member(on(X,Y), State).
goal_achieved(clear(X), State) :- member(clear(X), State).
goal_achieved(clear(X/on(X,Y)), State) :- goal_achieved(on(X,Y), State), goal_achieved(clear(X), State).
goal_achieved(on(X,Y/on(Y,Z)), State) :- goal_achieved(on(Y,Z), State), goal_achieved(on(X,Y), State).
goal_achieved(diff(Z,X/on(X,Y)), State) :- goal_achieved(on(X,Y), State), dif(Z,X).

plan(State, Goals, [], State) :- goals_achieved(Goals, State).

%jeśli nie ma żadnych celów, to znaczy, że są spełnione
goals_achieved([], _). 
%sprawdzamy każdy element listy celów, czy znajduje się też w liście stanu, chcemy uniknąć powtórzeń w liście celów 
goals_achieved([X|Goals], State) :- goals_achieved(Goals, State), goal_achieved(X,State), \+member(X,Goals). 

%wybieramy element który należy listy celów, ale nie należy do stanu początkowego
choose_goal(Goal, Goals, RestGoals,InitState) :- select(Goal,Goals,RestGoals), \+member(Goal,InitState). 

achieves(on(X,Z),move(X,Y/on(X,Y),Z)). 
achieves(clear(Y),move(X/on(X,Y),Y,Z)). 

requires(move(X,Y/on(X,Y),Z),[clear(X),clear(Z)],[on(X,Y)]).
requires(move(X/on(X,Y),Y,Z),[clear(X/on(X,Y))],[clear(Z),diff(Z,X/on(X,Y))]).

inst_action(move(X/on(X,Y),Y,Z),Cond,State,move(X,Y,Z)) :- goals_achieved(Cond,State).
inst_action(move(X,Y/on(X,Y),Z),Cond,State,move(X,Y,Z)) :- goals_achieved(Cond,State).

perform_action(State1,move(X,Y,Z),State2) :-  append(State1,[clear(Y),on(X,Z)],Temp),subtract(Temp,[clear(Z),on(X,Y)],State2).

conc(L1,L2,L3) :- append(L1,L2,L3).

plan(InitState, Goals, Plan, FinalState) :-
choose_goal(Goal, Goals, RestGoals, InitState),
achieves(Goal, Action),
requires(Action, CondGoals, Conditions),
plan(InitState, CondGoals, PrePlan, State1),
inst_action(Action, Conditions, State1, InstAction),
perform_action(State1, InstAction, State2),
plan(State2, RestGoals, PostPlan, FinalState),
conc(PrePlan, [InstAction | PostPlan], Plan).
