plan(State, Goals, [], State) :- goals_achieved(Goals, State).

%jeśli nie ma żadnych celów, to znaczy, że są spełnione
goals_achieved([], _). 
%sprawdzamy każdy element listy celów, czy znajduje się też w liście stanu, chcemy uniknąć powtórzeń w liście celów 
goals_achieved([X|Goals], State) :- goals_achieved(Goals, State), member(X,State), \+member(X,Goals). 

plan(InitState, Goals) :- choose_goal(Goal, Goals, RestGoals, InitState).

%wybieramy element który należy listy celów, ale nie należy do stanu początkowego
choose_goal(Goal, Goals, RestGoals,InitState) :- select(Goal,Goals,RestGoals), \+member(Goal,InitState). 

achieves(on(X,Y),move(X,Z/on(X,Z),Y)). 

requires(move(X,Z/on(X,Z),Y),[clear(X),clear(Y)],[]).