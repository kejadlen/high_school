member(X,[X|Y]).
member(X,[H|T]) :-
  member(X,T).

different(X,X) :-
  !,
  fail.
different(_,_).

person(bob,male,1902).
person(ann,female,1904).
person(joe,male,1900).
person(jill,female,1910).
person(parth,male,1927).
person(jane,female,1930).
person(jack,male,1935).
person(pat,female,1937).
person(jim,male,1940).
person(jad,male,1949).
person(steven,male,1951).

family(bob,ann,[jane,jack]).
family(joe,jill,[pat,jim]).
family(parth,jane,[]).
family(jack,pat,[jad,steven]).

sex(X,Y) :-
  person(X,Y,_).

dob(X,Y) :-
  person(X,_,Y).

husband(X,Y) :-
  family(X,Y,_).

wife(X,Y) :-
  family(Y,X,_).

mother(X,Y) :-
  family(_,X,Z),
  member(Y,Z).

father(X,Y) :-
  family(X,_,Z),
  member(Y,Z).

parent(X,Y) :-
  father(X,Y).

parent(X,Y) :-
  mother(X,Y).

offspring(X,Y) :-
  parent(Y,X).

son(X,Y) :-
  offspring(X,Y),
  sex(X,male).

daughter(X,Y) :-
  offspring(X,Y),
  sex(X,female).

grandparent(X,Z) :-
  parent(X,Y),
  parent(Y,Z).

grandmother(X,Y) :-
  grandparent(X,Y),
  sex(X,female).

grandfather(X,Y) :-
  grandparent(X,Y),
  sex(X,male).

sibling(X,Y) :-
  parent(Z,X),
  parent(Z,Y),
  different(X,Y).

ancestor(X,Y) :-
  parent(Z,Y),
  ancestor(X,Z).

ancestor(X,Y) :-
  parent(X,Y).

descendant(X,Y) :-
  offspring(Z,Y),
  descendant(X,Z).

descendant(X,Y) :-
  offspring(X,Y).

oldersister(X,Y) :-
  sister(X,Y),
  dob(Y,Z),
  dob(X,A),
  >(Z,A).

brother(X,Y) :-
  sibling(X,Y),
  sex(X,male).

sister(X,Y) :-
  sibling(X,Y),
  sex(X,female).

uncle(X,Y) :-
  parent(Z,Y),
  brother(X,Z).
uncle(X,Y) :-
  parent(Z,Y),
  sister(M,Z),
  husband(X,M),
  sex(X,male).
