%Alpha Chen
%Period 2
%1/19/00
%Prolog Assignment 1

different(X,X) :- !, fail.
different(_,_).

parent(pam,bob).
parent(tom,bob).
parent(tom,liz).
parent(bob,ann).
parent(bob,pat).
parent(pat,jim).
female(pam).
female(liz).
female(ann).
female(pat).
male(tom).
male(bob).
male(jim).

mother(X,Y) :-
  parent(X,Y),
  female(X).

predecessor(X,Y) :-
  parent(X,Y).

predecessor(X,Y) :-
  parent(X,Z),
  predecessor(Z,Y).

offspring(Y,X) :-
  parent(X,Y).

grandparent(X,Z) :-
  parent(X,Y),
  parent(Y,Z).

sister(X,Y) :-
  female(X),
  parent(Z,X),
  parent(Z,Y),
  different(X,Y).

happy(X) :-
  parent(X,_).

hastwochildren(X) :-
  parent(X,Y),
  sister(Y,_).

grandchild(X,Z) :-
  parent(Z,Y),
  parent(Y,X).

aunt(X,Y) :-
  sister(X,Z),
  parent(Z,Y).

%Question 11

flies(X) :-
  hasfeathers(X),
  animal(X).

bird(X) :-
  flies(X),
  layseggs(X).

animal(tracy).
hasfeathers(tracy).
layseggs(tracy).
