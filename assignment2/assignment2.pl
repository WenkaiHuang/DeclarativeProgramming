%assignment2

%question1:

correspond(E1, [E1|_], E2, [E2|_]).
correspond(E1, [Head1|Tail1], E2, [Head2|Tail2]) :-
	correspond(E1, Tail1, E2, Tail2).


%question2:
interleave(Ls, L) :-
    interleave1(Ls, L, []),
    same_length(Ls).

  
interleave1([], [], []).
interleave1([], [H1|T1], [L|Ls]) :-
    interleave1([L|Ls], [H1|T1], []).
interleave1([[H1|[]]|T1], [H1|T2], L1) :-
    interleave1(T1, T2, L1).
interleave1([[H1|Elts]|T1], [H1|T2], L1) :-
    append(L1, [Elts], L2),
    interleave1(T1, T2, L2).

same_length([]).
same_length([_]).
same_length([E1,E2|T1]):-
  length(E1,L),
  length(E2,L),
  same_length([E1|T1]).
  

%quesiton3:

partial_eval(Expr0, Var, Val, Expr) :-
	( number(Expr0) ->
		Expr = Expr0
	).

partial_eval(Expr0, Var, Val, Expr) :-
	( atom(Expr0) ->
		 (Expr0 = Var ->
			Expr = Val
		;	Expr = Expr0
			)
	).

partial_eval(E1+E2, Var, Val, Expr) :-
	partial_eval(E1, Var, Val, Expr1),
	partial_eval(E2, Var, Val, Expr2),
	( number(Expr1), number(Expr2) ->
		Expr is (Expr1+Expr2)
	; Expr = Expr1+Expr2
	).

partial_eval(E1-E2, Var, Val, Expr) :-
	partial_eval(E1, Var, Val, Expr1),
	partial_eval(E2, Var, Val, Expr2),
	( number(Expr1), number(Expr2) ->
		Expr is (Expr1-Expr2)
	; Expr = Expr1-Expr2
	).

partial_eval(E1*E2, Var, Val, Expr) :-
	partial_eval(E1, Var, Val, Expr1),
	partial_eval(E2, Var, Val, Expr2),
	( number(Expr1), number(Expr2) ->
		Expr is (Expr1*Expr2)
	; Expr = Expr1*Expr2
	).

partial_eval(E1/E2, Var, Val, Expr) :-
	partial_eval(E1, Var, Val, Expr1),
	partial_eval(E2, Var, Val, Expr2),
	( number(Expr1), number(Expr2) ->
		Expr is (Expr1/Expr2)
	; Expr = Expr1/Expr2
	).

partial_eval(E1//E2, Var, Val, Expr) :-
	partial_eval(E1, Var, Val, Expr1),
	partial_eval(E2, Var, Val, Expr2),
	( number(Expr1), number(Expr2) ->
		Expr is (Expr1//Expr2)
	; Expr = Expr1//Expr2
	).