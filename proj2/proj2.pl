%  File     : proj2.pl
%  ID       : 991257
%  Author   : Wenkai Huang (whhuang@student.unimelb.edu.au)
%  Origin   : Tue May 14 14:59:10 2019
%  Purpose  : This prolog file is to solve the math puzzles which
%			  is illustrated in the project2 in details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Defining a predicate puzzle_solution/1 to find the value in 
%% the squares which satisfies the constraints.
%%
%% 
%% This code is to find the valid value with some constraints
%% in the puzzle to fit the project2 specification. Basically,
%% it possesses to part, the first one is to make all the value
%% in diagonal except the heads in the first row and first column
%% to be same and then it makes the varible ground and subsitutes
%% the right value into the varible. What is more, all the number
%% filled in are distinct anf between 1 and 9. By doing that, it
%% can help us save time because we can find the same value in the
%% diagonal at first instead of finding a lot of case and restricts
%% the value in the diagonal to be the same.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%					   Running the Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(clpfd)).


% The argument of Puzzle is the matrix (list of lists) containing 
% the varibles or numbers.
%
% This predicate is to find the result of input in the project2 
% and it also can check whether the value suit the constraints in
% the project. It unifies whether the diagonal except the heads
% is the same using  and then finds or checks the other values 
% fitting the contraints. To realise that, it uses unify_diagonal/1
% and check_valid_solution/1 separately.
%
% puzzle_solution/1
% Other predicates: unify_diagonal/1, check_valid_solution/1
% puzzle_solution(?Puzzle)
puzzle_solution(Puzzle) :-
	unify_diagonal(Puzzle),
	check_valid_solution(Puzzle).


% The argument of this predicate is only the Puzzle which is same
% as the argument in the puzzle_solution. The argument of
% puzzle/1 denotes the Puzzle which has been removed the first
% row and first column.
%
% This predicate is to make the the diagonal be the same using the
% split_heads/1 to remove the first row and first column and using
% the unify_diagonal_1/2 to make an iteration to ensure all the
% the value is the same.
%
% unify_diagonal/1
% Other predicates: split_heads/2, unify_diagonal_1/1
% unify_diagonal(?Puzzle)
unify_diagonal([]).
unify_diagonal([_|[]]).
unify_diagonal(Puzzle) :-
   split_heads(Puzzle, Puzzle_1),
   unify_diagonal_1(Puzzle_1).

% The first argument is also the Puzzle matrix and the second 
% argument Split_Heads_Puzzle is output of Puzzle which 
% removes the heads in first row and first column. E denotes
% the number in the list of lists when there is only one
% element. Tail1 is the first row without the the first element
% Tail2 is the removing the first row remain squares after using
% transpose/2 predicate.
%
% This predicate basically uses twice transpose/2 predicate to remove
% the first row at first and transpose the row to the column vice versa
% and then remove the first row again to get the squares without the 
% first row and the first column.
%
% split_heads/2
% Another predicate: transpose/2
% split_heads(?Puzzle, -Split_Heads_Puzzle)
split_heads([], []).
split_heads([[E|[]]|[]], Split_Heads_Puzzle) :-
   Split_Heads_Puzzle = [[E|[]]|[]].
split_heads([_|Tail1], Split_Heads_Puzzle) :-
   transpose(Tail1, [_|Tail2]),
   transpose(Tail2, Split_Heads_Puzzle).

% The Split_Heads_Puzzle is the Puzzle without the first row and
% first column. The Unified_Diagonal_Puzzle is the Split_Heads_Puzzle
% restricts the diagonal value. Head is the first value of the first
% row in current matrix and Tail1 is the current first row without
% first element. Elts1 is the remaining squares. Tail2 is the first 
% row without the Head when removing the first row and first column.
% Elts2 is the remaining squares. L is the number of rows.
%
% This predicate is iteration of the unify_diagonal which removes
% the first row and first column. It just uses the split_heads
% to remove the first row and first column each time and  check 
% whether the first element of first row is the same with the later
% one to ensure the diagonal has the same value. When it comes to
% the last case, to avoid it interating infinitely, use the length 
% predicate and set a base case to deal with this situation.
%
% unify_diagonal_1/1
% Other predicates: length/2, split_heads/2
% unify_diagonal_1(?Split_Heads_Puzzle, -Unified_Diagonal_Puzzle)
unify_diagonal_1([[_|[]]|[]]).
unify_diagonal_1([[Head|Tail1]|Elts1]) :-
   length([[Head|Tail1]|Elts1], L),
   (L > 1 ->
   split_heads([[Head|Tail1]|Elts1], [[Head|Tail2]|Elts2]),
   unify_diagonal_1([[Head|Tail2]|Elts2])).


% The Head is the first row of the Valid_Puzzle and Tail1 denotes
% remaining rows. Tail2 is the remaining rows without the first 
% row after transposing the predicate.
%
% This predicate is just to remove the first row to check or find 
% the value on each row iteratively and then transpose the 
% Valid_Puzzle to check or find the value in the columns.
%
% check_valid_solution/1
% Other predicates: transpose/2, check_valid_solution_1/1
% check_valid_solution(?Valid_Puzzle)
check_valid_solution([]).
check_valid_solution([_|[]]).
check_valid_solution([Head|Tail1]) :-
   check_valid_solution_1(Tail1),
   transpose([Head|Tail1], [_|Tail2]),
   check_valid_solution_1(Tail2).

% The argument of Head is the first row and the Tail is the
% rest rows.
%
% This predicate is to interate each row and check or find
% the value in each row.
%
% check_valid_solution_1/1
% Another predicate: check_row/1
% check_valid_solution_1(?No_First_Row_Puzzle)
check_valid_solution_1([Head|[]]) :-
   check_row(Head).
check_valid_solution_1([Head|Tail]) :-
   check_row(Head),
   check_valid_solution_1(Tail).

% The head is the first value of each row and Tail is rest.
% E1 is the result of sum in a row and E2 is the result of 
% product in a row.
%
% The check_row check whether the element in the tail is
% ground using the check_ground predicate and make it distinct
% with each other in the Tail and then sum and multiply the Tail.
% Finally, to check whether there is a case to satisfy 
% the constraints.
%
% check_row/1
% Other predicates: check_ground/1, all_distinct/1
% sumlist/2, multiply/2
% check_row(?Row)
check_row([Head, Head]).
check_row([Head|Tail]) :-
	check_ground(Tail),
	all_distinct(Tail),
	sumlist(Tail, E1),
	multiply(Tail, E2),
	(Head = E1 ; Head = E2).

% The Head is the first element in the row after removing
% the first element. The Tail is the rest elements.
%
% This predicate check if the Head is ground. If it is not
% ground, it just set the value from 1 to 9. On the other
% hand, if it is ground, it checks the next element.
%
% check_ground/1
% Other predicates: ground/1, between/3
% check_ground(?Tail)
check_ground([]).
check_ground([Head|Tail]) :-
	(ground(Head) ->
		check_ground(Tail)
	;
		between(1, 9, Head),
		check_ground(Tail)
	).

% Head is the first element in a row without the first element,
% Tail is the rest and E is the result of multiply elements. E1
% is the result in the inner interation.
%
% This predicate put the whole row without the first element in.
% It interates the rest Tail firs to get the value and multiply
% the Head to get final result.
% 
% multiply/2
% multiply(+Row, -Result)
multiply([E], E).
multiply([Head|Tail], E) :-
   multiply(Tail, E1),
   E is  Head *E1.