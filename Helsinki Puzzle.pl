% grid build(N,M) (M is an N*N grid of unbound variables):

grid_build(N,M):-
	helper(N,N,[],M).

helper(_,0,Acc,Acc).

helper(N,N1, Acc, Res):-
	N1>0,
	length(H,N),
	N2 is N1-1,
		helper(N,N2, [H|Acc], Res).
%-----------------------------------------
%grid_gen(N,M) (M is a grid of RANDOM numbers from 1 to N):	
grid_gen(N,M):-
    grid_build(N,M),
	
	randomList1(N,M).


randomList1(_,[]).
randomList1(N,[H|Res]):-
	num_gen(1,N,L),
	randomValues(L,H),
	randomList1(N,Res).
	
randomValues(List,[H|T]):-
	member(H,List),
	randomValues(List,T).
randomValues(_,[]).
%-----------------------------------------

%num_gen: a sequence of numbers from F to L

num_gen(F,F,[F]).

num_gen(F,L,[F|T]):-
	F < L ,
	F1 is F+1,
	num_gen(F1,L,T).

%-----------------------------------------


%acceptable_distribution helpers(get row and get coulmn):
getRow([H|_],1,H).

getRow([_|T],N,Row):- 
	N>1,
	N1 is N-1,
	getRow(T,N1,Row).

%get nth element in the list
getElement(1,[H|_],H).
getElement(Index,[_|T],N):-
	Index>1, Index1 is Index-1,
	getElement(Index1,T,N).

getColumn(M,Index,Column):-
	length(M,MaxIndex),
	getColumn(M,Index,[],MaxIndex,Column).

getColumn(_,_,Acc,0,Acc).

getColumn(M,Index,Acc,Counter,Res):- Counter>0,
	getRow(M,Counter,Row),
	getElement(Index,Row,Element),
	NewCounter is Counter-1,
	getColumn(M,Index,[Element|Acc],NewCounter,Res).

%to get the index of a row

getRowIndex(M,I,R):-
		subsetM(R,M),
		getRowHelper(M,Res,[],R),
		evaluate(Res,I).

getRowHelper([],Res,Res,_).	
getRowHelper([H|_],Res,Acc,H):- Res=[1|Acc].
getRowHelper([H|T],Res,Acc,R):- 
	H\=R,
	getRowHelper(T,Res,[1|Acc],R).
	
evaluate([],0).
evaluate([_|T],R):-
	evaluate(T,R1),
	R is R1+1.
	
subsetM(H,[H|_]).
subsetM(H,[_|T]):-
		subsetM(H,T).
	
%-----------------------------------------

%acceptable_distribution:

acceptable_distribution(M):-
	length(M,L),
	acceptable_distribution(M,L).

acceptable_distribution(_,0).

acceptable_distribution(M,I):- I>0,
	getColumn(M,I,Column),getRow(M,I,Row),Row\=Column,
	I1 is I-1,acceptable_distribution(M,I1).

%-----------------------------------------

%Transpose:

trans(M,M1):-
       length(M,L1),
       length(M1,L1),
       trans(M,M1,L1).
trans(_,_,0).
trans(M,M1,L):-
      %length(M,L1),
      getColumn(M,L,Row),
      %length(Row,L1),
      getRow(M1,L,Row),
      L2 is L-1,
     trans(M,M1,L2).


%-----------------------------------------

%distinct_rows:
%goes by each row in the matrix and checks whether or not it is unrepeated

distinct_rows(M):-
	length(M,L),
	L>0,
	distinct_rows(M,L).

distinct_rows(M,L):-
	getRow(M,L,Row),
	L1 is L-1,
	unrepeatedRow(M,L1,Row),
	distinct_rows(M,L1).

distinct_rows(_,0).	

%checks whether the Lth row is repeated at row L-1, L-2,..1

unrepeatedRow(M,L1,Row):-
	L1>0,
	getRow(M,L1,Row2), Row2\=Row,
	L2 is L1-1,
	unrepeatedRow(M,L2,Row).

unrepeatedRow(_,0,_).	


%-----------------------------------------

%distinct_columns:
%goes by each column in the matrix and checks whether or not it is unrepeated


distinct_columns(M):-
	length(M,L),
	L>0,
	distinct_col(M,L).

distinct_col(M,L):-
	getColumn(M,L,Col),
	L1 is L-1,
	unrepeatedCol(M,L1,Col),
	distinct_col(M,L1).

distinct_col(_,0).	

unrepeatedCol(M,L1,Col):-
	L1>0,
	getColumn(M,L1,Col2), Col2\=Col,
	L2 is L1-1,
	unrepeatedCol(M,L2,Col).

unrepeatedCol(_,0,_).	

%-------------------------------------------
%check_num_grid

maxOfList([H|T],M):- 
	maxHelper(T,H,M).

maxHelper([],M,M).
maxHelper([X|T],H,M):-
	X>H , 
	maxHelper(T,X,M).
maxHelper([X|T],H,M):-
	X=<H , 
	maxHelper(T,H,M).

check_num_grid(G):-
	flatten(G,Flist),
	maxOfList(Flist,Max),
	NewMax is Max-1,
	num_gen(1,NewMax,SeqList),
	subset(SeqList, Flist).

%---------------------------------------
%row_col_match

row_col_match(M):-
			length(M,N),
			matchHelper(M,N,1).

matchHelper(_,N,N1):- N1 is N+1.
matchHelper(M,N,Index):-
			Index =< N,
			getRow(M,Index,R),
			trans(M,M1),
			getRowIndex(M1,Index1,R),
			Index \= Index1,
			I is Index+1,
			matchHelper(M,N,I).
			
%----------------------------------------
%acceptable_permutation

acceptable_permutation(L,R):-
			perm(L,R),
			allDiff(L,R).

perm([],[]).
perm([H|T],R) :- 	
			perm(T,X), 
			remove2(H,R,X).
remove2(H,[H|R],R).
remove2(X,[H|R],[H|S]) :-
			X\=H,
			remove2(X,R,S).	

allDiff([],[]).
allDiff([X|T1],[Y|T2]):-
			X \= Y,
			allDiff(T1,T2).
			
acceptable_perm([]).
acceptable_perm([X,Y|T]):-
			acceptable_permutation(X,Y),
			acceptable_perm([Y|T]).

%----------------------------------------

%MAKES IT FAST			

indexPermutation(M):-
	length(M,N),
	num_gen(1,N,L),
	acceptable_permutation(L,ColumnIndex),
	helperIndex(M,ColumnIndex,1).

helperIndex(_,[],_).
helperIndex(M,[H|T],C):-
	getRow(M,H,Row),
	getColumn(M,C,Row),
	C1 is C+1,
	helperIndex(M,T,C1).

%----------------------------------------
%helsinki
			
helsinki(N,G):-
  	grid_build(N,G),
  	indexPermutation(G),
	grid_gen(N,G),
	
	check_num_grid(G),
	acceptable_distribution(G),
	distinct_rows(G),
	row_col_match(G).