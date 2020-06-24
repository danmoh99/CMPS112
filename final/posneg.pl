posneg(P, N, []).%:- write(P), nl, write(N), nl.
posneg(P, N, [H|T]) :-
	((H > 0)->posneg([H|P], N, T);
		posneg(P, [H|N], T)).
