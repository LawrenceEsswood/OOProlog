link(pred(Nm, N), _) :-
	length(Args, N),
	Orig =.. [Nm|Args],
	atom_concat('p_', Nm, NNm),
	New =.. [NNm|Args],
	retractall(Nm/N),
	assert(Orig :- New).
link_names :- defined_names(Names), filter(link, Names, _).