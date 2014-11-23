expand_field(Name, (Name,_G96)).
expand_predicate(This, (Name,Predicate), (Name,F)):-F=..[Predicate,This|'[]'].
build_class(Type,C):-C=class(Type,Members),::(Type,'Fields',_G2594),maplist('expand_field',_G2594,Fields),::(Type,'Predicates',_G2523),maplist(expand_predicate(C),_G2523,Predicates),append(Fields,Predicates,MembersLs),Members=..['members'|MembersLs].
::(class(_G967,M),Nm,Res):-arg(_G1017,M, (Nm,Res)).
construct_classType(This,N,F,Pred,Par,Out,Cons):- ::(This,'Name',N),::(This,'Fields',F),::(This,'Predicates',Pred),::(This,'Parent',Par),::(This,'Outer',Out),::(This,'Constructor',Cons).
class_def_classType(X):-X=class(X,members(('Name','classType'), ('Fields',['Name','Fields','Predicates','Parent','Outer','Constructor'|'[]']), ('Predicates','[]'), ('Parent','none'), ('Outer','none'), ('Constructor','construct_classType'))).
