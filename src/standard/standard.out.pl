expand_field(Name, (Name,_G102)).
expand_predicate(This, (Name,Predicate), (Name,F)):-F=..[Predicate,This|'[]'].
build_class(Type,C):-C=class(Type,Members),::(Type,'Fields',_G3110),maplist('expand_field',_G3110,Fields),::(Type,'Predicates',_G3039),maplist(expand_predicate(C),_G3039,Predicates),append(Fields,Predicates,MembersLs),Members=..['members'|MembersLs].
::(class(_G973,M),Nm,Res):-arg(_G1023,M, (Nm,Res)).
::(class(_G1175,M),Nm,Res,N):-arg(N,M, (Nm,Res)),'!'.
::(class(_G1396,M),Nm,Res,_G1424):-arg(_G1454,M, (Nm,Res)).
construct_classType(This,N,F,Pred,Par,Out,Cons):- ::(This,'Name',N,1),::(This,'Fields',F,2),::(This,'Predicates',Pred,3),::(This,'Parent',Par,4),::(This,'Outer',Out,5),::(This,'Constructor',Cons,6).
class_def__classType(X):-X=class(X,members(('Name','classType'), ('Fields',['Name','Fields','Predicates','Parent','Outer','Constructor'|'[]']), ('Predicates','[]'), ('Parent','none'), ('Outer','none'), ('Constructor','construct_classType'))).
