expand_field(Name, (Name,_)).
expand_predicate(This, (Name,Predicate), (Name,F)):-F=..[Predicate,This].
build_class(Type,C):-build_class(Type,C,[]).
build_class(Type,C,ExtraMembers):-C=class(Type,Members),::(Type,'Fields',T0),maplist('expand_field',T0,Fields),::(Type,'Predicates',T1),maplist(expand_predicate(C),T1,Predicates),append(Fields,Predicates,MembersLs),append(MembersLs,ExtraMembers,AllMembers),Members=..['members'|AllMembers].
::(class(_,M),Nm,Res):-arg(_,M, (Nm,Res)).
::(class(_,M),Nm,Res,N):-arg(N,M, (Nm,Res)),'!'.
::(class(_,M),Nm,Res,_):-arg(_,M, (Nm,Res)).
object(class(_,_)).
classof(class(T,_),T).
construct_classType(This,N,F,Pred,Par,Out,Cons):- ::(This,'Name',N,1),::(This,'Fields',F,2),::(This,'Predicates',Pred,3),::(This,'Parent',Par,4),::(This,'Outer',Out,5),::(This,'Constructor',Cons,6).
class_def__classType(X):-X=class(X,members(('Name','classType'), ('Fields',['Name','Fields','Predicates','Parent','Outer','Constructor']), ('Predicates',[]), ('Parent','none'), ('Outer','none'), ('Constructor','construct_classType'))).
add_to_defined_names(NewNames):-current_predicate('defined_names'/1),defined_names(OldNames),'!',union(NewNames,OldNames,Names),retract(defined_names(OldNames)),assert(defined_names(Names)).
add_to_defined_names(NewNames):-assert(defined_names(NewNames)).
