:- ['./standard.out.pl'].
class_def_io(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T0), (T0=none;call(T0,T,io,['In','Out'],[],none,none,p_io_io)).
p_io_io(This,T1,T2):- ::(This,'In',T1),::(This,'Out',T2).
class_def_class_header_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T3), (T3=none;call(T3,T,class_header_token,['Name','Parents'],[],none,none,p_class_header_token_class_header_token)).
p_class_header_token_class_header_token(This,T4,T5):- ::(This,'Name',T4),::(This,'Parents',T5).
class_def_class_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T6), (T6=none;call(T6,T,class_token,['Hdr','Bdy'],[],none,none,p_class_token_class_token)).
p_class_token_class_token(This,T7,T8):- ::(This,'Hdr',T7),::(This,'Bdy',T8).
class_def_clause_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T9), (T9=none;call(T9,T,clause_token,['Left','Name','N','Mods'],[ (split_fnc,p_clause_token_split_fnc), (check_mod_allowed,p_clause_token_check_mod_allowed)],none,none,none)).
p_clause_token_split_fnc(This):- ::(This,'Left',T10),::(This,'Name',T11),::(This,'N',T12),functor(T10,T11,T12).
p_clause_token_check_mod_allowed(This,Mod):- ::(This,'Mods',T13), (not(member(Mod,T13)), (not(member(Mod,['public','private','protected']));intersection(T13,['public','private','protected'],[]));throw_atoms(['Illegal modifier ',Mod])).
class_def_rule_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T14), (T14=none;call(T14,T,rule_token,['Left','Name','N','Mods','Right'],[ (add_mod,p_rule_token_add_mod), (split_fnc,p_clause_token_split_fnc), (check_mod_allowed,p_clause_token_check_mod_allowed)],none,none,p_rule_token_rule_token)).
p_rule_token_rule_token(This,T15,T16):- ::(This,'Left',T15),::(This,'Right',T16),p_clause_token_split_fnc(This),::(This,'Mods',T17),T17=[].
p_rule_token_rule_token(This,T18,T19,T20):- ::(This,'Left',T18),::(This,'Right',T19),::(This,'Mods',T20),p_clause_token_split_fnc(This).
p_rule_token_add_mod(This,Mod,Tkn):-p_clause_token_check_mod_allowed(This,Mod),class_def_rule_token(T22),::(This,'Left',T23),::(This,'Right',T24),::(This,'Mods',T25),build_class(T22,Tkn,[]),::(T22,'Constructor',T21), (T21=none;call(T21,Tkn,T23,T24,[Mod|T25])).
class_def_fact_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T26), (T26=none;call(T26,T,fact_token,['Left','Name','N','Mods'],[ (add_mod,p_fact_token_add_mod), (split_fnc,p_clause_token_split_fnc), (check_mod_allowed,p_clause_token_check_mod_allowed)],none,none,p_fact_token_fact_token)).
p_fact_token_fact_token(This,T27):- ::(This,'Left',T27),p_clause_token_split_fnc(This),::(This,'Mods',T28),T28=[].
p_fact_token_fact_token(This,T29,T30):- ::(This,'Left',T29),::(This,'Mods',T30),p_clause_token_split_fnc(This).
p_fact_token_add_mod(This,Mod,Tkn):-p_clause_token_check_mod_allowed(This,Mod),class_def_fact_token(T32),::(This,'Left',T33),::(This,'Mods',T34),build_class(T32,Tkn,[]),::(T32,'Constructor',T31), (T31=none;call(T31,Tkn,T33,[Mod|T34])).
:-op(100,'yfx','::').
:-op(99,'fx','class').
:-op(100,'xfx','extends').
:-op(1199,'fy','static').
:-op(1199,'fy','public').
:-op(1199,'fy','private').
:-op(1199,'fy','protected').
class_def_parser(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T35), (T35=none;call(T35,T,parser,['IO'],[ (parse_until_eof,p_parser_parse_until_eof), (parse_defs,p_parser_parse_defs), (parse_def,p_parser_parse_def), (parse_include,p_parser_parse_include), (parse_field,p_parser_parse_field), (parse_class,p_parser_parse_class), (parse_header,p_parser_parse_header), (all_mods,p_parser_all_mods), (parse_clause,p_parser_parse_clause), (tag_predicate,p_parser_tag_predicate), (is_named_member,p_parser_is_named_member), (name_dont_care,p_parser_name_dont_care), (tag_atoms,p_parser_tag_atoms)],none,none,p_parser_parser)).
p_parser_parser(This,T36):- ::(This,'IO',T36).
p_parser_parse_until_eof(This,Res):-p_parser_parse_defs(This,Res,'end_of_file').
p_parser_parse_defs(This,DefLs,End):- ::(This,'IO',T38),::(T38,'In',T37),read_term(T37,T,[variable_names(Eqs)]),p_not_unified(T,End),'!', (p_not_unified(T,'end_of_file');throw('Unexpected end of stream')), (p_parser_parse_def(This,Eqs,T,DefL);throw('Could not parse definition')),p_bind_vars_to_name(Eqs),append(DefL,DefR,DefLs),p_parser_parse_defs(This,DefR,End).
p_parser_parse_defs(_,[],_).
p_parser_parse_def(This,_,T,[Def]):-var(T),'!',p_parser_parse_field(This,T,Def).
p_parser_parse_def(This,_,T,Defs):-p_parser_parse_include(This,T,Defs).
p_parser_parse_def(This,_,T,[Def]):-p_parser_parse_class(This,T,Def).
p_parser_parse_def(This,Eqs,T,[Def]):-p_parser_parse_clause(This,Eqs,T,Def).
p_parser_parse_include(This, (:-oopl_include(Path)),Defs):-open(Path,'read',In),class_def_io(T40),::(This,'IO',T42),::(T42,'Out',T41),build_class(T40,NewIO,[]),::(T40,'Constructor',T39), (T39=none;call(T39,NewIO,In,T41)),class_def_parser(T44),build_class(T44,P,[]),::(T44,'Constructor',T43), (T43=none;call(T43,P,NewIO)),::(P,'parse_defs',T45),call(T45,Defs,'end_of_file'),close(In).
p_parser_parse_field(_,X,field(X)):-'!'.
p_parser_parse_class(This,T,class_def(C)):-p_parser_parse_header(This,T,H),p_parser_parse_defs(This,Defs,'endclass'),class_def_class_token(T47),build_class(T47,C,[]),::(T47,'Constructor',T46), (T46=none;call(T46,C,H,Defs)).
p_parser_parse_header(_,class(X)extends Y,H):-atomic(X),atomic(Y),'!',class_def_class_header_token(T49),build_class(T49,H,[]),::(T49,'Constructor',T48), (T48=none;call(T48,H,X,[Y])).
p_parser_parse_header(_,class(X),H):-atomic(X),class_def_class_header_token(T51),build_class(T51,H,[]),::(T51,'Constructor',T50), (T50=none;call(T50,H,X,[])).
p_parser_all_mods(_,['static','public','private','protected']).
p_parser_parse_clause(This,Eqs, (ModA:-B),R):-ModA=..[Mod,A],p_parser_all_mods(This,AllMods),member(Mod,AllMods),'!',ModP=..[Mod, (A:-B)],p_parser_parse_clause(This,Eqs,ModP,R).
p_parser_parse_clause(This,Eqs,ModP,R2):-ModP=..[Mod,P],p_parser_all_mods(This,AllMods),member(Mod,AllMods),'!',p_parser_parse_clause(This,Eqs,P,R),R=..[F,Arg],::(Arg,'add_mod',T52),call(T52,Mod,Arg2),R2=..[F,Arg2].
p_parser_parse_clause(This,Eqs, (:-Xs),rule(R)):-'!',p_parser_tag_atoms(This,Eqs,Xs,Ys),class_def_rule_token(T54),build_class(T54,R,[]),::(T54,'Constructor',T53), (T53=none;call(T53,R,'',Ys)).
p_parser_parse_clause(This,Eqs, (P:-Xs),rule(R)):-'!',p_parser_tag_predicate(This,Eqs,P,Pd),p_parser_tag_atoms(This,Eqs,Xs,Ys),class_def_rule_token(T56),build_class(T56,R,[]),::(T56,'Constructor',T55), (T55=none;call(T55,R,Pd,Ys)).
p_parser_parse_clause(This,Eqs,P,fact(F)):-p_parser_tag_predicate(This,Eqs,P,Pd),class_def_fact_token(T58),build_class(T58,F,[]),::(T58,'Constructor',T57), (T57=none;call(T57,F,Pd)).
p_parser_tag_predicate(_,_,P,P):-atomic(P).
p_parser_tag_predicate(This,Eqs,P,Pd):-not(atomic(P)),P=..[Nm|Args],::(This,'tag_atoms',T59),add_functor_args(T59,[Eqs],F),maplist(F,Args,Args2),Pd=..[Nm|Args2].
p_parser_is_named_member(_,X,[_=Y|_]):-X==Y.
p_parser_is_named_member(This,X,[_|YS]):-p_parser_is_named_member(This,X,YS).
p_parser_name_dont_care(This,Eqs,X):-p_parser_is_named_member(This,X,Eqs).
p_parser_name_dont_care(_,_,'_').
p_parser_tag_atoms(This,Eqs,X,var(X)):-var(X),'!',p_parser_name_dont_care(This,Eqs,X).
p_parser_tag_atoms(_,_,X,atom(X)):-atomic(X),'!'.
p_parser_tag_atoms(This,Eqs,P,compound(P2)):-functor(P,_,_),P=..[Nm|Args],::(This,'tag_atoms',T60),add_functor_args(T60,[Eqs],F),maplist(F,Args,Args2),P2=..[Nm|Args2].
p_not_unified(A,B):-not(unifiable(A,B,[])).
p_comma_list_append([],P,P).
p_comma_list_append([X|Xs],P, (X,Pd)):-p_comma_list_append(Xs,P,Pd).
p_flatten_single([],[]).
p_flatten_single([ (V,NstLs)|Rest],Res):-p_flatten_single(Rest,Restd),append(NstLs,[V|Restd],Res).
p_flatten_pair([],[],[]).
p_flatten_pair([ (V,NstLs)|Rest],[V|RestV],Res):-p_flatten_pair(Rest,RestV,RestNstLs),append(NstLs,RestNstLs,Res).
p_bind_vars_to_name([X=X|XS]):-p_bind_vars_to_name(XS).
p_bind_vars_to_name([]).
:-['../src/utils/utils.pl'].
class_def_scope(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T61), (T61=none;call(T61,T,scope,['Prefix','Parent','Hdr','CS','Top','Defs'],[ (get_name,p_scope_get_name), (get_field_name,p_scope_get_field_name), (get_field_name_qoute,p_scope_get_field_name_qoute), (get_defined_names,p_scope_get_defined_names), (new_scope,p_scope_new_scope), (new_class_scope,p_scope_new_class_scope), (is_top,p_scope_is_top), (dump,p_scope_dump), (lookup,p_scope_lookup), (lookup_here,p_scope_lookup_here), (lookup_nesting,p_scope_lookup_nesting), (lookup_parent,p_scope_lookup_parent), (scope_of,p_scope_scope_of), (get_parent_scope,p_scope_get_parent_scope), (pred_is_constructor,p_scope_pred_is_constructor), (is_constructor,p_scope_is_constructor), (is_not_constructor,p_scope_is_not_constructor), (get_constructors,p_scope_get_constructors), (get_non_constructors,p_scope_get_non_constructors), (get_qouted_fields,p_scope_get_qouted_fields), (get_pred_mods,p_scope_get_pred_mods), (pred_has_mod,p_scope_pred_has_mod), (pred_to_cononical_name,p_scope_pred_to_cononical_name), (pred_cls,p_scope_pred_cls), (get_mod_conflict,p_scope_get_mod_conflict), (any_mod_conflict,p_scope_any_mod_conflict), (error_checks,p_scope_error_checks)],none,none,p_scope_scope)).
p_scope_scope(This,T62):- ::(This,'Defs',T62),class_def_class_header_token(T64),::(This,'Hdr',T65),build_class(T64,T65,[]),::(T64,'Constructor',T63), (T63=none;call(T63,T65,'',[])),::(This,'Parent',T66),T66='top',::(This,'Top',T67),T67='true',p_scope_new_scope(This,'',''),p_scope_error_checks(This).
p_scope_scope(This,T68,T69,T70):- ::(This,'Defs',T68),::(This,'Hdr',T69),::(This,'Parent',T70),::(This,'Top',T71),T71='false',::(T69,'Name',T72),::(T70,'Prefix',T73),p_scope_new_scope(This,T72,T73),p_scope_error_checks(This).
p_scope_get_name(_,class_def(Def),class(T74)):- ::(Def,'Hdr',T75),::(T75,'Name',T74).
p_scope_get_name(_,fact(Def),pred(T76,T77)):- ::(Def,'Name',T76),::(Def,'N',T77),T76\=''.
p_scope_get_name(_,rule(Def),pred(T78,T79)):- ::(Def,'Name',T78),::(Def,'N',T79),T78\=''.
p_scope_get_name(_,field(Name),field(Name)).
p_scope_get_field_name(_,field(Name),Name).
p_scope_get_field_name_qoute(This,X,Y):-p_scope_get_field_name(This,X,Y).
p_scope_get_defined_names(_,[],[]).
p_scope_get_defined_names(This,[D|Ds],[N|Ns]):-p_scope_get_name(This,D,N),'!',p_scope_get_defined_names(This,Ds,Ns).
p_scope_get_defined_names(This,[_|Ds],Ns):-p_scope_get_defined_names(This,Ds,Ns).
p_scope_new_scope(This,Name,ParentPrefix):- ::(This,'Prefix',T80),atomic_list_concat(['_',Name,ParentPrefix],T80),::(This,'new_class_scope',T81),::(This,'Defs',T82),::(This,'CS',T83),filter(T81,T82,T83).
p_scope_new_class_scope(This,class_def(C),cs(T84,ClassScope)):- ::(C,'Hdr',T85),::(T85,'Name',T84),class_def_scope(T87),::(C,'Bdy',T88),build_class(T87,ClassScope,[]),::(T87,'Constructor',T86), (T86=none;call(T86,ClassScope,T88,T85,This)).
p_scope_is_top(This):- ::(This,'Top',T89),T89='true'.
p_scope_dump(This,IO):- ::(This,'Defs',T90),p_scope_get_defined_names(This,T90,Names),::(IO,'Out',T91),write_standard(T91,defined_names(Names)),write_standard(T91,'.
:- [\'./interpret.pl\'].
:- interpret.
	').
p_scope_lookup(This,Arg,ScopeRes):-p_scope_lookup_nesting(This,Arg,ScopeRes),'!'.
p_scope_lookup(This,Arg,ScopeRes):-p_scope_lookup_parent(This,Arg,ScopeRes).
p_scope_lookup_here(This,Arg,This):- ::(This,'Defs',T92),member(Def,T92),p_scope_get_name(This,Def,Arg).
p_scope_lookup_nesting(This,Arg,Res):-not(p_scope_is_top(This)),::(This,'Parent',T94),::(T94,'lookup_nesting',T93),call(T93,Arg,Res).
p_scope_lookup_nesting(This,Arg,Res):-p_scope_lookup_here(This,Arg,Res).
p_scope_lookup_parent(This,Arg,Res):-not(p_scope_is_top(This)),::(This,'Hdr',T96),::(T96,'Parents',T95),[P]=T95,p_scope_get_parent_scope(This,P,PScope),::(PScope,'lookup_parent',T97),call(T97,Arg,Res).
p_scope_lookup_parent(This,Arg,Res):-p_scope_lookup_here(This,Arg,Res).
p_scope_scope_of(This,Pname,Scp):- ::(This,'CS',T98),member(cs(Pname,Scp),T98).
p_scope_get_parent_scope(This,Pname,Scp):- (p_scope_lookup(This,class(Pname),PDefScope),'!';throw_atoms(['Could not find a class of name ',Pname,'.'])),::(PDefScope,'scope_of',T99), (call(T99,Pname,Scp),'!';throw_atoms([Pname,' is not a class.'])).
p_scope_pred_is_constructor(This,pred(T100,_)):- ::(This,'Hdr',T101),::(T101,'Name',T100),T100\=''.
p_scope_is_constructor(This,Def,pred(T102,N)):- ::(This,'Hdr',T103),::(T103,'Name',T102),p_scope_get_name(This,Def,pred(T102,N)),T102\=''.
p_scope_is_not_constructor(This,Def,pred(Nm,N)):-not(p_scope_is_constructor(This,Def,_)),p_scope_get_name(This,Def,pred(Nm,N)).
p_scope_get_constructors(This,Cons):- ::(This,'is_constructor',T104),::(This,'Defs',T105),filter(T104,T105,Cons).
p_scope_get_non_constructors(This,Cons):- ::(This,'is_not_constructor',T106),::(This,'Defs',T107),filter(T106,T107,Cons).
p_scope_get_qouted_fields(This,Fields):- ::(This,'get_field_name_qoute',T108),::(This,'Defs',T109),filter(T108,T109,Fields).
p_scope_get_pred_mods(This,Arg,Mods):- ::(This,'Defs',T110),member(Def,T110),p_scope_get_name(This,Def,Arg),Def=..[_,C],::(C,'Mods',T111),Mods=T111.
p_scope_pred_has_mod(This,Mod,Arg):-p_scope_get_pred_mods(This,Arg,Mods),member(Mod,Mods).
p_scope_pred_to_cononical_name(_,pred(Nm,N),Nm/N).
p_scope_pred_cls(_,rule(RT),RT).
p_scope_pred_cls(_,fact(RT),RT).
p_scope_get_mod_conflict(This,Def,OtherDefs):-p_scope_pred_cls(This,Def,A),p_scope_get_name(This,Def,Nm),member(Def2,OtherDefs),p_scope_get_name(This,Def2,Nm),p_scope_pred_cls(This,Def2,B),::(A,'Mods',T112),::(B,'Mods',T113),T112\=T113.
p_scope_any_mod_conflict(_,[]).
p_scope_any_mod_conflict(This,[X|XS]):-not(p_scope_get_mod_conflict(This,X,XS)),p_scope_any_mod_conflict(This,XS).
p_scope_any_mod_conflict(This,[X|_]):-p_scope_get_name(This,X,Nm),p_scope_pred_to_cononical_name(This,Nm,CNm),throw_atoms(['Predicate ',CNm,' defined with different access modifiers.']).
p_scope_error_checks(This):- ::(This,'Defs',T114),p_scope_any_mod_conflict(This,T114).
p_compile(In,Out,Extras,Opts):-class_def_generator(T116),build_class(T116,G,[]),::(T116,'Constructor',T115), (T115=none;call(T115,G,Opts,In,Out,Extras)),::(G,'g_compile',T117),call(T117).
p_quick(X,Extras,Opts):-atom_concat(X,'.oopl',X1),atom_concat(X,'.out.pl',X2),p_compile(X1,X2,Extras,Opts).
p_quick(X,Opts):-p_quick(X,'',Opts).
p_quick(X):-p_quick(X,[]).
p_generic_compile(In,Out):-p_generic_compile(In,Out,[]).
p_generic_compile(In,Out,Opts):-p_compile(In,Out,':- [\'./standard.out.pl\'].
',Opts).
p_quick_generic(In):-atom_concat(In,'.oopl',X1),atom_concat(In,'.out.pl',X2),p_generic_compile(X1,X2).
class_def_generator(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T118), (T118=none;call(T118,T,generator,['Opts','TopScope','Extras','InFile','OutFile','IO','TNext'],[ (next_tmp,p_generator_next_tmp), (g_compile,p_generator_g_compile), (pair_predicate,p_generator_pair_predicate), (find_parents_definitions,p_generator_find_parents_definitions), (make_super_call,p_generator_make_super_call), (predicate_inherit,p_generator_predicate_inherit), (find_class_definitions,p_generator_find_class_definitions), (not_same_pred_name,p_generator_not_same_pred_name), (get_unique_predicates,p_generator_get_unique_predicates), (none_if_empty,p_generator_none_if_empty), (make_class,p_generator_make_class), (append_if_not_blank,p_generator_append_if_not_blank), (make_predicate_name,p_generator_make_predicate_name), (make_class_type_functor,p_generator_make_class_type_functor), (make_class_type_args,p_generator_make_class_type_args), (make_class_type_body,p_generator_make_class_type_body), (expand_new,p_generator_expand_new), (make_from_fact,p_generator_make_from_fact), (load_scope,p_generator_load_scope), (generate_defs_top,p_generator_generate_defs_top), (generate_defs,p_generator_generate_defs), (resolve_arg_new,p_generator_resolve_arg_new), (resolve_arg,p_generator_resolve_arg), (resolve_args,p_generator_resolve_args), (goal_funcs,p_generator_goal_funcs), (contin_change,p_generator_contin_change), (resolve_args_comma_functor,p_generator_resolve_args_comma_functor), (add_this,p_generator_add_this), (generate_def,p_generator_generate_def), (compile_exception,p_generator_compile_exception)],none,none,p_generator_generator)).
p_generator_next_tmp(This,T):- ::(This,'TNext',T119),arg(1,T119,N),Ni is N+1,setarg(1,T119,Ni),atom_concat('T',N,T).
p_generator_generator(This,T120,T121,T122,T123):- ::(This,'Opts',T120),::(This,'InFile',T121),::(This,'OutFile',T122),::(This,'Extras',T123),::(This,'TNext',T124),T124=cntr(0).
p_generator_g_compile(This):- ::(This,'InFile',T125),open(T125,'read',InS),class_def_io(T127),build_class(T127,IO1,[]),::(T127,'Constructor',T126), (T126=none;call(T126,IO1,InS,'user_output')),class_def_parser(T129),build_class(T129,P,[]),::(T129,'Constructor',T128), (T128=none;call(T128,P,IO1)),::(P,'parse_until_eof',T130),call(T130,Defs),'!',close(InS),::(This,'OutFile',T131),open(T131,'write',OutS),::(This,'Extras',T132),write_standard(OutS,T132),::(This,'IO',T134),build_class(T127,T134,[]),::(T127,'Constructor',T133), (T133=none;call(T133,T134,'user_input',OutS)),p_generator_generate_defs_top(This,Defs),'!',close(OutS).
p_generator_pair_predicate(This,Scope,Name, (Name,Resolved)):-p_generator_make_predicate_name(This,Name,Scope,Resolved).
p_generator_find_parents_definitions(_,_,[],[],[],[]).
p_generator_find_parents_definitions(This,Scope,[Parent],Fields,Predicates,SuperPreds):- ::(Scope,'get_parent_scope',T135),call(T135,Parent,ParentScope),::(ParentScope,'Hdr',T136),p_generator_find_class_definitions(This,ParentScope,T136,Fields,Predicates,SuperPreds).
p_generator_make_super_call(_,CRes,PRes,N, (L:-R)):-tmp_lst(N,Ts),L=..[CRes,'This'|Ts],R=..[PRes,'This'|Ts].
p_generator_predicate_inherit(_,[],TotalPairs,TotalPairs,[]).
p_generator_predicate_inherit(This,[ (pred(CName,NC),CRes)|Ps],ParentPs,TotalPairs,[Sup|SuperPreds]):-member((pred(CName,NP),PRes),ParentPs),'!',delete(ParentPs, (pred(CName,NP),PRes),ParentPsWO),p_generator_make_super_call(This,CRes,PRes,NP,Sup),p_generator_predicate_inherit(This,[ (pred(CName,NC),CRes)|Ps],ParentPsWO,TotalPairs,SuperPreds).
p_generator_predicate_inherit(This,[ (CName,CRes)|Ps],ParentPs,[ (CName,CRes)|TotalPairs],SuperPreds):-p_generator_predicate_inherit(This,Ps,ParentPs,TotalPairs,SuperPreds).
p_generator_find_class_definitions(This,Scope,Hchild,Fields,Predicates,SuperPreds):- ::(Scope,'get_qouted_fields',T137),call(T137,ThisFields),::(Scope,'get_non_constructors',T138),call(T138,AllPreds),::(Scope,'pred_has_mod',T139),add_functor_args(T139,['static'],PHM),partition(PHM,AllPreds,_,NonStaticPreds),::(This,'pair_predicate',T140),add_functor_args(T140,[Scope],F),filter(F,NonStaticPreds,ThisPredicates),::(Hchild,'Parents',T141),p_generator_find_parents_definitions(This,Scope,T141,ParentFields,ParentPredicates,ParentSuperPreds), (intersection(ThisFields,ParentFields,[]);throw('Cannot redefine a parents field.')),append(ParentFields,ThisFields,Fields),p_generator_predicate_inherit(This,ThisPredicates,ParentPredicates,Predicates,ThisSuperPreds),append(ParentSuperPreds,ThisSuperPreds,SuperPreds).
p_generator_not_same_pred_name(_,Nm, (pred(Nm2,N),Res), (pred(Nm2,N),Res)):-Nm\=Nm2.
p_generator_get_unique_predicates(_,[],[]).
p_generator_get_unique_predicates(This,[ (pred(Nm,_),Res)|Ps],[ (Nm,Res)|OPs]):- ::(This,'not_same_pred_name',T142),add_functor_args(T142,[Nm],F),filter(F,Ps,FPs),p_generator_get_unique_predicates(This,FPs,OPs).
p_generator_none_if_empty(_,_,[],'none').
p_generator_none_if_empty(This,Scope,[X|_],Y):-p_generator_make_predicate_name(This,X,Scope,Y).
p_generator_make_class(This,Def,ScopeAbove,Scope,SuperPreds):- ::(Def,'Hdr',T143),p_generator_find_class_definitions(This,Scope,T143,Fields,PredicatesPaired,SuperPreds),p_generator_get_unique_predicates(This,PredicatesPaired,UPredicates),::(Scope,'get_non_constructors',T144),call(T144,NonConstructors),::(Scope,'pred_has_mod',T145),add_functor_args(T145,['static'],PHM),partition(PHM,NonConstructors,Statics,_),::(This,'pair_predicate',T146),add_functor_args(T146,[Scope],F),maplist(F,Statics,StaticsPaired),p_generator_get_unique_predicates(This,StaticsPaired,UStaticsPaired),::(T143,'Name',T147),p_generator_make_class_type_functor(This,T147,ScopeAbove,Func),p_generator_make_class_type_args(This,ScopeAbove,Args),::(Scope,'get_constructors',T148),call(T148,Constructors),p_generator_none_if_empty(This,Scope,Constructors,Constructor),p_generator_make_class_type_body(This,T147,Fields,UPredicates,Bdy,Constructor,UStaticsPaired),P=..[Func|Args],::(This,'IO',T150),::(T150,'Out',T149),write_standard(T149, (P:-Bdy)),write(T149,'.
').
p_generator_append_if_not_blank(_,Prefix,PredicateName,Result):-Prefix\='',atomic_list_concat(['p',Prefix,PredicateName],Result).
p_generator_append_if_not_blank(_,'',PredicateName,PredicateName).
p_generator_make_predicate_name(This,pred(Nm,_),_,Nm):- ::(This,'Opts',T151),member('no_munge',T151),'!'.
p_generator_make_predicate_name(_,pred('',_),_,'').
p_generator_make_predicate_name(This,pred(PredicateName,_),Scope,Result):-PredicateName\='',::(Scope,'Prefix',T152),p_generator_append_if_not_blank(This,T152,PredicateName,Result).
p_generator_make_class_type_functor(_,ClassName,Scope,Result):- ::(Scope,'Prefix',T153),atomic_list_concat(['class_def',T153,ClassName],Result).
p_generator_make_class_type_args(This,Scope,Bdy):-p_generator_add_this(This,['T'],Scope,Bdy).
p_generator_make_class_type_body(This,Name,Fields,Predicates,Bdy,Constructor,Statics):-maplist('add_qoutes',Fields,QFields),p_generator_next_tmp(This,T),p_generator_expand_new(This,new('X','T',Name,QFields,Predicates,'none','none',Constructor),T,Statics,NewComs),list_to_comma_functor([class_def__classType('X')|NewComs],Bdy).
p_generator_expand_new(This,Command,Con,Result):-p_generator_expand_new(This,Command,Con,[],Result).
p_generator_expand_new(_,Command,Con,Statics,[build_class(Type,Class,Statics),::(Type,Constructor,Con), (Con='none';Call)]):-Command=..['new',Type,Class|Args],add_qoutes('Constructor',Constructor),Call=..['call',Con,Class|Args].
p_generator_make_from_fact(_,Nm,ArgLst,[],Res):-Res=..[Nm|ArgLst].
p_generator_make_from_fact(_,Nm,ArgLst,Commands, (P1:-P2)):-Commands\=[],P1=..[Nm|ArgLst],list_to_comma_functor(Commands,P2).
p_generator_load_scope(_,Names,Scope):-class_def_scope(T155),build_class(T155,Scope,[]),::(T155,'Constructor',T154), (T154=none;call(T154,Scope,'__',Names,'top','top',[])).
p_generator_generate_defs_top(This,Defs):-class_def_scope(T157),::(This,'TopScope',T158),build_class(T157,T158,[]),::(T157,'Constructor',T156), (T156=none;call(T156,T158,Defs)),p_generator_generate_defs(This,Defs,T158),::(This,'Opts',T159),::(T158,'dump',T160),::(This,'IO',T161), (member('no_interpret',T159);call(T160,T161)).
p_generator_generate_defs(This,[Def|Defs],Scope):-p_generator_generate_def(This,Def,Scope),p_generator_generate_defs(This,Defs,Scope).
p_generator_generate_defs(_,[],_).
p_generator_resolve_arg_new(This,Scope,Reps,RepsOut,compound(P),AllCommands):-functor(P,'new',_),'!',p_generator_next_tmp(This,Tmp),p_generator_expand_new(This,P,tmp(Tmp),Commands),p_generator_resolve_args(This,Scope,Reps,RepsOut,Commands,Commands1,Commands2,'false'),append(Commands2,Commands1,AllCommands).
p_generator_resolve_arg(_,_,Reps,Reps,T,Td,[],_):-member((T,Td),Reps),'!'.
p_generator_resolve_arg(This,Scope,Reps,Reps3,compound(Z),Tmp,Cmds4,_):-functor(Z,'::',2),arg(1,Z,X),arg(2,Z,Y),'!',p_generator_next_tmp(This,Tmp),p_generator_resolve_arg(This,Scope,[ (compound(Z),Tmp)|Reps],Reps2,X,Xd,Cmds1,'false'),p_generator_resolve_arg(This,Scope,Reps2,Reps3,Y,Yd,Cmds2,'false'),append(Cmds1,Cmds2,Cmds3),append(Cmds3,[::(Xd,Yd,Tmp)],Cmds4).
p_generator_resolve_arg(This,Scope,Reps,Reps,atom(X),P,[],'true'):- ::(Scope,'lookup',T162),call(T162,pred(X,0),PredScope),'!',p_generator_make_predicate_name(This,pred(X,0),PredScope,Nm),p_generator_add_this(This,[],PredScope,pred(X,0),Args),P=..[Nm|Args].
p_generator_resolve_arg(This,Scope,Reps,[ (atom(X),Tmp)|Reps],atom(X),Tmp,[C],_):- ::(Scope,'lookup',T163),call(T163,class(X),DefScp),'!',p_generator_next_tmp(This,Tmp),p_generator_make_class_type_functor(This,X,DefScp,F),p_generator_add_this(This,[Tmp],DefScp,Ls),C=..[F|Ls].
p_generator_resolve_arg(_,_,R,R,atom(X),Y,[],_):-'!',add_qoutes(X,Y).
p_generator_resolve_arg(This,Scope,Reps,[ (var(Arg),Tmp)|Reps],var(Arg),Tmp,[::('This',ArgQ,Tmp)],_):- ::(Scope,'lookup',T164),call(T164,field(Arg),_),'!',add_qoutes(Arg,ArgQ),p_generator_next_tmp(This,Tmp).
p_generator_resolve_arg(_,_,R,R,var(Arg),Arg,[],_):-'!'.
p_generator_resolve_arg(_,_,R,R,tmp(Arg),Arg,[],_):-'!'.
p_generator_resolve_arg(This,Scope,Reps,RepsOut,compound(P),Pd,Commands,'true'):-P=..[Nm|Args],length(Args,N),::(Scope,'lookup',T165),call(T165,pred(Nm,N),PredScope),'!',::(PredScope,'pred_is_constructor',T166), (not(call(T166,pred(Nm,N)));throw('Cannot directly call a constructor.')),p_generator_make_predicate_name(This,pred(Nm,N),PredScope,NewName),p_generator_resolve_args(This,Scope,Reps,RepsOut,Args,Args2,Commands,'false'),p_generator_add_this(This,Args2,PredScope,pred(Nm,N),Args3),Pd=..[NewName|Args3].
p_generator_resolve_arg(This,Scope,Reps,RepsOut,compound(P),Pd,Commands,Chge):-P=..[Nm|Args],p_generator_contin_change(This,Nm,Chge,Chge2),p_generator_resolve_args(This,Scope,Reps,RepsOut,Args,Argsd,Commands,Chge2),Pd=..[Nm|Argsd].
p_generator_resolve_arg(This,Scope,Reps,RepsOut,P,Pd,Commands,Chge):-functor(P,Nm,_),Nm\='compound',p_generator_resolve_arg(This,Scope,Reps,RepsOut,compound(P),Pd,Commands,Chge).
p_generator_resolve_args(This,Scope,Reps,RepsOut2,[Arg|Args],[NewArg|NewArgs],AllCommands,Chge):-p_generator_resolve_arg(This,Scope,Reps,RepsOut,Arg,NewArg,Commands,Chge),p_generator_resolve_args(This,Scope,RepsOut,RepsOut2,Args,NewArgs,Commands2,Chge),append(Commands,Commands2,AllCommands).
p_generator_resolve_args(_,_,R,R,[],[],[],_).
p_generator_goal_funcs(_,[',',';','not']).
p_generator_contin_change(_,_,'false','false').
p_generator_contin_change(This,Nm,'true','true'):-p_generator_goal_funcs(This,L),member(Nm,L).
p_generator_contin_change(This,Nm,'true','false'):-p_generator_goal_funcs(This,L),not(member(Nm,L)).
p_generator_resolve_args_comma_functor(This,compound(B),Scope,Reps,RepsOut2,Res):-functor(B,',',_),'!',arg(1,B,Arg),arg(2,B,Bdy), (p_generator_resolve_arg_new(This,Scope,Reps,RepsOut,Arg,CommandsNew),'!',append(CommandsNew,Res1,Res);p_generator_resolve_arg(This,Scope,Reps,RepsOut,Arg,NewArg,Commands1,'true'),append(Commands1,[NewArg|Res1],Res)),p_generator_resolve_args_comma_functor(This,Bdy,Scope,RepsOut,RepsOut2,Res1).
p_generator_resolve_args_comma_functor(This,B,Scope,Reps,RepsOut,Res):-functor(B,Nm,_),Nm\=',', (p_generator_resolve_arg_new(This,Scope,Reps,RepsOut,B,Res),'!';p_generator_resolve_arg(This,Scope,Reps,RepsOut,B,Bd,Commands,'true'),append(Commands,[Bd],Res)).
p_generator_add_this(This,N,Ls,Scope,Pred,Ls2):-N>=1,p_generator_add_this(This,Ls,Scope,Pred,Ls2).
p_generator_add_this(_,0,Ls,Scope,Pred,Ls):- ::(Scope,'is_top',T167),::(Scope,'pred_has_mod',T168), (call(T167);call(T168,'static',Pred)),'!'.
p_generator_add_this(_,0,Ls,_,_,['_'|Ls]).
p_generator_add_this(_,Ls,Scope,Pred,Ls):- ::(Scope,'is_top',T169),::(Scope,'pred_has_mod',T170), (call(T169);call(T170,'static',Pred)),'!'.
p_generator_add_this(_,Ls,_,_,['This'|Ls]).
p_generator_add_this(_,Ls,Scope,Ls):- ::(Scope,'is_top',T171),call(T171),'!'.
p_generator_add_this(_,Ls,_,['This'|Ls]).
p_generator_generate_def(This,fact(FT),Scope):- ::(FT,'Left',T172),T172=..[Nm|ArgLstIn],p_generator_resolve_args(This,Scope,[],_,ArgLstIn,ArgLst,Commands,'false'),::(FT,'N',T173),p_generator_make_predicate_name(This,pred(Nm,T173),Scope,NewName),count_occurences('This', (Commands,ArgLst),Ns),p_generator_add_this(This,Ns,ArgLst,Scope,pred(Nm,T173),ArgLst2),p_generator_make_from_fact(This,NewName,ArgLst2,Commands,Res),::(This,'IO',T175),::(T175,'Out',T174),write_standard(T174,Res),write(T174,'.
').
p_generator_generate_def(This,rule(RT),Scope):- ::(RT,'Left',T176),T176=..[Nm|ArgLstIn],p_generator_resolve_args(This,Scope,[],OutReps,ArgLstIn,ArgLst,Commands,'false'),::(RT,'Right',T177),p_generator_resolve_args_comma_functor(This,T177,Scope,OutReps,_,Res),append(Commands,Res,Body),::(RT,'N',T178),p_generator_make_predicate_name(This,pred(Nm,T178),Scope,NewName),count_occurences('This', (Body,ArgLst),Ns),p_generator_add_this(This,Ns,ArgLst,Scope,pred(Nm,T178),ArgLst2),Pd=..[NewName|ArgLst2],list_to_comma_functor(Body,CBdy),::(This,'IO',T180),::(T180,'Out',T179),write_standard(T179, (Pd:-CBdy)),write(T179,'.
').
p_generator_generate_def(This,class_def(C),Scope):- ::(Scope,'scope_of',T181),::(C,'Hdr',T183),::(T183,'Name',T182),call(T181,T182,NewScope),p_generator_make_class(This,C,Scope,NewScope,Supers),::(C,'Bdy',T184),p_generator_generate_defs(This,T184,NewScope),::(This,'IO',T186),::(T186,'Out',T185),write_list(T185,Supers).
p_generator_generate_def(This,field(Nm),Scope):- ::(Scope,'is_top',T187), (not(call(T187));p_generator_compile_exception(This,['Field \'',Nm,'\' defined outside of a class.'])).
p_generator_compile_exception(_,Ms):-throw_atoms(Ms).
