% ==========================================================
% LaTeX stuff
% ==========================================================

% ==========================================================
% Top Level
% ==========================================================

latex_output(N,Meaning,Proof,Con,Subst,NV) :-
     latex_output_format(Format),
   ( Format = none    -> true 
   ; Format = nd      -> latex_nd_output(N,Meaning,Proof,Con,Subst,NV)
   ; Format = fitch   -> nd_to_fitchlist(Proof,1,Max,FH,[],FL,[]),
                         latex_fitch_output(N,Meaning,Max,FL,FH,Con,Subst,NV)
   ; told,
     format('Undefined LaTeX output format: ~p~n',[Format]),
     fail
   ).

% ============================================================
% Natural Deduction, Prawitz style
% ============================================================

% =

latex_nd_output(N,Meaning,rule(Name,A,S,Sem,Rs),Con,Subst,NV) :-
        format('~n%~n% ~p~n%~n~n{\\samepage~n~n\\ensuremath{',[Meaning]),
        write_nd(Rs,Name,A,S,Sem,Con,Subst,NV,0),
        format('}~n~n\\vspace{4mm}~n~n',[]),
        format('{\bf ~p. }\\mbox{$',[N]),
        write_sem(Meaning),
        format('$}~n}~n~n',[]).

write_nd([],Name,A,S,Sem,Con,Subst,NV,Tab0) :-
        Tab is Tab0 + 3,
        format('~n~*|',[Tab]),
       ( Name=lex,
         compact_lex(yes) ->
	 write('\\infer{'),
	 write_sem(Sem,'',' : ',Subst,NV),
	 write_type(S),
	 write('}{'),
	 write_label(A,'','',Con),
	 write('}')
       ;
       (Name=hyp(_), 
        hypo_scope(yes) ->
        write(' \bo ')
       ;
        true),
        write_label(A,'',' \\vdash ',Con),
        write_sem(Sem,'',' : ',Subst,NV),
        write_type(S),
       (Name=hyp(N),
        hypo_scope(yes) ->
        format(' \bc^{~p} ',[N])
       ;
        true)
       ).

write_nd([R|Rs],N,A,S,Sem,Con,Subst,NV,Tab0) :-
        Tab is Tab0 + 3,
       (boring(N,[R|Rs]) ->
        format('~n~*|\\infer{',[Tab]),
        write_label(A,'',' \vdash ',Con),
        write_sem(Sem,'',' : ',Subst,NV),
        write_type(S),
        write('}{ \\cdots }')
       ;
        format('~n~*|\\infer[',[Tab]),
        write_nd_rule_name(N),
        write(']{'),
        write_label(A,'',' \\vdash ',Con),
        write_sem(Sem,'',' : ',Subst,NV),
        write_type(S),
        write('}{'),
        write_nds(Rs,R,Con,Subst,NV,Tab),
        format('~n~*|}',[Tab])
       ).

% write_nds(+ListOfNDs)
% write a list of NDs

write_nds([],rule(N,A,S,Sem,Rs0),Con,Subst,NV,Tab) :-
        write_nd(Rs0,N,A,S,Sem,Con,Subst,NV,Tab).
write_nds([R|Rs],rule(N,A,S,Sem,Rs0),Con,Subst,NV,Tab) :-
        write_nd(Rs0,N,A,S,Sem,Con,Subst,NV,Tab),
        format('~n~*|& ',[Tab]),
        write_nds(Rs,R,Con,Subst,NV,Tab).

% =

write_nd_rule_name(Rule) :-
        rule_name(Rule,Name),
        format('\\bo ~p \\bc^{',[Name]),
        rule_discharges(Rule,Xs),
        write_indices(Xs),
        write('}').

rule_discharges(dli(_,X),[X]) :- !.
rule_discharges(dri(_,X),[X]) :- !.
rule_discharges(pe(_,X,Y),[X,Y]) :- !.
rule_discharges(diae(_,X),[X]) :- !.
rule_discharges(_,[]).

write_indices([]).
write_indices([Y|Ys]) :-
        sort([Y|Ys],[Z|Zs]),
        write_indices1(Zs,Z).

write_indices1([],Z) :- 
        print(Z).
write_indices1([X|Xs],Z) :-
        format('~p,',[Z]),
        write_indices1(Xs,X).

% ============================================================
% Natural Deduction, Fitch style
% ============================================================

% =

latex_fitch_output(N,Meaning,Max,RFL,FH,Con,Subst,NV) :-
     format('~n% ~p~n%~n~n\begin{center}~n\begin{tabular}{rll}~n',[Meaning]),
     reverse(RFL,[],FL),
     write_fitch(FL,[],Max,FH,Con,Subst,NV),
     format('~n\\end{tabular}~n\\end{center}~n\\vspace{4mm}~n\\textbf{ ~p. }\\mbox{$',[N]),
     write_sem(Meaning),
     format('$~n}~n~n',[]).

nd_to_fitchlist(rule(Name,A,S,Sem,R),N0,N,FH0,FH,[N0-frule(Name,A,S,Sem,Link)|L0],L) :-
       (Name=hyp(_) ->
        FH0=[N0-frule(Name,A,S,Sem,Link)|FH1]
       ;
        FH1=FH0
       ),
        N1 is N0+1,
        nds_to_fitchlist(R,N1,N,FH1,FH,L0,L,[],Link).

nds_to_fitchlist([],N,N,FH,FH,L,L,Ln,Ln).
nds_to_fitchlist([X|Xs],N0,N,FH0,FH,L0,L,Ln0,Ln) :-
        nds_to_fitchlist(Xs,N0,N1,FH0,FH1,L0,L1,[N1|Ln0],Ln),
        nd_to_fitchlist(X,N1,N,FH1,FH,L1,L).

write_fitch([],_,_,_,_,_,_).

write_fitch([N0-frule(Name,A,T,Sem,Rs0)|Rest],Ind0,Max,FH,Con,Subst,NV) :-
        N is Max-N0,
        format(' $ ~p. $ & $ ',[N]),
        update_indent(Name,Ind0,Ind),
        write_indent(Ind),
        write_label(A,'',':',Con),
        write_type(T),
        write_sem(Sem,'-','',Subst,NV),
        write(' $ & $'),
        write_fitch_rule_name(Name,Rs0,Max,FH),
        write('$ \\[-2.2ex]'),nl,
        write_fitch(Rest,Ind,Max,FH,Con,Subst,NV).

update_indent(hyp(N)   ,Ind,[N|Ind]) :- !.
update_indent(pe(_,N,M),Ind0,Ind) :- 
        !,
        update_list(Ind0,N,Ind1),
        update_list(Ind1,M,Ind). 
update_indent(dli(_,N) ,Ind0,Ind) :- 
        !,
        update_list(Ind0,N,Ind).
update_indent(dri(_,N) ,Ind0,Ind) :- 
        !,
        update_list(Ind0,N,Ind).
update_indent(diae(_,N),Ind0,Ind) :- 
        !,
        update_list(Ind0,N,Ind).
update_indent(_        ,Ind ,Ind).

write_indent(N0) :-
       (hypo_scope(yes) ->
        reverse(N0,[],N)
       ;
        N=[]),
        write_indent1(N).

write_indent1([]) :- 
        !,
        write('\\rule{0ex}{4ex} ').
write_indent1([X|Xs]) :-
       (X=x -> N = 0 ; N = 3),
        format('\\rule[-1ex]{.1ex}{~pex} \\ ',[N]),
        write_indent1(Xs).

update_list([],_,[]).
update_list([N|Xs],N,Ys) :- 
        !,
        remove_xs(Xs,Ys).
update_list(Xs,N,Zs) :-
        update_list1(Xs,N,Ys),
        remove_xs(Ys,Zs).

update_list1([],_,[]).
update_list1([N|Rest],N,[x|Rest]) :-
        !.
update_list1([N|Rest0],M,[N|Rest]) :-
        update_list1(Rest0,M,Rest).

remove_xs([x|Xs],Ys) :-
        !,
        remove_xs(Xs,Ys).
remove_xs(Xs,Xs).

% =

write_fitch_rule_name(Rule,Rs0,Max,FH) :-
        rule_name(Rule,Name),
        format('~p \\ ',[Name]),
        rule_discharges(Rule,Nums),
        number_hypos(Nums,FH,Rs0,Rs),
        write_numbers(Rs,Max).

number_hypos([],_,Rs,Rs).
number_hypos([X|Xs],FH,Rs0,Rs) :-
        member_check(N-frule(hyp(X),_,_,_,_),FH),
        number_hypos(Xs,FH,[N|Rs0],Rs).

write_numbers(X,Max) :-
        sort(X,Y),
        reverse(Y,[],Z),
        write_numbers1(Z,Max).

write_numbers1([],_).
write_numbers1([X|Xs],Max) :-
        write(' ( '),
        write_numbers2(Xs,X,Max),
        write(' ) ').

write_numbers2([],X,Max) :-
        Y is Max-X,
        print(Y).
write_numbers2([Y|Ys],X,Max) :-
        Z is Max-X,
        format('~p,',[Z]),
        write_numbers2(Ys,Y,Max).

% =

rule_name(R,N) :-
        rule_name1(R,N),
        !.
rule_name([R|Rs],N) :-
        !,
        rule_list(Rs,R,N).
rule_name(N,N). % N is a structural rule name

rule_name1(lex,'Lex').
rule_name1(uhyp,'Hyp').
rule_name1(hyp(_),'Hyp').
rule_name1(dle(_),'\\bs E').
rule_name1(dre(_),'/ E').
rule_name1(dli(_,_),'\\bs I').
rule_name1(dri(_,_),'/ I').
rule_name1(pe(_,_,_),'\\bullet E').
rule_name1(pi(_),'\\bullet I').
rule_name1(diae(_,_),'\\Diamond E').
rule_name1(diai(_),'\\Diamond I').
rule_name1(boxe(_),'\\Boxd E').
rule_name1(boxi(_),'\\Boxd I').

rule_list([],N,N) :- !.
rule_list([R|Rs],R0,(N0,N)) :-
        rule_name(R0,N0),
        rule_list(Rs,R,N).

text_output(Sem,Pros) :-
       (output_text_sem(yes) ->
        format('~n~n%~n% ~p~n%~n~n',[Sem]),
        write_list_of_labels(Pros),
        write_sem(Sem)
       ;
        true).

write_list_of_labels([L|Ls]) :-
        format('~n\\vspace{2mm}~n\\begin{tabular}{l}~n',[]),
        write_list_of_labels1(Ls,L),
        format('~n\\end{tabular}~n~n\\vspace{2mm}~n~n',[]).

write_list_of_labels1([],Label) :-
        write(' $ '),
        write_label(Label,1,[]),
        write(' $ ').

write_list_of_labels1([Label|Labels],Label0) :-
        write(' $ '),
        write_label(Label0,1,[]),
        write(' $ \\ '),nl,
        write_list_of_labels1(Labels,Label).

% = write_label

write_label(L,P1,P2,Con) :-
       (output_labels(yes) ->
        print(P1),
        write_label(L,1,Con),
        print(P2)
       ;
        true).

% =

write_label(p(I,A,B),N,Con) :-
        !,
        write_bo(N),
        binding(A,p(I,A,B),NA),
        write_label(A,NA,Con),
        write('\\circ_{'),
	write_mode(I),
	write('}'),
        binding(B,p(I,A,B),NB),
        write_label(B,NB,Con),
        write_bc(N).

write_label(zip(I,A),_,Con) :-
        !,
        write('\\langle '),
        write_label(A,1,Con),
        write('\\rangle^{'),
        write_mode(I),
        write('}').

write_label(l(J,D),_,Con) :-
        !,
        member_check(cf(l(J,D),_,'$VAR'(N)),Con),
        write_pros_var(N).

write_label(r(J,D),_,Con) :-
        !,
        member_check(cf(r(J,D),_,'$VAR'(N)),Con),
        write_pros_var(N).

write_label(unzip(J,D),_,Con) :-
        !,
        member_check(cf(unzip(J,D),_,'$VAR'(N)),Con),
        write_pros_var(N).

write_label('$VAR'(N),_,_) :-
        !,
        print('$VAR'(N)).

write_label(_-'$VAR'(N),_,_) :-
        !,
        write_pros_var(N).

write_label(_-A,_,_) :-
        write('\\textrm{'),
       (atom(A) ->
        write_atom(A)
       ;
        print(A)),
        write('}').

write_pros_var(N) :-
        V is N mod 4,
        I is N // 4,
        pros_var_name(V,Name),
        format('\\textrm{~p}_{~p}',[Name,I]).

pros_var_name(0,p).
pros_var_name(1,q).
pros_var_name(2,r).
pros_var_name(3,s).

% ===========================================================

write_type(T0) :-
       (macro_reduce(yes) ->
        macro_reduce(T0,T)
       ;
        T=T0),
        write_type(T,1).

% = write_type0(+Type)
% write a type with outer brackets

write_type(lit(X),N) :- !,write_type(X,N).

write_type(dl(I,A,B),N) :- 
        !, 
        write_bo(N),
        binding(A,dl(I,A,B),NA),
        write_type(A,NA),
        write(' \\bs_{'),
        write_mode(I),
        write('}'),
        binding(B,dl(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).

write_type(dr(I,A,B),N) :- 
        !, 
        write_bo(N),
        binding(A,dr(I,A,B),NA),
        write_type(A,NA),
        write(' /_{'),
        write_mode(I),
        write('}'),
        binding(B,dr(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).
write_type(p(I,A,B),N) :-
        !,
        write_bo(N),
        binding(A,p(I,A,B),NA),
        write_type(A,NA),
        write(' \\bullet_{'),
        write_mode(I),
        write('}'),
        binding(B,p(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).
write_type(dia(I,A),_) :-
        !,
        write('\\Diamond_{'),
        write_mode(I),
        write('}'),
        binding(A,dia(I,A),NA),
        write_type(A,NA).
write_type(box(I,A),_) :-
        !,
        write('\\Boxd_{'),
        write_mode(I),
        write('}'),
        binding(A,box(I,A),NA),
        write_type(A,NA).

/* extra rules for non-atomic macro definitions */

write_type(bang(I,A),_) :-
        !,
        write('!_{'),
        write_mode(I),
        write('}'),
        write_type(A,0).

write_type(q(A,B,C),_) :-
        !,
        write('q('),
        write_type(A,1),
        write(','),
        write_type(B,1),
        write(','),
        write_type(C,1),
        write(')').

write_type(X,_) :-
        /* X is an atomic type */ 
       (atom(X) ->
        write_atom(X)
       ;
        print(X)).

% write_mode(+Mode)

write_mode([]) :- !.
write_mode(one(M)):-!,write(M),write('_{1}').
write_mode(two(M)):-!,write(M),write('_{2}').
write_mode(Atom):-write(Atom).

% ===========================================================

write_sem(Sem0,P1,P2,Subst,NV) :-
       (output_semantics(yes) ->
        (output_subst_lex_sem(yes) ->
         substitute_sem(Subst,Sem0,Sem1,NV,_)
        ;
         Sem1=Sem0),
        (output_reduced_sem(yes) ->
         reduce_sem(Sem1,Sem)
        ;
         Sem=Sem1),
         print(P1),
         write_sem(Sem,1),
         print(P2)
       ;
        true).

write_sem(T) :-
        write_sem(T,1).

write_sem(drs(V,C),_) :-
        !,
        format('\\mbox{~n\\begin{tabular}{|l|} \\hline~n',[]),
        write_list_of_vars(V),
        format(' \\ \\hline~n',[]),
        write_list_of_conds(C),
        format(' \\end{tabular}~n}~n',[]).

write_sem(merge(A,B),N) :-
        !,
        write_bo(N),
        binding(A,merge(A,B),NA),
        write_sem(A,NA),
        write(' \\circ '),
        binding(B,merge(A,B),NB),
        write_sem(B,NB),
        write_bc(N).

write_sem(exists(X),_) :-
        !,
        write('\\exists '),
        write_sem(X).

write_sem(bool(A,C,B),N) :-
        !,
        write_bo(N),
        binding(A,bool(A,C,B),NA),
        write_sem(A,NA),
        write_conn(C),
        binding(B,bool(A,C,B),NB),
        write_sem(B,NB),
        write_bc(N).

write_sem(not(X),_) :-
        !,
        write(' \\neg '),
        binding(X,not(X),NX),
        write_sem(X,NX).

write_sem(quant(Q,X,T),N) :-
        !,
        write_bo(N),
        write_quant(Q),
        write_sem(X,1),
        binding(T,quant(Q,X,T),NT),
        write_sem(T,NT),
        write_bc(N).      

write_sem(lambda(X,V),N) :-
        !,
        write_bo(N),
        write('\\lambda '),
        write_sem(X,1),
        write('.'),
        binding(V,lambda(X,V),NV),
        write_sem(V,NV),
        write_bc(N).

write_sem(appl(F,X),_) :-
        sem_brackets(no) ->
        write_fun_args(F,[X]),
        !.

write_sem(appl(X,Y),_) :-
        !,
        write('('),
        write_sem(X,1),
        write(' \\  '),
        write_sem(Y,1),
        write(')').

write_sem(pair(X,Y),_) :-
        !,
        write(' \\langle '),
        write_sem(X,1),
        write(' , '),
        write_sem(Y,1),
        write(' \\rangle ').

write_sem(fst(X),_) :-
        !,
        write(' \\pi^1'),
        binding(X,fst(X),NX),
        write_sem(X,NX).

write_sem(snd(X),_) :-
        !,
        write(' \\pi^2'),
        binding(X,snd(X),NX),
        write_sem(X,NX).

write_sem(debox(X),_) :-
        !,
        write(' {}^{\\vee} '),
        binding(X,debox(X),NX),
        write_sem(X,NX).

write_sem(dedia(X),_) :-
        !,
        write(' {}^{\\cup} '),
        binding(X,dedia(X),NX),
        write_sem(X,NX).

write_sem(conbox(X),_) :-
        !,
        write(' {}^{\\wedge} '),
        binding(X,conbox(X),NX),
        write_sem(X,NX).

write_sem(condia(X),_) :-
        !,
        write(' {}^{\\cap} '),
        binding(X,condia(X),NX),
        write_sem(X,NX).

write_sem('$VAR'(N),_) :-
        !,
        V is N mod 3,
        I is N // 3,
        sem_var_name(V,Name),
        format('~p_{~p}',[Name,I]).

write_sem(Const,_) :-
        write('\\textbf{'),
       (atom(Const) ->
        write_atom(Const)
       ;
        print(Const)),
        write('}').

sem_var_name(0,x).
sem_var_name(1,y).
sem_var_name(2,z).

% =

write_fun_args(Fun,As) :- 
        atomic_sem(Fun),
        !,
        write_sem(Fun,1),
        write('('),
        reverse(As,[],[B|Bs]),
        write_args(Bs,B),
        write(')').

write_fun_args(appl(X,Y),As) :-
        write_fun_args(X,[Y|As]).

write_args([],A) :-
        write_sem(A,1).
write_args([A|As],A0) :-
        write_sem(A0,1),
        write(' , '),
        write_args(As,A).

% =

write_list_of_vars([]) :- 
        write('$ \\ $').
write_list_of_vars([V|Vs]) :-
        write_list_of_vars(Vs,V).

write_list_of_vars([],V) :-
        write('$'),
        write_sem(V),
        write('$').
write_list_of_vars([V|Vs],V0) :-
        write('$'),
        write_sem(V0),
        write('\\ $'),
        write_list_of_vars(Vs,V).

% =

write_list_of_conds([]) :-
        write('$ \\ $ \\\\ \\hline ').
write_list_of_conds([C|Cs]) :-
        write_list_of_conds(Cs,C).

write_list_of_conds([],C) :-
        write('$'),
        write_sem(C),
        format('$ \\\\ \\hline ',[]).

write_list_of_conds([C|Cs],C0) :-
        write('$'),
        write_sem(C0),
        format('$ \\\\~n ',[]),
        write_list_of_conds(Cs,C).


atomic_sem(At) :- atom(At),!.
atomic_sem('$VAR'(_)).

write_bo(0) :- write('(').
write_bo(1).

write_bc(0) :- write(')').
write_bc(1).

binding(T0,T,N) :-
   (output_expl_brackets(yes) ->
    N=0
   ;
    binds(T0,_,_,Num0),
    binds(T,Ass,Eq,Num),
    bind1(Eq,T0,Ass,Num0,Num,N)
   ),!.

binding(_,_,0).

bind1( _,T,T,M ,M,1) :- !.
bind1(=<,_,_,M0,M,N) :- (M >  M0 -> N = 0 ; N = 1).
bind1( <,_,_,M0,M,N) :- (M >= M0 -> N = 0 ; N = 1).

binds(dia(_,_),n,=<,20).
binds(box(_,_),n,=<,20).
binds(zip(_,_),n,=<,20).
binds(p(I,_,_),p(I,_,_),<,8) :- ignore_brackets(I),!.
binds(p(_,_,_),n,<,8).
binds(dl(_,_,_),n,<,4).
binds(dr(_,_,_),n,<,4).

binds(not(_),n,=<,20).
binds(dedia(_),n,=<,20).
binds(condia(_),n,=<,20).
binds(debox(_),n,=<,20).
binds(conbox(_),n,=<,20).
binds(quant(_,_,_),n,=<,12).
binds(lambda(_,_),n,=<,12).
binds(bool(_,&,_),bool(_,&,_),<,8).
binds(bool(_,\/,_),bool(_,\/,_),<,8).
binds(bool(_,->,_),n,<,4).
binds(merge(_,_),merge(_,_),<,2).

% =

write_quant(exists) :-
     !,
     write(' \\exists ').
write_quant(forall) :-
     !,
     write(' \\forall ').
write_quant(iota) :-
     !,
     write(' \\iota ').
write_quant(X) :-
     write(X).

% =

write_conn(&) :-
     !,
     write(' \\wedge ').
write_conn(\/) :-
     !,
     write(' \\vee ').
write_conn(->) :-
     !,
     write(' \\rightarrow ').
write_conn(neq) :-
     !,
     write(' \\neq ').
write_conn(X) :-
     write(X).

% =

write_atom(At) :-
      write_atom(At,'\\_{}').

write_atom(At,Pr) :-
      name(At,L),
      write_atom_list(L,Pr).

write_atom_list(L,Pr) :-
      split(L,95,L0,[],L1),
      !,
      name(At,L0),
      format('~p~p',[At,Pr]),
      write_atom_list(L1,Pr).
write_atom_list(L,_) :-
      name(At,L),
      print(At).

split([X|Xs],X,Ys,Ys,Xs) :- !.
split([X|Xs],S,[X|Ys0],Ys,Zs) :-
      split(Xs,S,Ys0,Ys,Zs).

% =

boring(N,[R|Rs]) :-
     rule_names([R|Rs],Ns,[]),
     boring([N|Ns]).

boring([]).
boring([N|Ns]) :-
     boring_rule(N),
     boring(Ns).

% =

rule_names([],N,N).
rule_names([rule(Name,_,_,_,Rs1)|Rs],[Name|N0],N) :-
     rule_names(Rs1,N0,N1),
     rule_names(Rs,N1,N).

% =

tail([],X,X,[]).
tail([Y|Ys],Z,X,[Z|Zs]) :-
     tail(Ys,Y,X,Zs).

reverse([],L,L).
reverse([X|Xs],Ys,Zs) :-
     reverse(Xs,[X|Ys],Zs).

/* postscript */

postscript :-
	my_tk_interpreter(I),
	tex_out_dir(TOD),
	tcl_eval(I,format('set filename [tk_getSaveFile -initialdir {~w} -defaultextension .ps -title "Export Postscript..." -filetypes {{"Postscript" {.ps .PS}} {"All Files" *} }]',[TOD]),PSS),
      ( PSS \== [] ->
	append("mv proofs1.ps ",PSS,MVS),
	name(MV,MVS),
	tex_out_dir(Dir),
	current_directory(OldDir,Dir),
	always_succeed(shell('dvips -t landscape proofs1.dvi')),
	always_succeed(shell(MV)),
	current_directory(_,OldDir)
       ;
	   true
       ).

always_succeed(X) :-
      call(X),
      !.
always_succeed(_).

write_post([]).
write_post([postulate(X,Y,Z)|Rest]) :-
         write('\\ensuremath{'),
         write_label(Y,1,[]),
         write('} & \\ensuremath{'),
         write_label(X,1,[]),
         format('} & \\ensuremath{[~p]} \\\\~n',[Z]),
         write_post(Rest).

write_lex([]).
write_lex([lex(A,B0,C)|Rest]) :-
         write('\\ensuremath{'),
         write_label(x-A,1,[]),
         write(' : '),
         macro_expand(B0,B),
         write_type(B),
         write(' - '),
         write_sem(C),
         write('} \\\\'),nl,
         write_lex(Rest).


xdvi_post :- 
         findall(postulate(X,Y,Z),(postulate(X,Y,Z),
                                   numbervars(X,0,_)),L),
         tell_texout('post.tex'),
         format('\\documentclass{article}~n\\usepackage{latexsym}~n\\begin{document}~n\\begin{tabular}{r@{$\\ \\rightarrow\\ $}ll}~n',[]),
         write_post(L),
         format('~n\\end{tabular}~n\\end{document}~n',[]),
         told,
         tex_out_dir(Dir),
         current_directory(OldDir,Dir),
         always_succeed(shell('latex post.tex > /dev/null; xdvi -thorough post &')),
         current_directory(_,OldDir).

xdvi_lex :-
         findall(lex(A,B,C),(lex(A,B,C),
                             numbervars(C,0,_)),L),
         tell_texout('lex.tex'),
         format('\\documentclass{article}~n\\usepackage{latexsym}~n\\newcommand{\\Boxd}{\\Box^{\\downarrow}}~n\\newcommand{\\bs}{\\backslash}~n\\begin{document}~n\\begin{tabular}{l}~n',[]),
         write_lex(L),
         format('~n\\end{tabular}~n\\end{document}~n',[]),
         told,
         tex_out_dir(Dir),
         current_directory(OldDir,Dir),
         always_succeed(shell('latex lex.tex > /dev/null; xdvi -thorough lex &')),
         current_directory(_,OldDir).

xdvi_sent :-
         findall(example(A,B),(example(A,B0),
                               macro_expand(B0,B)),L),
         tell_texout('exa.tex'),
         format('\\documentclass{article}~n\\usepackage{latexsym}~n\\newcommand{\\Boxd}{\\Box^{\\downarrow}}~n\\newcommand{\\bs}{\\backslash}~n\\begin{document}~n\\begin{tabular}{l}~n',[]),
         write_sent(L),
         format('~n\end{tabular}~n\end{document}~n',[]),
         told,
         tex_out_dir(Dir),
         current_directory(OldDir,Dir),
         always_succeed(shell('latex exa.tex > /dev/null; xdvi -thorough exa &')),
         current_directory(_,OldDir).

write_sent([]).
write_sent([example(A0,B)|Rest]) :-
         example_deriv_status(A0,A1,Status),
         example_trim_underscores(A1,A),
         format(' {\em ~s} $~w ',[A,Status]),
         write_type(B),
         format('$ \\ ~n',[]),
         write_sent(Rest).

example_trim_underscores([],[]).
example_trim_underscores([X|Xs],[Y|Ys]) :-
       ( X=95 ->
         Y=32
       ;
         Y=X
       ),
         example_trim_underscores(Xs,Ys).

example_deriv_status([42|R],R,' \\not\\Rightarrow ') :- !.
example_deriv_status([63|R],R,' \\Rightarrow_{?} ') :- !.
example_deriv_status([32|R],R,' \\Rightarrow ') :- !.
example_deriv_status(R,R,' \\Rightarrow ').

make_clean :-
         tex_out_dir(Dir),
         current_directory(OldDir,Dir),
         always_succeed(shell('/usr/bin/rm -f eg.tex proofs1.dvi proofs1.aux proofs1.log proofs1.ps texput.log')),
         always_succeed(shell('/usr/bin/rm -f post.tex post.dvi post.ps post.aux post.log')),
         always_succeed(shell('/usr/bin/rm -f lex.tex lex.dvi lex.ps lex.aux lex.log')),
         always_succeed(shell('/usr/bin/rm -f exa.tex exa.dvi exa.ps exa.aux exa.log')),
         current_directory(_,OldDir).

tex_updated.

%tex_updated :-
%	my_tk_interpreter(I),
%      ( var(I) ->
%	true
%      ;
%        tex_out_dir(Dir),
%        current_directory(OldDir,Dir),
%	absolute_file_name('eg.tex',EG),
%	absolute_file_name('proofs1.dvi',P1),
%        ( file_exists(EG,write) ->
%	    file_exists(P1,write) ->
%	       tcl_eval(I,'expr ([file mtime eg.tex]-[file mtime proofs1.dvi])>0',
%                       } else {
%                       return 1
%                       }
%                       } else {
%                       return 0
%                       }',Return),Return="1"), 
%        current_directory(_,OldDir),
%        Return = "1"
%      ).

set_dvi_output :-
	retractall(latex_command(_)),
	assert(latex_command(latex)),
	retractall(preview_command(_,_)),
	assert(preview_command('xdvi -bd 0 -thorough -paper a4r -mgs 1200 -hush -S 30',"dvi")).

set_ps_output :-
	retractall(latex_command(_)),
	assert(latex_command(latex)),
	retractall(preview_command(_,_)),
	assert(preview_command('dvips proofs1.dvi ; ghostview',"ps")).

set_pdf_output :-
	retractall(latex_command(_)),
	assert(latex_command(pdflatex)),
	retractall(preview_command(_,_)),
	assert(preview_command(acroread,"pdf")).

add_geometry(C0,Ext,C) :-
	'xdvi initialsize'(IX),
	name(C0,NC0),
	append(NC0," -geometry ",NC1),
	append(NC1,IX,NC2),
	append(NC2,"+0+0 proofs1.",NC3),
	append(NC3,Ext,NC4),
	append(NC4," &",NC),
	name(C,NC).

/* xdvi */

xdvi :-
     retex,
     tex_out_dir(Dir),
     current_directory(OldDir,Dir),
     always_succeed((preview_command(C0,Ext),add_geometry(C0,Ext,C),shell(C))),
     current_directory(_,OldDir).

/* latex */

latex :- 
      tex_out_dir(Dir),
      current_directory(OldDir,Dir),
	( file_exists('eg.tex',read) ->
	    latex_command(LaTeX),
	    name(LaTeX,LaTeXS),
	    append(LaTeXS," proofs1.tex | egrep '^\\!.*$'; echo 'LaTeX ready.' &",CommandS),
	    name(Command,CommandS), 
            always_succeed(shell(Command))
	;
	    format('~n{Error: Couldn''t open file eg.tex!}~n',[])
	),
      current_directory(_,OldDir).
