% ============================================================
% Engine
% ============================================================

tex(X,Goal) :-
     tex(X,Goal,_).

% = tex(+ListOfWords,+GoalType,?NumSolutions)

tex(X,Goal,File,Num,_) :-
     my_tk_interpreter(Interp),
     tcl_eval(Interp,'set_cursor watch left_ptr',_),
     retractall('latex output'(_,_,_,_,_,_)),
     telling(Stream),
     init(X,Goal),
     init_db(X,Goal,Meaning0,S1,Subst,NVAR,File,Num,[E|Es],[],TwoRules0,Hyp0,Con0,XOff),
     count_check([E|Es]),
     estimate([E|Es],[],EstL),
     create_estimate_window(Interp,EstL,Max),
     copy_term(t([E|Es],S1,TwoRules0,Hyp0,Con0),
               t([E0|Es0],S0,TwoRules,Hyp,Con1)),
     tcl_eval(Interp,'.stats.txt configure -text "Linking"',_),
     prove(Es,E,Es0,E0,EstL,Max,XOff),
     substitute_sem(Subst,Meaning0,Meaning1,NVAR,_),
     reduce_sem(Meaning1,Meaning),
    ( 
      normalize(S1,S) -> true
    ;
      no_solution(S1,Meaning)
    ),
     found_solution(Meaning,S0,S,N),
    (
      latex_output_format(none) -> true
    ;
      tcl_eval(Interp,'.stats.txt configure -text "Generating Output"',_),
      tcl_eval(Interp,'update idletasks',_),
      my_once((normalize(S0,S,OneRules,[]),
	       generate_nd(Hyp,OneRules,TwoRules,Con1,Con,ND0))),
      assert('latex output'(N,Meaning0,ND0,Con,Subst,NVAR)),
      eta_reduce(ND0,ND1),
      collapse(ND1,ND),
      /* LaTeX output */
      tell_texout('eg.tex'),
      latex_output(N,Meaning,ND,Con,Subst,NVAR),
      tell(Stream)
    ),
     tcl_eval(Interp,'.stats.txt configure -text "Linking"',_),
     fail.

tex(_,_,_,_,N) :-
     my_tk_interpreter(Interp),
     tcl_eval(Interp,'set runningstate "done"\n\
                      set rewritestate "done"\n\
                      set_cursor left_ptr left_ptr\n\
                      .stats.c dtag node\n\
                      grab release .stats\n\
                      if {[winfo exists .stats.ind]} {\n\
                      .stats.ind delete bar\n\
                      }\n\
                      if {[winfo exists .rewrite]} {\n\
                      .rewrite.exit configure -state disabled\n\
                      grab release .rewrite\n\
                      }\n\
                      .stats.exit configure -state disabled\n\
                      focus $oldFocus',_),
     /* Screen */
     write_solutions(N),
     write_statistics,
     /* LaTeX */
     tell_texout('eg.tex'),
     nl,
    ( N>0 -> tcl_eval(Interp,'.stats.txt configure -text "Done"',_),
             told,latex
 
    ; N<0 -> tcl_eval(Interp,'.stats.txt configure -text "Lookup Failed"',_),
             told,fail
    ; tcl_eval(Interp,'.stats.txt configure -text "Failed"',_),
      told
    ).

% retex
% reload options.pl and generate the latex output corresponding
% to the previous query.

retex :-
    ( latex_output_format(none) ->
      format1('~nNo LaTeX output~n',[])
    ;
      ( tex_updated ->
        format1('~nCalling LaTeX...~n',[]),
        retractall('options changed'),
        retex1
      ;
        format1('~nNo changes to eg.tex~n',[])
      )
    ).

retex1 :-
     tell_texout('eg.tex'),
     'latex output'(N,Meaning0,ND0,Con,Subst,NV),
     eta_reduce(ND0,ND1),
     collapse(ND1,ND),  
     substitute_sem(Subst,Meaning0,Meaning1,NV,_),
     reduce_sem(Meaning1,Meaning),
     latex_output(N,Meaning,ND,Con,Subst,NV),
     fail.

retex1 :-
     told,
     latex.

% =

init_db(X,G0,Meaning0,S0,Subst,N,File,SNum,[vertex(0,Bs,Ps)|Es0],Es,Rules0,H0,C0,XOff) :-
      unknown_words(X),
      create_proofnet_window,
      my_tk_interpreter(Interp),
      tcl_eval(Interp,'fontWidget .stats.c create text 0 0 -font $pnfont -text "M"',MItem),
      tcl_eval(Interp,format('.stats.c bbox ~s',[MItem]),BBox),
      quadruple(Interp,BBox,Left,Bot,Right,Top),
      tcl_eval(Interp,'fontWidget .stats.c create text 0 0 -font $indexfont -text "M"',MItem2),
      tcl_eval(Interp,format('.stats.c bbox ~s',[MItem2]),BBox2),
      quadruple(Interp,BBox2,Left2,Bot2,Right2,Top2),
      XOff is max(Right-Left,Right2-Left2),
      YOff is 2*min(Bot-Top,Bot2-Top2),
      Down is -(XOff+6),
      tcl_eval(Interp,'.stats.c delete all',_),
      lookup(X,File,SNum,0,M,1,N0,Rules0,Rules1,H0,H1,C0,C1,Subst,[],Es0,Es1,0,Num0,10,X0,Down,Y0,XOff,YOff,0,NV),
      macro_expand(G0,G),
      link0(G,Meaning0,S0,0,M,N0,N,Num0,Num,Rules1,[],H1,[],
           C1,[],Bs,[],Ps,[],Es1,Es),
      tcl_eval(Interp,'.stats.c delete lx',_), 
      copy_term(G,G1),
      numbervars(G1,NV,_),
      retractall('pn goal'(_)),
      assert('pn goal'(G1)),
      paint_formula0(G1,Interp,x,Num0,_Num,X0,X1,Down,Y1,XOff,YOff,_),
      Y is min(Y0,Y1),
      tcl_eval(Interp,format('.stats.c configure -width [expr ~w+30]',[X1]),_),
      tcl_eval(Interp,format('.stats.c configure -height [expr (-~w)+30+(~w * (~w/4))]',[Y,Num,XOff]),_),
      tcl_eval(Interp,format('.stats.c configure -scrollregion "0 [expr ~w-10-(~w*(~w/4))] [expr ~w+32] 20"',[Y,Num,XOff,X1]),_),
      tcl_eval(Interp,format('.stats.c lower [.stats.c create rectangle 0 [expr ~w-10-(~w*(~w/4))] [expr ~w+32] 20 -fill $grail_bg -outline $grail_bg]',[Y,Num,XOff,X1]),_),
      tcl_eval(Interp,'if {$interactive !="auto"} {\n\
                       wm geometry .stats {}\n\
                       }',_),
      tcl_eval(Interp,'incr lookups',Lookup),
      format1('~nLookup: ~s~n',[Lookup]),
      tcl_eval(Interp,'.stats.txt configure -text "Lookup $lookups"',_),
      tcl_eval(Interp,'.stats.ind delete all',_),
     ('solutions found'(NumS),
      NumS<1 ->
      retractall('solutions found'(_)),
      assert('solutions found'(0))
     ;
      true),
      wait_running1(Interp,_Runningstate).

% = redraw database after font selection

redraw_db :-
      my_tk_interpreter(Interp),
    ( tcl_eval(Interp,'winfo exists .stats.c',"1") ->
      tcl_eval(Interp,'fontWidget .stats.c create text 0 0 -font $pnfont -text "M"',MItem),
      tcl_eval(Interp,format('.stats.c bbox ~s',[MItem]),BBox),
      quadruple(Interp,BBox,Left,Bot,Right,Top),
      tcl_eval(Interp,'fontWidget .stats.c create text 0 0 -font $indexfont -text "M"',MItem2),
      tcl_eval(Interp,format('.stats.c bbox ~s',[MItem2]),BBox2),
      quadruple(Interp,BBox2,Left2,Bot2,Right2,Top2),
      XOff is max(Right-Left,Right2-Left2),
      YOff is 2*min(Bot-Top,Bot2-Top2),
      Down is -(XOff+6),
      tcl_eval(Interp,'.stats.c delete all',_),
      findall(Num-(Word-Form),'pn form'(Num,Word,Form),List0),
      keysort(List0,List),
      redraw_db(List,Interp,0,Num0,10,X0,Down,Y0,XOff,YOff),
      'pn goal'(G1),
      paint_formula0(G1,Interp,x,Num0,Num,X0,X1,Down,Y1,XOff,YOff,_),
      Y is min(Y0,Y1),
      tcl_eval(Interp,format('.stats.c configure -width [expr ~w+30]',[X1]),_),
      tcl_eval(Interp,format('.stats.c configure -height [expr (-~w)+30+(~w * (~w/4))]',[Y,Num,XOff]),_),
      tcl_eval(Interp,format('.stats.c configure -scrollregion "0 [expr ~w-10-(~w*(~w/4))] [expr ~w+32] 20"',[Y,Num,XOff,X1]),_),
      tcl_eval(Interp,format('.stats.c lower [.stats.c create rectangle 0 [expr ~w-10-(~w*(~w/4))] [expr ~w+32] 20 -fill $grail_bg -outline $grail_bg]',[Y,Num,XOff,X1]),_)
     ;
       true).

redraw_db([],_,Num,Num,X,X,Y,Y,_,_).

redraw_db([M0-(Word-Form)|Rest],Interp,Num0,Num,X0,X,Y0,Y,XOff,YOff) :-
     Down is -(XOff+6),
     paint_formula1(Form,Interp,M0,Num0,Num1,X0,X1,Down,Y1,XOff,YOff,t(MidX,_)),
     paint_word(Word,Interp,M0,MidX,0,LeftX,X2),
     Diff is X0-LeftX,
    (Diff >0 ->
     tcl_eval(Interp,format('.stats.c move l~w ~w 0',[M0,Diff]),_),
     Move = XOff+Diff
    ;
     Move = XOff),
     X3 is max(X1,X2),
     X4 is X3+Move,
     Y2 is min(Y0,Y1),
     redraw_db(Rest,Interp,Num1,Num,X4,X,Y2,Y,XOff,YOff). 

% ============================================================
% Formula decomposition. Has extra difference list pairs for
% information necessary to reconstruct natural deduction proof
% ============================================================

% = antecedent types

link1(lit(A),V,S,Pos0,Pos1,N,N,Id0,Id,Rules,Rules,H,H,
      Con,Con,[neg(A,V,S,Id0,Pos0,Pos1)|As],As,Ps,Ps,Es,Es) :-
      Id is Id0+1.

link1(dia(J,X),V,R,Pos0,Pos1,N0,N,Id0,Id,Rules0,Rules,
      [rule(hyp(_),unzip(J,R),X,'$VAR'(N0),[])|H0],H,
      [cf(unzip(J,R),dedia(V),'$VAR'(N0))|Con0],Con,
      As0,As,Ps0,Ps,Es0,Es) :-
    (continuous_dia(J) -> Pos2=Pos0,Pos3=Pos1 ; true),
     N1 is N0+1,
     link1(X,dedia(V),unzip(J,R),Pos2,Pos3,N1,N,Id0,Id,Rules0,Rules,
           H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es).
	
link1(box(J,X),V,R,Pos0,Pos1,N0,N,Id0,Id,[box(J,zip(J,R)-X)|Rules0],Rules,
      H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es) :-
    (continuous_dia(J) -> Pos2=Pos0,Pos3=Pos1 ; true),
     link1(X,debox(V),zip(J,R),Pos2,Pos3,N0,N,Id0,Id,Rules0,Rules,
           H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es).
	
link1(dr(J,X,Y),U,R,Pos0,Pos1,N0,N,Id0,Id,[dr(J,p(J,R,S)-X,S-Y)|Rules0],Rules,
      H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=Pos5 ; true),
     link1(X,appl(U,V),p(J,R,S),Pos2,Pos5,N0,N1,Id0,Id1,Rules0,Rules1,
           H0,H1,Con0,Con1,As0,As1,Ps0,Ps1,Es0,Es1),
     link0(Y,V,S,Pos3,Pos4,N1,N,Id1,Id,Rules1,Rules,
           H1,H,Con1,Con,As1,As,Ps1,Ps,Es1,Es). 

link1(dl(J,Y,X),U,R,Pos0,Pos1,N0,N,Id0,Id,[dl(J,S-Y,p(J,S,R)-X)|Rules0],Rules,
      H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=Pos5 ; true),
     link0(Y,V,S,Pos5,Pos2,N0,N1,Id0,Id1,Rules0,Rules1,
           H0,H1,Con0,Con1,As0,As1,Ps0,Ps1,Es0,Es1), 
     link1(X,appl(U,V),p(J,S,R),Pos4,Pos3,N1,N,Id1,Id,
           Rules1,Rules,H1,H,Con1,Con,As1,As,Ps1,Ps,Es1,Es).

link1(p(J,X,Y),V,R,Pos0,Pos1,N0,N,Id0,Id,Rules0,Rules,
      [rule(hyp(_),l(J,R),X,'$VAR'(N0),[]),
       rule(hyp(_),r(J,R),Y,'$VAR'(N1),[])|H0],H,
      [cf(l(J,R),fst(V),'$VAR'(N0)),cf(r(J,R),snd(V),'$VAR'(N1))|Con0],Con,
      As,As,[N0-N1|Ps],Ps,
      [vertex(N0,Bs,Qs),vertex(N1,Cs,Rs)|Es0],Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=c(N0),Pos5=c(N0) ; true),
     N1 is N0+1,
     N2 is N1+1,
     link1(X,fst(V),l(J,R),Pos2,Pos4,N2,N3,Id0,Id1,
           Rules0,Rules1,H0,H1,Con0,Con1,Bs,[],Qs,[],Es0,Es1),
     link1(Y,snd(V),r(J,R),Pos5,Pos3,N3,N,Id1,Id,
           Rules1,Rules,H1,H,Con1,Con,Cs,[],Rs,[],Es1,Es).
	
% = succedent types

link0(lit(A),V,S,Pos0,Pos1,N,N,Id0,Id,Rules,Rules,H,H,
      Con,Con,[pos(A,V,S,Id0,Pos0,Pos1)|As],As,Ps,Ps,Es,Es) :-
      Id is Id0+1.

link0(dia(J,X),condia(V),zip(J,R),Pos0,Pos1,N0,N,Id0,Id,
      [dia(J,R-X)|Rules0],Rules,H0,H,Con0,Con,
      As0,As,Ps0,Ps,Es0,Es) :-
    (continuous_dia(J) -> Pos2=Pos0,Pos3=Pos1 ; true),
     link0(X,V,R,Pos2,Pos3,N0,N,Id0,Id,Rules0,Rules,H0,H,
           Con0,Con,As0,As,Ps0,Ps,Es0,Es).
	
link0(box(J,X),conbox(V),unpack(J,R),Pos0,Pos1,N0,N,Id0,Id,Rules0,Rules,
      H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es) :-
    (continuous_dia(J) -> Pos2=Pos0,Pos3=Pos1 ; true),
     link0(X,V,R,Pos2,Pos3,N0,N,Id0,Id,Rules0,Rules,H0,H,
           Con0,Con,As0,As,Ps0,Ps,Es0,Es).
	
link0(dr(J,X,Y),lambda('$VAR'(N0),V),dr(J,R,x-'$VAR'(N0)),Pos0,Pos1,N0,N,Id0,Id,
      Rules0,Rules,[rule(hyp(_),x-'$VAR'(N0),Y,'$VAR'(N0),[])|H0],H,Con0,Con,
      As,As,[N0-N1|Ps],Ps,
      [vertex(N0,Bs,Qs),vertex(N1,Cs,Rs)|Es0],Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=c(N0),Pos5=c(N0) ; true),
     N1 is N0+1,  
     N2 is N1+1,
     link1(Y,'$VAR'(N0),x-'$VAR'(N0),Pos3,Pos4,N2,N3,Id0,Id1,
           Rules0,Rules1,H0,H1,Con0,Con1,Bs,[],Qs,[],Es0,Es1),
     link0(X,V,R,Pos2,Pos5,N3,N,Id1,Id,Rules1,Rules,
           H1,H,Con1,Con,Cs,[],Rs,[],Es1,Es).

link0(dl(J,Y,X),lambda('$VAR'(N0),V),dl(J,x-'$VAR'(N0),R),Pos0,Pos1,N0,N,Id0,Id,
      Rules0,Rules,[rule(hyp(_),x-'$VAR'(N0),Y,'$VAR'(N0),[])|H0],H,Con0,Con,
      As,As,[N0-N1|Ps],Ps,
      [vertex(N0,Bs,Qs),vertex(N1,Cs,Rs)|Es0],Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=c(N0),Pos5=c(N0) ; true),
     N1 is N0+1,
     N2 is N1+1,  
     link0(X,V,R,Pos5,Pos3,N2,N3,Id0,Id1,Rules0,Rules1,
           H0,H1,Con0,Con1,Bs,[],Qs,[],Es0,Es1), 
     link1(Y,'$VAR'(N0),x-'$VAR'(N0),Pos4,Pos2,N3,N,Id1,Id,
           Rules1,Rules,H1,H,Con1,Con,Cs,[],Rs,[],Es1,Es).
	
link0(p(J,X,Y),pair(U,V),p(J,R,S),Pos0,Pos1,N0,N,Id0,Id,
      [p(J,R-X,S-Y)|Rules0],Rules,H0,H,Con0,Con,
      As0,As,Ps0,Ps,Es0,Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=Pos5 ; true),
     link0(Y,V,S,Pos5,Pos3,N0,N1,Id0,Id1,Rules0,Rules1,
           H0,H1,Con0,Con1,As0,As1,Ps0,Ps1,Es0,Es1),
     link0(X,U,R,Pos2,Pos4,N1,N,Id1,Id,Rules1,Rules,
           H1,H,Con1,Con,As1,As,Ps1,Ps,Es1,Es).


% ============================================================
% Graph Reductions
% ============================================================

reduce_graph(G0,G) :-
        select(vertex(N,As,Ps0),G0,G1),
        select(M-M,Ps0,Ps),
        !,
        select(vertex(M,Bs,Qs),G1,G2),
        merge_vertices(vertex(N,As,Ps),vertex(M,Bs,Qs),G2,G3),
        reduce_graph(G3,G).
reduce_graph(G,G) :-
        connected(G).

replace_edges([],_,_,[]).
replace_edges([vertex(N,As,Ps)|Ls],X,Y,[vertex(N,As,Qs)|Ms]) :-
       replace_edges1(Ps,X,Y,Qs),
       replace_edges(Ls,X,Y,Ms).

replace_edges1([],_,_,[]).
replace_edges1([P1-P2|Ps],X,Y,[Q1-Q2|Qs]) :-
        replace_vertex(P1,X,Y,Q1),
        replace_vertex(P2,X,Y,Q2),
        replace_edges1(Ps,X,Y,Qs).

replace_vertex(V,X,Y,W) :-      
     ( V=X ->
       W=Y
     ;
       W=V).

connected([_]) :- !.

connected(L) :- 
       connected1(L).

connected1([vertex(_,As,Ps)|R]) :-
      (As = [] ->
       Ps = [_|_],connected1(R)
      ;
       connected1(R)
      ).
connected1([]).

% = cyclic(+Vertex1,+Vertex2,+Graph)
% true if linking Vertex1 and Vertex2 in Graph will produce
% a cycle for some switching of Graph.
% - If Vertex1 and Vertex2 don't have a common ancestor, linking
%   them won't produce a cycle.
% - If Vertex1 and Vertex2 have a common ancestor, linking them
%   will produce a cycle unless they are on different branches
%   of a par link.

cyclic(E1,E2,G0) :-
      select(A,[E1,E2|G0],G1),
      ancestor(A,E1,G1,_,Path1,[]),
      ancestor(A,E2,G1,_,Path2,[]),
      !,
      \+ branches(Path1,Path2).

branches([X|Xs],[Y|Ys]) :-
      X=l(P),Y=r(P) -> true
    ; X=r(P),Y=l(P) -> true
    ; X=Y ->
      branches(Xs,Ys).

ancestor(vertex(N,_,Ps),vertex(M,_,Rs),G0,G,L0,L) :-
      ( N=M,
        L=L0
      ;
        Ps=[P1-P2|Ps0],
       (  select(vertex(P1,_,Qs),G0,G1),
          L0=[l(P1-P2)|L1],
          ancestor(vertex(P1,_,Qs),vertex(M,_,Rs),G1,G,L1,L)
       ;
         select(vertex(P2,_,Qs),G0,G1),
         L0=[r(P1-P2)|L1],
         ancestor(vertex(P2,_,Qs),vertex(M,_,Rs),G1,G,L1,L)
       ;
         ancestor(vertex(N,_,Ps0),vertex(M,_,Rs),G0,G,L0,L)
       )
      ).

merge_vertices(vertex(N,As,Ps),vertex(M,Bs,Qs),G0,[vertex(N,Cs,Rs)|G]) :-
      append(Bs,As,Cs),
      append(Qs,Ps,Rs),
      replace_edges(G0,M,N,G).            

prove([],vertex(_,[],[]),_,_,_,_,_).

prove([G0|Gs0],G,[H0|Hs0],H,EstL0,Max,XOff) :-
       my_tk_interpreter(Interp),
       tcl_eval(Interp,'update',_),
       portray_estimate(Interp,EstL0,Max),
       tcl_eval(Interp,'set runningstate',Running),
      (  Running = "selected" ->
         /* user selected atomic formula */
         tcl_eval(Interp,'set atomid1',AtomS1),
         number_codes(Id0,AtomS1),
         select_atom_id(Id0,Atom0,Vertex0,Atom1,Vertex1,
                        [G,G0|Gs0],Gs1,[H,H0|Hs0],Hs1)
      ;
         /* select ground formula */
         select_atom_id1(Id0,Atom0,Vertex0,Atom1,Vertex1,
                         [G,G0|Gs0],Gs1,[H,H0|Hs0],Hs1)
      ),
      atom_formula(Atom0,Form),
      tcl_eval(Interp,format('detailedtext $runningstate "Linking ~w"',[Form]),_),
      find_conj_ids(Atom0,Gs1,Ids),
      tcl_eval(Interp,format('focus .stats.c\n\
        set tags [.stats.c gettags v~w]\n\
        set list_ind [lsearch -regexp $tags {^(n)[0-9]+(n)[0-9]+$}]\n\
        set nums [lindex $tags $list_ind]\n\
        set axs [split $nums n]\n\
        .stats.c addtag node withtag n[lindex $axs 1]\n\
        .stats.c addtag node withtag n[lindex $axs 2]\n\
        .stats.c delete v~w',[Id0,Id0]),_),
      get_estimate(Atom0,f(EA,EM,_EN0),EstL0,EstL),
      select_conj_ids(Ids,Id0,Atom0,Vertex2,Atom1,Vertex3,
                      Gs1,Gs2,Hs1,Hs2,EA,EM,EN,EstL,Max,XOff),
        (  cyclic(Vertex0,Vertex2,Gs2) ->
           tcl_eval(Interp,'detailedtext $runningstate "Cyclic!"',_),
           wait_running(Interp,_),
           fail
        ;
         true
        ),       
       merge_vertices(Vertex0,Vertex2,Gs2,Gs3),
       merge_vertices(Vertex1,Vertex3,Hs2,Hs3),
        (  reduce_graph(Gs3,[G4|Gs4]) ->
           reduce_graph(Hs3,[H4|Hs4]),
           tcl_eval(Interp,'incr acclinks',_),
           tcl_eval(Interp,format('detailedtext $runningstate "Linked ~w"',[Form]),_),
           wait_running(Interp,_),
           tcl_eval(Interp,'set eagerl',Eager),
          (  Eager = "none" ->
             G5=G4,Gs5=Gs4
          ;
             remove_all_divisions([G4|Gs4],[G5|Gs5])
          ),
           tcl_eval(Interp,'incr labellinks',_),
           prove(Gs5,G5,Hs4,H4,[f(EA,EM,EN)|EstL],Max,XOff)
        ;
           tcl_eval(Interp,'detailedtext $runningstate "Disconnected!"',_),
           wait_running(Interp,_),
           fail
        ).

select_atom_id(Id,Atom0,vertex(N,As,Ps),Atom1,vertex(N,Bs,Qs),G0,G,H0,H) :-
       duo_select(vertex(N,As0,Ps),vertex(N,Bs0,Qs),G0,G,H0,H),
       duo_select(Atom0,Atom1,As0,As,Bs0,Bs),
       atom_id(Atom0,Id),
       !.

select_atom_id1(Id,Atom0,vertex(N,As,Ps),Atom1,vertex(N,Bs,Qs),G0,G,H0,H) :-
       duo_select(vertex(N,As0,Ps),vertex(N,Bs0,Qs),G0,G,H0,H),
       duo_select(Atom0,Atom1,As0,As,Bs0,Bs), 
       atom_label(Atom0,Label),
       is_ground(Label),
       atom_id(Atom0,Id),
       !.

select_conj_ids([],Id0,_,_,_,_,_,_,_,_,_,_,_,_,_,_) :-
       my_tk_interpreter(Interp),
         tcl_eval(Interp,format('focus .stats.c\n\
      set tags [.stats.c gettags v~w]\n\
      set list_ind [lsearch -regexp $tags {^(n)[0-9]+(n)[0-9]+$}]\n\
      set nums [lindex $tags $list_ind]\n\
      set axs [split $nums n]\n\
      .stats.c addtag node withtag n[lindex $axs 1]\n\
      .stats.c addtag node withtag n[lindex $axs 2]\n\
      .stats.c delete v~w',[Id0,Id0]),_),
       fail.

select_conj_ids([Id1|Ids0],Id0,Atom0,vertex(N,As,Ps),Atom1,vertex(N,Bs,Ps),G0,G,H0,H,EA,EM,EN,EstL,Max,XOff) :-
       my_tk_interpreter(Interp),
       atom_formula(Atom0,Form),
       tcl_eval(Interp,format('detailedtext $runningstate "Linking ~p"',[Form]),_),
         tcl_eval(Interp,format('focus .stats.c\n\
      set tags [.stats.c gettags v~w]\n\
      set list_ind [lsearch -regexp $tags {^(n)[0-9]+(n)[0-9]+$}]\n\
      set nums [lindex $tags $list_ind]\n\
      set axs [split $nums n]\n\
      .stats.c addtag node withtag n[lindex $axs 1]\n\
      .stats.c addtag node withtag n[lindex $axs 2]\n\
      .stats.c delete v~w',[Id0,Id0]),_),
       tcl_eval(Interp,format('.stats.c delete selectbox\n\
                        set tags [.stats.c gettags [.stats.c find withtag "n~w"]]\n\
                        set list_ind [lsearch -regexp $tags {^(n)[0-9]+$}]\n\
                        set path [lindex $tags $list_ind]\n\
                        set box [.stats.c bbox $path]\n\
                        set boxl [expr [lindex $box 0] -1]\n\
                        set boxr [expr [lindex $box 2] +1]\n\
                        set boxu [expr [lindex $box 1] -1]\n\
                        set boxd [expr [lindex $box 3] +1]\n\
                        set s_tags {}\n\
                        lappend s_tags selectbox $path\n\
                        .stats.c create line $boxl $boxu $boxr $boxu -tags $s_tags\n\
                        .stats.c create line $boxl $boxd $boxr $boxd -tags $s_tags\n\
                        .stats.c create line $boxl $boxd $boxl $boxu -tags $s_tags\n\
                        .stats.c create line $boxr $boxu $boxr $boxd -tags $s_tags',[Id0]),_),
       /* draw white box around possible conjugates */
       box_ids([Id1|Ids0],Interp,white),
       wait_running(Interp,Running2),
     ( Running2 = "linked" ->
        tcl_eval(Interp,'set runningstate "creep"',_),
        tcl_eval(Interp,'activate .stats',_),
        tcl_eval(Interp,'set atomid1',AtomS3),
        tcl_eval(Interp,'set atomid2',AtomS2),
        number_codes(Id3,AtomS3),
        number_codes(Id2,AtomS2),
        (Id2=Id0 -> Id=Id3 ; Id=Id2),
	 Ids1 = [Id1|Ids0]
     ;
       Running2 = "commit" ->
        tcl_eval(Interp,'set runningstate "creep"',_),
        tcl_eval(Interp,'activate .stats',_),
        tcl_eval(Interp,'set atomid1',AtomS3),
        tcl_eval(Interp,'set atomid2',AtomS2),
        number_codes(Id3,AtomS3),
        number_codes(Id2,AtomS2),
        (Id2=Id0 -> Id=Id3 ; Id=Id2),
	 Ids1 = [Id]
     ;
        /* user did not select conjugate; try first one */
        Id=Id1,
	Ids1 = [Id1|Ids0]
     ),
       length(Ids1,EN0),
       portray_estimate(Interp,[f(EA,EM,EN0)|EstL],Max),
     ( select(Id,Ids1,Ids) ->
        ( Id0 < Id -> IdS = Id0, IdE = Id
        ; IdS = Id, IdE = Id0),
         my_tk_interpreter(Interp),
         tcl_eval(Interp,'incr totallinks',_),
         tcl_eval(Interp,format('focus .stats.c\n\
      .stats.c delete selectbox\n\
      set tags [.stats.c gettags v~w]\n\
      set list_ind [lsearch -regexp $tags {^(n)[0-9]+(n)[0-9]+$}]\n\
      set nums [lindex $tags $list_ind]\n\
      set axs [split $nums n]\n\
      .stats.c addtag node withtag n[lindex $axs 1]\n\
      .stats.c addtag node withtag n[lindex $axs 2]\n\
      .stats.c delete v~w',[Id0,Id0]),_),
         tcl_eval(Interp,format('focus .stats.c\n\
     set box1 [.stats.c bbox n~w]\n\
     set box2 [.stats.c bbox n~w]',[Id0,Id]),_),
         tcl_eval(Interp,format('set skip [expr ~w/2]\n\
     set box1x [expr ([lindex $box1 0] + [lindex $box1 2])/2]\n\
     set box2x [expr ([lindex $box2 0] + [lindex $box2 2])/2]\n\
     set box1y [expr [lindex $box1 1]+2]\n\
     set box2y [expr [lindex $box2 1]+2]\n\
     set maxy [expr $box1y]\n\
     for {set x ~w} {$x <= ~w} {incr x} {\n\
     if {$height($x) < $maxy} {\n\
     set maxy $height($x)\n\
     }}\n\
     set maxy [expr $maxy-$skip]\n\
     set continue "yes"\n\
     if {$box1x < $box2x} {\n\
         set boxl $box1x\n\
         set boxr $box2x\n\
     } else {\n\
        set boxl $box2x\n\
        set boxr $box1x\n\
     }\n\
     while {$continue == "yes"} {\n\
     set continue "no"\n\
     foreach i [.stats.c find withtag top] {\n\
     set boxi [.stats.c bbox $i]\n\
     set boxil [expr [lindex $boxi 0]]\n\
     set boxir [expr [lindex $boxi 2]]\n\
     set boxit [expr [lindex $boxi 1]+2]\n\
     if {($boxir > $boxl) && ($boxr > $boxil)  && abs($boxit - $maxy) < $skip} {\n\
        set maxy [expr $maxy-$skip]\n\
        set continue "yes"\n\
     }\n\
     }}\n\
     ',[XOff,IdS,IdE]),_),     
         tcl_eval(Interp,format('.stats.c dtag n~w node\n\
    .stats.c dtag n~w node',[Id0,Id]),_),
         tcl_eval(Interp,format('.stats.c create line $box1x $box1y $box1x $maxy -tags "ax n~wn~w v~w"',[Id0,Id,Id0]),_),
         tcl_eval(Interp,format('.stats.c create line $box2x $box2y $box2x $maxy -tags "ax n~wn~w v~w"',[Id0,Id,Id0]),_),
         tcl_eval(Interp,format('.stats.c create line $box1x $maxy $box2x $maxy -tags "top ax n~wn~w v~w"',[Id0,Id,Id0]),_),
      (duo_select(vertex(N,[A|As0],Ps),vertex(N,[B|Bs0],Ps),G0,G,H0,H),
       duo_select(Conj0,Conj1,[A|As0],As,[B|Bs0],Bs),
       atom_id(Conj0,Id),
       unify_atoms(Atom0,Conj0),
       unify_atoms(Atom1,Conj1),
       EN = EN0,
         tcl_eval(Interp,'incr planarlinks',_)
      ;
        /* on backtracking try alternative ids */
        select_conj_ids(Ids,Id0,Atom0,vertex(N,As,Ps),Atom1,vertex(N,Bs,Ps),G0,G,H0,H,EA,EM,EN,EstL,Max,XOff)
      )
      ;
       /* user made invalid selection, retry */
       select_conj_ids([Id1|Ids0],Id0,Atom0,vertex(N,As,Ps),Atom1,vertex(N,Bs,Ps),G0,G,H0,H,EA,EM,EN,EstL,Max,XOff)
      ).

find_conj_ids(Atom0,Gs,Ids) :-
       findall(Id,(select(vertex(_,As0,_),Gs,_),
                   select(Atom,As0,_),
                   conjugates_id(Atom0,Atom,Id)),Ids).
                   
box_ids([],_,_).
box_ids([N|Ns],Interp,Color) :-
       tcl_eval(Interp,format('set tags [.stats.c gettags [.stats.c find withtag "n~w"]]\n\
                        set list_ind [lsearch -regexp $tags {^(n)[0-9]+$}]\n\
                        set path [lindex $tags $list_ind]\n\
                        set box [.stats.c bbox $path]\n\
                        set boxl [expr [lindex $box 0] -1]\n\
                        set boxr [expr [lindex $box 2] +1]\n\
                        set boxu [expr [lindex $box 1] -1]\n\
                        set boxd [expr [lindex $box 3] +1]\n\
                        set s_tags {}\n\
                        lappend s_tags selectbox $path\n\
                        .stats.c create line $boxl $boxu $boxr $boxu -fill ~w -tags $s_tags\n\
                        .stats.c create line $boxl $boxd $boxr $boxd -fill ~w -tags $s_tags\n\
                        .stats.c create line $boxl $boxd $boxl $boxu -fill ~w -tags $s_tags\n\
                        .stats.c create line $boxr $boxu $boxr $boxd -fill ~w -tags $s_tags',[N,Color,Color,Color,Color]),_),
      box_ids(Ns,Interp,Color).

unify_atoms(neg(B,V,S,_,P0,P1),pos(B,V,S,_,P2,P3)) :-
     (P0=P2,P1=P3 ->
      true
     ;
      my_tk_interpreter(Interp),
      tcl_eval(Interp,'detailedtext $runningstate "Failed on Continuity"',_),
      wait_running(Interp,_),
      fail).
unify_atoms(pos(B,V,S,_,P0,P1),neg(B,V,S,_,P2,P3)) :-
     (P0=P2,P1=P3 ->
      true
     ;
      my_tk_interpreter(Interp),
      tcl_eval(Interp,'detailedtext $runningstate "Failed on Continuity"',_),
      wait_running(Interp,_),
      fail).

atom_id(neg(_,_,_,Id,_,_),Id).
atom_id(pos(_,_,_,Id,_,_),Id).

atom_formula(neg(Form,_,_,_,_,_),Form).
atom_formula(pos(Form,_,_,_,_,_),Form).

atom_label(neg(_,_,Label,_,_,_),Label).
atom_label(pos(_,_,Label,_,_,_),Label).

conjugates_id(neg(Form,_,_,_,_,_),pos(Form,_,_,Id,_,_),Id).
conjugates_id(pos(Form,_,_,_,_,_),neg(Form,_,_,Id,_,_),Id).

% ============================================================
% Label Reductions
% ============================================================
% remove_all_divisions(+ProofNet,-NewNet)
% true if NewNet is ProofNet with remove_divisions applied
% to all ground labels.

remove_all_divisions([],[]).
remove_all_divisions([vertex(N,As0,Ps)|Rest0],[vertex(N,As,Ps)|Rest])
:-
     remove_all_divisions1(As0,As),
     remove_all_divisions(Rest0,Rest).

remove_all_divisions1([],[]).
remove_all_divisions1([A0|As0],[A|As]) :-
     remove_all_divisions2(A0,A),
     remove_all_divisions1(As0,As).

remove_all_divisions2(neg(A,B,Label0,C,D,E),neg(A,B,Label,C,D,E)) :-
    ( is_ground(Label0) ->
      ( check_lp(Label0) ->
        Label = Label0
      ;
        my_tk_interpreter(Interp),
        tcl_eval(Interp,'.stats.txt configure -text "Checking Label Constraints"',_),
        ( remove_divisions(Label0,Label) ->
          tcl_eval(Interp,'.stats.txt configure -text "Label Constraints Satisfied"',_),
          wait_rewrite(Interp,_),
          tcl_eval(Interp,'.stats.txt configure -text "Linking"',_)
        ;
          /* if eager evaluation fails, show user failing label */
          tcl_eval(Interp,'detailedtext $runningstate "Failed on Label Constraints"',_),
          wait_running(Interp,_),
          tcl_eval(Interp,'.stats.txt configure -text "Linking"',_),
          fail
        )
      )
    ;
      Label=Label0
    ).

remove_all_divisions2(pos(A,B,Label0,C,D,E),pos(A,B,Label,C,D,E)) :-
    ( is_ground(Label0) ->
      ( check_lp(Label0) ->
        Label = Label0
      ;
        trace,
        my_tk_interpreter(Interp),
        tcl_eval(Interp,'.stats.txt configure -text "Checking Label Constraints"',_),
        ( remove_divisions(Label0,Label) ->
          tcl_eval(Interp,'.stats.txt configure -text "Label Constraints Satisfied"',_),
          wait_rewrite(Interp,_),
          tcl_eval(Interp,'.stats.txt configure -text "Linking"',_)
        ;
          /* if eager evaluation fails, show user failing label */
          tcl_eval(Interp,'detailedtext $runningstate "Failed on Label Constraints"',_),
          wait_running(Interp,_),
          tcl_eval(Interp,'.stats.txt configure -text "Linking"',_),
          fail
        )
      )
    ;
      Label=Label0
    ).

% remove_divisions(+Label,-DivFreeLabel)
% remove all dr, dl occurences from label 

% first select_divisions/4 is called, which returns a (possibly
% non-ground) DivFreeLabel and a difference-list containing
% Division-Hole pairs 

remove_divisions(S0,S) :-
     create_rewrite_window,
     my_tk_interpreter(Interp),
     tcl_eval(Interp,'if {$rewritestate != "nonstop"} {\n\
                        if {$eagerl != "auto"} {\n\
                        set rewritestate $defaultrewrite\n\
                        } else {\n\
                        set rewritestate "continue"\n\
                        }\n\
                      }',_),
     tcl_eval(Interp,'update\n\
                      set rewritestate',Rewrite),
    ((Rewrite = "nonstop";Rewrite = "continue") ->
     display_rd_label(S0,0),
     wait_rewrite(Interp,_),
     remove_redexes(S0,Interp,S),
     display_rd_label(S,0),
     wait_rewrite(Interp,_)
    ;
     tcl_eval(Interp,'incr labelgen',_),
     breadth_star([],1,[1-S0|B],B,node(S0,0,empty,empty),Interp,[],S)
    ).

remove_divisions(_,_) :-
     my_tk_interpreter(Interp),
     tcl_eval(Interp,'.stats.txt configure -text "Linking"\n\
                      if {[winfo exists .rewrite]} {\n\
                      .rewrite.c dtag node\n\
                      .rewrite.exit configure -state disabled\n\
                      }',_),
     fail.

% search is breadth first with closed set, and succeeds only once
%
% solution(Label) = check_lp(Label)
% child(Parent,Child) = rewrite(Parent,Child)

% = breadth_star(+NewChildren,+QLength,+QFront,+QBack,+Closed,?Answer)

breadth_star([],N0,[Depth0-Node|F],B,Closed,Interp,Undo,Answer) :-
     N0 > 0,
     N is N0-1,
     Depth is Depth0+1,
       tcl_eval(Interp,'incr labelcount',_),
       update_undo(Undo,Interp),
       display_rd_label(Node,1),
       tcl_eval(Interp,format('if {[winfo exists .rewrite]} {\n\
                               .rewrite.text configure -text "Depth: ~w   Queue: ~w"}',[Depth0,N]),_),
       tcl_eval(Interp,'update idletasks',_),
    (wait_rewrite(Interp,Rewrite) ->
      ( Rewrite \== "stepped",
        Rewrite \== "up",
	Rewrite \== "undo",
        check_lp(Node)
      ->
        Answer = Node
      ; 
        ( Rewrite = "undo" ->
	    /* undo previous rewrite step */
	   ( Undo = [] ->
	       breadth_star([],N0,[Depth0-Node|F],B,Closed,Interp,Undo,Answer)
	   ;
	       Undo=[U|Undo0],
	       breadth_star([],N0,[U|F],B,Closed,Interp,Undo0,Answer)
	   )
	;
          Rewrite = "up" ->
            /* remove all items at current depth or below from queue */
            remove_from_queue(N,F,B,Depth0,N1,F1,B1),
            breadth_star([],N1,F1,B1,Closed,Interp,[],Answer)
        ;
          Rewrite = "stepped" ->
           /* user selected rewrite step */
          'current label'(Label),
          children(Node,Children),
          union(Children,Closed,Closed1,Children1,[]),
          ( select(Label,Children1,Children2) ->
            true
          ;
            Children2=Children1
          ),
          add_depth(Children2,Depth,Children3),
          add_to_front([Depth-Label|Children3],Interp,N,F,N1,F1),
          breadth_star([],N1,F1,B,Closed1,Interp,[Depth0-Node|Undo],Answer)
       ;
         children(Node,Children),
         union(Children,Closed,Closed1,Children1,[]),
         add_depth(Children1,Depth,Children2),
         breadth_star(Children2,N,F,B,Closed1,Interp,[],Answer)
       )
      )
    ;
     breadth_star([],N,F,B,Closed,Interp,[],Answer)
    ).  

breadth_star([X|Xs],N0,F,[X|B],Closed,Interp,Undo,Answer) :-
     N is N0+1,
     tcl_eval(Interp,'incr labelgen',_),
     breadth_star(Xs,N,F,B,Closed,Interp,Undo,Answer).

% = children/2 will generate a set of children from a given parent.
 
children(ParentNode,ChildrenNodes) :-
     findall(ChildNode,rewrite(ParentNode,ChildNode),ChildrenNodes).
     
% = normalize(+Label,-NormalLabel)
% perform label reductions and rewrite according to
% structural postulates until a normal label results.

% search is breadth first with closed set, and succeeds only once.
% solution(Label) = normal(Label)
% child(Parent,Child) = rewrite(Parent,Child)

% normalize(+Label,-NormalLabel)
% normalize Label with user interaction

normalize(Start0,Answer) :-
     remove_divisions(Start0,Start),
     create_rewrite_window,
     my_tk_interpreter(Interp),
     tcl_eval(Interp,'if {$rewritestate != "nonstop"} {\n\
                      set rewritestate $defaultrewrite\n\
                      }',_),
     display_label(Start,0),
     tcl_eval(Interp,'.stats.txt configure -text "Rewriting"',_),
     tcl_eval(Interp,'incr labelgen',_),
     breadth_star1([],1,[1-Start|B],B,node(Start,0,empty,empty),Interp,[],Answer),
     display_label(Answer,0),
     tcl_eval(Interp,'if {[winfo exists .rewrite]} {\n\
                      .rewrite.c dtag node\n\
                      }',_).

normalize(_,_) :-
     my_tk_interpreter(Interp),
     tcl_eval(Interp,'.stats.txt configure -text "Linking"\n\
                      if {[winfo exists .rewrite]} {\n\
                      .rewrite.c dtag node\n\
                      .rewrite.exit configure -state disabled\n\
                      }',_),
     fail.

breadth_star1([],N0,[Depth0-Node|F],B,Closed,Interp,Undo,Answer) :-
     N0 > 0,
     N is N0-1,
     Depth is Depth0+1,
       tcl_eval(Interp,'incr labelcount',_),
       display_label(Node,1),
       update_undo(Undo,Interp),
       tcl_eval(Interp,format('if {[winfo exists .rewrite]} {\n\
                               .rewrite.text configure -text "Depth: ~w   Queue: ~w"}',[Depth0,N]),_),
       tcl_eval(Interp,'update idletasks',_),
    (wait_rewrite(Interp,Rewrite) ->
      ( Rewrite \== "stepped",
        Rewrite \== "up",
	Rewrite \== "undo",
        normal(Node)
       ->
        Answer = Node
      ; 
        ( Rewrite = "undo" ->
	    /* undo previous rewrite step */
	   ( Undo = [] ->
	       breadth_star1([],N0,[Depth0-Node|F],B,Closed,Interp,Undo,Answer)
	   ;
	       Undo=[U|Undo0],
	       breadth_star1([],N0,[U|F],B,Closed,Interp,Undo0,Answer)
	   )
	;
	  Rewrite = "up" ->
            /* remove all items at current depth or below from queue */
            remove_from_queue(N,F,B,Depth0,N1,F1,B1),
            breadth_star1([],N1,F1,B1,Closed,Interp,[],Answer)
        ;
          Rewrite = "stepped" ->
           /* user selected rewrite step */
          'current label'(Label),
          children(Node,Children),
          union(Children,Closed,Closed1,Children1,[]),
          ( select(Label,Children1,Children2) ->
            true
          ;
            Children2=Children1
          ),
          add_depth(Children2,Depth,Children3),
          add_to_front([Depth-Label|Children3],Interp,N,F,N1,F1),
          breadth_star1([],N1,F1,B,Closed1,Interp,[Depth0-Node|Undo],Answer)
       ;
         children(Node,Children),
         union(Children,Closed,Closed1,Children1,[]),
         add_depth(Children1,Depth,Children2),
         breadth_star1(Children2,N,F,B,Closed1,Interp,[],Answer)
       )
      )
    ;
     breadth_star1([],N,F,B,Closed,Interp,[],Answer)
    ).  

breadth_star1([X|Xs],N0,F,[X|B],Closed,Interp,Undo,Answer) :-
     N is N0+1,
     tcl_eval(Interp,'incr labelgen',_),
     breadth_star1(Xs,N,F,B,Closed,Interp,Undo,Answer).

% add_to_front(+List,+Length,+Front,-Length,-Front)
% add items in List to the front of the input queue.

add_to_front([],_,N,F,N,F).
add_to_front([X|Xs],Interp,N0,F0,N,[X|F]) :-
      N1 is N0+1,
      tcl_eval(Interp,'incr labelgen',_),
      add_to_front(Xs,Interp,N1,F0,N,F).

% remove_from_queue(+L,+F,+B,+Depth,-L,-F,-B)
% remove items of depth above or equal to Depth
% from the queue.

remove_from_queue(0,B,B,_,0,B,B).
remove_from_queue(N0,[D0-X|F0],B0,D,N,F,B) :-
      N0 > 0,
      N1 is N0-1,
     (D0>=D ->
      remove_from_queue(N1,F0,B0,D,N,F,B)
     ;
      F=[D0-X|F1],
      remove_from_queue(N1,F0,B0,D,N2,F1,B),
      N is N2+1).

add_depth([],_,[]).
add_depth([L|Ls0],D,[D-L|Ls]) :-
      add_depth(Ls0,D,Ls).

% rewrite(+Label0,?Label) 
% true if Label0 is obtainable from Label in a single residuation 
% reduction or postulate rewrite 

rewrite(D,D1) :-
     reduce(D,D1).
rewrite(D,D1) :-
     postulate(D,D1,_Name).
rewrite(unpack(J,D),unpack(J,D1)) :-
     rewrite(D,D1).
rewrite(zip(J,D),zip(J,D1)) :-
     rewrite(D,D1).
rewrite(p(J,D,G),p(J,D1,G)) :-
     rewrite(D,D1).
rewrite(p(J,G,D),p(J,G,D1)) :-
     rewrite(D,D1).
rewrite(dr(J,D,G),dr(J,D1,G)) :-
     rewrite(D,D1).
rewrite(dl(J,G,D),dl(J,G,D1)) :-
     rewrite(D,D1).

% = Residuation reductions

reduce(p(J,l(J,D),r(J,D)),D). % product
reduce(dr(J,p(J,G,D),D),G).   % division right
reduce(dl(J,D,p(J,D,G)),G).   % division left
reduce(zip(J,unzip(J,D)),D).  % diamond
reduce(unpack(J,zip(J,D)),D). % box

% = Linear precedence

check_lp(Node) :-
     check_lp(Node,x,_).

% check_lp(+Node,+GreaterThan,-Max)

check_lp(N-_,X,Y) :-
      N = x -> Y = X
    ;
      precedes(X,N), 
      Y = N.

check_lp(p(I,A,B),N0,N) :-
      transparent(I)   -> check_lp(A,N0,N1), check_lp(B,N1,N)
    ; check_lp(A,x,_),check_lp(B,x,_).
 
check_lp(zip(I,A),N0,N) :-
      transparent_dia(I) -> check_lp(A,N0,N)
    ; check_lp(A,x,_).

check_lp(unzip(_,A),N0,N) :-
     check_lp(A,N0,N).

check_lp(unpack(_,A),N0,N) :-
     check_lp(A,N0,N).

check_lp(dl(_,x-B,A0),_,N) :-
     min_pos(A0,Min),
     replace_pos(A0,Min,B,A),
     check_lp(A,Min,N).

check_lp(dr(_,A0,x-B),N0,Max0) :-
     max_pos(A0,Max0),
     Max is Max0+1,
     replace_pos(A0,Max,B,A),
     check_lp(A,N0,Max).

check_lp(l(_,A),N0,N) :-
     check_lp(A,N0,N).

check_lp(r(_,A),N0,N) :-
     check_lp(A,N0,N).

max_pos(A,Max) :-
    max_pos(A,0,Max).

max_pos(X-_,N0,N) :-
     X = x -> N = N0
   ;
     precedes(N0,X) -> N = X
   ;
     N = N0.

max_pos(p(_,A,B),N0,N) :-
     max_pos(A,N0,N1),
     max_pos(B,N1,N).

max_pos(dl(_,_,A),N0,N) :-
     max_pos(A,N0,N).

max_pos(dr(_,A,_),N0,N) :-
     max_pos(A,N0,N).

max_pos(l(_,A),N0,N) :-
     max_pos(A,N0,N).

max_pos(r(_,A),N0,N) :-
     max_pos(A,N0,N).

max_pos(zip(_,A),N0,N) :-
     max_pos(A,N0,N).

max_pos(unzip(_,A),N0,N) :-
     max_pos(A,N0,N).

max_pos(unpack(_,A),N0,N) :-
     max_pos(A,N0,N).

min_pos(A,Min) :-
     min_pos(A,x,Min0),
    (Min0=x -> Min=x
    ;
     Min is Min0-1
    ).

min_pos(X-_,N0,N) :-
     X = x -> N = N0
   ;
     precedes(X,N0) -> N = X
   ;
     N = N0.

min_pos(p(_,A,B),N0,N) :-
     min_pos(A,N0,N1),
     min_pos(B,N1,N).

min_pos(dl(_,_,A),N0,N) :-
     min_pos(A,N0,N).

min_pos(dr(_,A,_),N0,N) :-
     min_pos(A,N0,N).

min_pos(l(_,A),N0,N) :-
     min_pos(A,N0,N).

min_pos(r(_,A),N0,N) :-
     min_pos(A,N0,N).

min_pos(zip(_,A),N0,N) :-
     min_pos(A,N0,N).

min_pos(unzip(_,A),N0,N) :-
     min_pos(A,N0,N).

min_pos(unpack(_,A),N0,N) :-
     min_pos(A,N0,N).

replace_pos(M0-A,N,C,M-A) :-
    (A=C -> M=N
    ;
     M=M0
    ).
replace_pos(p(I,A0,B0),N,C,p(I,A,B)) :-
     replace_pos(A0,N,C,A),
     replace_pos(B0,N,C,B).
replace_pos(dl(I,B,A0),N,C,dl(I,B,A)) :-
     replace_pos(A0,N,C,A).
replace_pos(dr(I,A0,B),N,C,dr(I,A,B)) :-
     replace_pos(A0,N,C,A).
replace_pos(l(I,A0),N,C,l(I,A)) :-
     replace_pos(A0,N,C,A).
replace_pos(r(I,A0),N,C,r(I,A)) :-
     replace_pos(A0,N,C,A).
replace_pos(zip(I,A0),N,C,zip(I,A)) :-
     replace_pos(A0,N,C,A).
replace_pos(unzip(I,A0),N,C,unzip(I,A)) :-
     replace_pos(A0,N,C,A).
replace_pos(unpack(I,A0),N,C,unpack(I,A)) :-
     replace_pos(A0,N,C,A).

update_undo([],Interp) :-
	tcl_eval(Interp,'if {[winfo exists .rewrite]} {.rewrite.mbar.link.menu entryconfigure 0 -state disabled}',_).

update_undo([_|_],Interp) :-
	tcl_eval(Interp,'if {[winfo exists .rewrite]} {.rewrite.mbar.link.menu entryconfigure 0 -state normal}',_).

	
% normalize(+Label,+NormalLabel,-PathHead,?PathTail)
% as normalize/2 but now with dl pair representing path to solution

normalize(S0,S,Path0,Path) :-
       my_tk_interpreter(Interp),
       breadth_first_search1(S0,S,Interp,Path0,Path).

% 280999 These predicates are no longer called.

% remove_divisions(+Label,-DivFreeLabel)
% remove all dr, dl occurences from label 
 
% first select_divisions/4 is called, which returns a (possibly
% non-ground) DivFreeLabel and a difference-list containing
% Division-Hole pairs 

remove_divisions(S0,S,Interp,Path0,Path) :-
        select_divisions(S0,S1,L,[]),
        remove_divisions1(L,S1,S,Interp,Path0,Path).

% remove_divisions(+ListOfDHPairs,?LabelWithHole,-GroundLabel)
% successively remove the divisions from the list of pairs and put
% them back in the corresponding holes

remove_divisions1([],S0,S,Interp,Path0,Path) :-
        breadth_first_search(S0,S,Interp,Path0,Path).
remove_divisions1([D-H|Rest0],S0,S,Interp,Path0,Path) :-
        breadth_first_search(D,H,Interp,Path0,Path1),
	map_replace_label1(Rest0,D,H,Rest),
        remove_divisions1(Rest,S0,S,Interp,Path1,Path).

% breadth first search which returns dl path to solution
% because we know there is a solution we represent the Open
% set as a dl instead of a queue

breadth_first_search1(Start,Answer,Interp,Path0,Path) :-
        tcl_eval(Interp,'incr labelgen',_),
        breadth_star1([],1,[Start/[]|Rest],Rest,[Start],[],RevPath,Interp,Answer),
        reverse_dl(RevPath,Path0,Path).

breadth_first_search(Start,Answer,Interp,Path0,Path) :-
        tcl_eval(Interp,'incr labelgen',_),
        breadth_star([],1,[Start/[]|Rest],Rest,[Start],[],RevPath,Interp,Answer),
        reverse_dl(RevPath,Path0,Path).

breadth_star([],N0,[Node/NodePath|Front],Back,Closed,_NodePath0,RevPath,Interp,Answer)
    :-
       N0 > 0,
       N is N0-1,
       tcl_eval(Interp,'incr labelcount',_),
       tcl_eval(Interp,'update',_),
       tcl_eval(Interp,'if {$runningstate == "cancel"} {\n\
                        prolog raise_exception(''Cancel'')\n\
                        }',_),
       ( check_lp1(Node),
         Answer = Node,
         RevPath = NodePath
       ; 
         labelled_children(Node,Children),
         lb_ord_union(Closed,Children,Closed1,Children1),
         breadth_star(Children1,N,Front,Back,Closed1,NodePath,RevPath,Interp,Answer)
       ).

breadth_star([Child-Label|Children],N0,F,[(Child/[Label|Path])|B],Closed,
              Path,RevPath,Interp,Answer) :-
       N is N0+1,
       tcl_eval(Interp,'incr labelgen',_),
       breadth_star(Children,N,F,B,Closed,Path,RevPath,Interp,Answer).

breadth_star1([],N0,[Node/NodePath|Front],Back,Closed,_NodePath0,RevPath,Interp,Answer)
    :-
       N0 > 0,
       N is N0-1,
       tcl_eval(Interp,'incr labelcount',_),
       tcl_eval(Interp,'update',_),
       tcl_eval(Interp,'if {$runningstate == "cancel"} {\n\
                        prolog raise_exception(''Cancel'')\n\
                        }',_),
       ( normal(Node),
         Answer = Node, 
         RevPath = NodePath
       ; 
         labelled_children(Node,Children),
         lb_ord_union(Closed,Children,Closed1,Children1),
         breadth_star(Children1,N,Front,Back,Closed1,NodePath,RevPath,Interp,Answer)
       ).

breadth_star1([Child-Label|Children],N0,F,[(Child/[Label|Path])|B],Closed,
              Path,RevPath,Interp,Answer) :-
       N is N0+1,
       tcl_eval(Interp,'incr labelgen',_),
       breadth_star1(Children,N,F,B,Closed,Path,RevPath,Interp,Answer).

% =

check_lp1(_-_).
check_lp1(zip(_,A)) :-
    check_lp1(A).
check_lp1(p(_,A,B)) :-
    check_lp1(A),
    check_lp1(B).
check_lp1(l(_,A)) :-
    check_lp1(A).
check_lp1(r(_,A)) :-
    check_lp1(A).
check_lp1(unzip(_,A)) :-
    check_lp1(A).

% = children/2 will generate a set of children from a given parent.
 
labelled_children(ParentNode,ChildrenSet) :-
     findall(ChildNode-Label,rewrite1(ParentNode,ChildNode,Label),ChildrenNodes),
     keysort(ChildrenNodes,ChildrenSet).

% =
     
add_children([],_,L,L).

add_children([Child-Label|Children],Path,[(Child/[Label|Path])|L0],L) :-
      add_children(Children,Path,L0,L).

% =

% rewrite(+Label0,?Label,?Name) 
% true if Label0 is obtainable from Label in a single residuation 
% reduction or postulate rewrite by applying the rule indicated
% by Name 

rewrite(unpack(J,D0),unpack(J,D),L) :-
     rewrite(D0,D,L).
rewrite(zip(J,D0),zip(J,D),L) :-
     rewrite(D0,D,L).
rewrite(p(J,D0,G),p(J,D,G),L) :-
     rewrite(D0,D,L).
rewrite(p(J,G,D0),p(J,G,D),L) :-
     rewrite(D0,D,L).
rewrite(dr(J,D0,G),dr(J,D,G),L) :-
     rewrite(D0,D,L).
rewrite(dl(J,G,D0),dl(J,G,D),L) :-
     rewrite(D0,D,L).
rewrite(D0,D,D0) :-
     reduce(D0,D).
rewrite(D0,D,sr(Name,D0,D)) :-
     postulate(D0,D,Name).

% rewrite(+Label0,?Label,?Name) 
% true if Label0 is obtainable from Label in a single residuation 
% reduction or postulate rewrite by applying the rule indicated
% by Name 

rewrite1(D0,D,L) :-
     rewrite_lr(D0,D,L).
rewrite1(unpack(J,D0),unpack(J,D),L) :-
     rewrite1(D0,D,L).
rewrite1(unzip(J,D0),unzip(J,D),L) :-
     rewrite1(D0,D,L).
rewrite1(zip(J,D0),zip(J,D),L) :-
     rewrite1(D0,D,L).
rewrite1(p(J,D0,G),p(J,D,G),L) :-
     rewrite1(D0,D,L).
rewrite1(p(J,G,D0),p(J,G,D),L) :-
     rewrite1(D0,D,L).
rewrite1(dr(J,D0,G),dr(J,D,G),L) :-
     rewrite1(D0,D,L).
rewrite1(dl(J,G,D0),dl(J,G,D),L) :-
     rewrite1(D0,D,L).
rewrite1(D0,D,D0) :-
     reduce(D0,D).
rewrite1(D0,D,sr(Name,D0,D)) :-
     postulate(D0,D,Name).

% make sure that when l(.) and r(.) co-occur any conversion inside
% one is also performed inside the other.

rewrite_lr(Label0,Label,Name) :-
	select_lr(Label0,l(L0)),
	select_lr(Label0,r(L0)),
	rewrite1(L0,L,Name),
	replace_label(Label0,L0,L,Label).

select_lr(unpack(_,D),X) :-
     select_lr(D,X).
select_lr(unzip(_,D),X) :-
     select_lr(D,X).
select_lr(zip(_,D),X) :-
     select_lr(D,X).
select_lr(p(_,D,_),X) :-
     select_lr(D,X).
select_lr(p(_,_,D),X) :-
     select_lr(D,X).
select_lr(dr(_,D,_),X) :-
     select_lr(D,X).
select_lr(dl(_,_,D),X) :-
     select_lr(D,X).
select_lr(l(_,D),X) :-
     select_lr(D,X).
select_lr(r(_,D),X) :-
     select_lr(D,X).
select_lr(l(J,D),l(J,D)).
select_lr(r(J,D),r(J,D)).
		
% = select_divisions(+Label,-LabelWithHole,-DList)
% when called with a Label select divisions will return a copy of that
% label with a hole on the place of the divisions in it, and a
% difference list containing Division-Hole pairs. The pairs are
% ordered in such a way that a Division is ground when all Holes
% before it one the list are filled

select_divisions(N-W,N-W) -->
	[].
select_divisions(l(I,A0),l(I,A)) -->
	select_divisions(A0,A).
select_divisions(r(I,A),r(I,A)) -->
	[].
select_divisions(unzip(I,A0),unzip(I,A)) -->
	select_divisions(A0,A).
select_divisions(zip(I,A0),zip(I,A)) -->
	select_divisions(A0,A).
select_divisions(unpack(I,A0),H) -->
	select_divisions(A0,A),
	[unpack(I,A)-H].
select_divisions(dr(I,A0,V),H) -->
	select_divisions(A0,A),
	[dr(I,A,V)-H].
select_divisions(dl(I,V,A0),H) -->
	select_divisions(A0,A),
	[dl(I,V,A)-H].
select_divisions(p(I,A0,B0),p(I,A,B)) -->
	select_divisions(A0,A),
	select_divisions(B0,B).

% ============================================================
% Lexicon
% ============================================================

unknown_words([]).
unknown_words([W|Ws]) :-
     lex(W,_,_) -> unknown_words(Ws)
   ; my_tk_interpreter(I),
     tcl_eval(I,format('dialog .d {Lexicon Error} {Lexical lookup failed: "~w"} error 0 {Cancel}',[W]),_),
     fail.

% = lexical lookup
% extra difference list pair for label-type pairs and 2-rules

lookup([],_,_,M,M,N,N,R,R,H,H,C,C,S,S,L,L,Num,Num,X,X,Y,Y,_,_,NV,NV).
lookup([W|Ws],File,SNum,M0,M,N0,N,R0,R,[rule(lex,M0-W,Syn,'$VAR'(N0),[])|H0],H,
       C0,C,[N0-Sem|S0],S,[vertex(N0,As,Ps)|L0],L,Num0,Num,X0,X,Y0,Y,XOff,YOff,NV0,NV) :-
     lex(W,Syn0,Sem,File,SNum,M0),
     macro_expand(Syn0,Syn),
     my_tk_interpreter(Interp),
     tcl_eval(Interp,format('.stats.c delete l~w',[M0]),_),
     copy_term(Syn,Syn1),
     numbervars(Syn1,NV0,NV1),
     retractall('pn form'(M0,_,_)),
     assert('pn form'(M0,W,Syn1)),
     Down is -(XOff+6),
     paint_formula1(Syn1,Interp,M0,Num0,Num1,X0,X1,Down,Y1,XOff,YOff,t(MidX,_)),
     paint_word(W,Interp,M0,MidX,0,LeftX,X2),
     Diff is X0-LeftX,
    (Diff >0 ->
     tcl_eval(Interp,format('.stats.c move l~w ~w 0',[M0,Diff]),_),
     Move = XOff+Diff
    ;
     Move = XOff
    ),
     X3 is max(X1,X2),
     X4 is X3+Move,
     Y2 is min(Y0,Y1),
     M1 is M0+1,
     N1 is N0+1,
     link1(Syn,'$VAR'(N0),M0-W,M0,M1,N1,N2,Num0,Num1,R0,R1,H0,H1,C0,C1,As,[],Ps,[],L0,L1),
     lookup(Ws,File,SNum,M1,M,N2,N,R1,R,H1,H,C1,C,S0,S,L1,L,Num1,Num,X4,X,Y2,Y,XOff,YOff,NV1,NV).

% expand macro definitions

macro_expand(S0,S) :-
     apply_macro(S0,S1),
     !,
     macro_expand(S1,S).

macro_expand(S,S).

% reduce macro definitions

macro_reduce(S0,S) :-
     apply_macro(S1,S0),
     !,
     macro_reduce(S1,S).

macro_reduce(S,S).

apply_macro(dia(I,S0),dia(I,S)) :-
     apply_macro(S0,S).
apply_macro(box(I,S0),box(I,S)) :-
     apply_macro(S0,S).
apply_macro(p(I,R0,S),p(I,R,S)) :-
     apply_macro(R0,R).
apply_macro(p(I,R,S0),p(I,R,S)) :-
     apply_macro(S0,S).
apply_macro(dl(I,R0,S),dl(I,R,S)) :-
     apply_macro(R0,R).
apply_macro(dl(I,R,S0),dl(I,R,S)) :-
     apply_macro(S0,S).
apply_macro(dr(I,R0,S),dr(I,R,S)) :-
     apply_macro(R0,R).
apply_macro(dr(I,R,S0),dr(I,R,S)) :-
     apply_macro(S0,S).
apply_macro(S0,S) :-
     macro(S0,S).
apply_macro(S,lit(S)) :-
     literal(S).

% ============================================================
% Auxiliaries
% ============================================================

strip_keys([],[]).
strip_keys([_-X|Xs],[X|Ys]) :-
    strip_keys(Xs,Ys).

reverse_dl([],Y,Y).

reverse_dl([X|Xs],Ys,Zs) :-
    reverse_dl(Xs,Ys,[X|Zs]).

% =

duo_select(X,Y,[X|Xs],Xs,[Y|Ys],Ys).
duo_select(V,W,[X|Xs],[X|Vs],[Y|Ys],[Y|Ws]) :-
       duo_select(V,W,Xs,Vs,Ys,Ws).

% =

member_check(X,[X|_]) :- !.
member_check(X,[_|Ys]) :-
     member_check(X,Ys).

%   lb_ord_union/4 is ord_union/4 (form Quintus library(ordsets) 
%   where NewSet and ReallyNew are Item-Label pairs and OldSet and 
%   Union lists of Items

%   lb_ord_union(+OldSet, +NewSet, ?Union, ?ReallyNew)
%   is true when Union is NewSet U OldSet and ReallyNew is NewSet \ OldSet.
%   This is useful when you have an iterative problem, and you're adding
%   some possibly new elements (NewSet) to a set (OldSet), and as well as
%   getting the updated set (Union) you would like to know which if any of
%   the "new" elements didn't already occur in the set (ReallyNew).

lb_ord_union([], Set2, Set2, Set2).
lb_ord_union([Head1|Tail1], Set2, Union, New) :-
	lb_ord_union_1(Set2, Head1, Tail1, Union, New).

lb_ord_union_1([], Head1, Tail1, [Head1|Tail1], []).
lb_ord_union_1([Head2-Label|Tail2], Head1, Tail1, Union, New) :-
	compare(Order, Head1, Head2),
	lb_ord_union(Order, Head1, Tail1, Label, Head2, Tail2, Union, New).

lb_ord_union(<, Head1, Tail1, Label, Head2, Tail2, [Head1|Union], New) :-
	lb_ord_union_2(Tail1, Label, Head2, Tail2, Union, New).
lb_ord_union(>, Head1, Tail1, Label, Head2, Tail2, [Head2|Union], [Head2-Label|New]) :-
	lb_ord_union_1(Tail2, Head1, Tail1, Union, New).
lb_ord_union(=, Head1, Tail1, _, _,    Tail2, [Head1|Union], New) :-
	lb_ord_union(Tail1, Tail2, Union, New).

lb_ord_union_2([], Label, Head2, Tail2, [Head2|Tail2], [Head2-Label|Tail2]).
lb_ord_union_2([Head1|Tail1], Label, Head2, Tail2, Union, New) :-
	compare(Order, Head1, Head2),
	lb_ord_union(Order, Head1, Tail1, Label, Head2, Tail2, Union, New).

% = count_check(+Graph)
% true if each atomic formula has an equal number of positive
% and negative occurrences.

count_check(G) :-
     count_check(G,[],L),
     count_check1(L,G).

count_check1([],_).
count_check1([A-N|Rest],G) :-
    (N \== 0 ->
      format1('Unbalanced ~w:~w~n===~n',[A,N]),
      my_tk_interpreter(Interp),
      findall(Id1,(select(vertex(_,As0,_),G,_),
                 select(pos(Form,_,_,Id1,_,_),As0,_),
                 functor(Form,A,_)),Ids1),
      box_ids(Ids1,Interp,white),
      findall(Id2,(select(vertex(_,As0,_),G,_),
                 select(neg(Form,_,_,Id2,_,_),As0,_),
                 functor(Form,A,_)),Ids2),
      box_ids(Ids2,Interp,black),
      tcl_eval(Interp,format(
               '.stats.txt configure -text "Unbalanced ~p''s"',[A]),_),
      wait_running(Interp,_),
      tcl_eval(Interp,'.stats.c delete selectbox',_),
      fail
    ;
     count_check1(Rest)
    ).

count_check([],L,L).
count_check([vertex(_,As,_)|Vs],L0,L) :-
     count_check_atom(As,L0,L1),
     count_check(Vs,L1,L).

count_check_atom([],L,L).
count_check_atom([A|As],L0,L) :-
     count_increment(A,L0,L1),
     count_check_atom(As,L1,L).


count_increment(neg(A,_,_,_,_,_),L0,L) :-
     /* allows use of compounds as atomic formulas */ 
     functor(A,F,_),
     count_increment(L0,F,-1,L).
count_increment(pos(A,_,_,_,_,_),L0,L) :-
     functor(A,F,_),
     count_increment(L0,F, 1,L).

count_increment([B-N0|As0],A,Incr,As) :-
    (A=B ->
     N is N0+Incr,
    (N=0 ->
     As = As0
    ;
     As=[A-N|As0])
    ;
     As=[B-N0|As1],
     count_increment(As0,A,Incr,As1)
    ).

count_increment([],A,Incr,[A-Incr]).

% ============================================================
% Auxiliaries
% ============================================================

init(X,Goal) :-
       garbage_collect,
       my_tk_interpreter(Interp),
       tcl_eval(Interp,'set solutionsfound 0',_),
       tcl_eval(Interp,'set labelcount 0',_),
       tcl_eval(Interp,'set labelgen 0',_),
       tcl_eval(Interp,'set totallinks 0',_),
       tcl_eval(Interp,'set planarlinks 0',_),
       tcl_eval(Interp,'set acclinks 0',_),
       tcl_eval(Interp,'set labellinks 0',_),
       tcl_eval(Interp,'set lookups 0',_),
       
       retractall('solutions found'(_)),
       retractall('start time'(_)),
       retractall('garbage collection'(_)),
       retractall(rd_table(_,_)),
       assert('solutions found'(-1)),
       statistics(runtime,[T0|_]),
       assert('start time'(T0)),
       statistics(garbage_collection,GC0),
       assert('garbage collection'(GC0)),
       format1('~n===~n~p => ~p~n===~n',[X,Goal]).

found_solution(Meaning,S0,S,N) :-
       retract('solutions found'(N0)),
       N is N0+1,
       assert('solutions found'(N)),
       my_tk_interpreter(Interp),
       tcl_eval(Interp,'incr solutionsfound',_),
       tcl_eval(Interp,'update idletasks',_),
       format1('~n~w. ',[N]),
       pp_sem1(Meaning),
       format1('~n~n',[]),
       pp_labels(S0,S),
       format1('~n===~n',[]),
       tcl_eval(Interp,'.stats.c itemconfigure current_ax -fill black\n\
                        .stats.c dtag current_ax',_),
       tcl_eval(Interp,'.stats.txt configure -text "Found Solution $solutionsfound"',_),
       wait_running1(Interp,_).

no_solution(S0,Meaning) :-
       format1('~n(FAILED). ',[]),
       pp_sem1(Meaning),
       format1('~n~n~p~p~n~p~n~n~p~n',[S0,' -->> ','IRREDUCIBLE','===']),
       fail.

write_solutions(N) :-
       retract('solutions found'(N)),
     ( N= -1 -> format('~nLexicon error!~n',[])
     ; N=  0 -> format('~nNo solutions!~n',[])
     ; N=  1 -> format('~n1 solution found.~n',[])
     ; format('~n~w solutions found.~n',[N])
     ).

write_statistics :-
     statistics(runtime,[T|_]),
     my_tk_interpreter(Interp),
    (var(Interp) ->
     true
    ;
     tcl_eval(Interp,'set lookups',Lookups),
     tcl_eval(Interp,'set labelcount',Labels),
     tcl_eval(Interp,'set labelgen',LGen),
     tcl_eval(Interp,'set totallinks',TotalL),
     tcl_eval(Interp,'set planarlinks',PlanarL),
     tcl_eval(Interp,'set acclinks',ACCL),
     tcl_eval(Interp,'set labellinks',LabelL),
     max_list_length([Lookups,LGen,TotalL],0,Max0),
     Max is Max0+18,
     format1('~n~61t~*|~n',[Max]),
     format1('=~tStatistics~t=~*|~n',[Max]),
     format1('~61t~*|~n',[Max]),
     format1('Total Lookups   : ~t~s~*|~n',[Lookups,Max]),
     format1('~61t~*|~n',[Max]),
     format1('Labels Generated: ~t~s~*|~n',[LGen,Max]),
     format1('Labels Visited  : ~t~s~*|~n',[Labels,Max]),
     format1('~61t~*|~n',[Max]),
     format1('Total Links     : ~t~s~*|~n',[TotalL,Max]),
     format1('Planar Links    : ~t~s~*|~n',[PlanarL,Max]),
     format1('ACC Links       : ~t~s~*|~n',[ACCL,Max]),
     format1('Label Links     : ~t~s~*|~n',[LabelL,Max]),
     format1('~61t~*|~n',[Max])
    ),
     retract('start time'(T0)),
     Time is T-T0,
     format('CPU Time used: ~3D~n',[Time]),
     statistics(garbage_collection,[NGC1,Bytes1,GCTime1]),
     retract('garbage collection'([NGC0,Bytes0,GCTime0])),
     NGC is NGC1-NGC0,
    (NGC = 0 ->
      true
    ;
      KBytes is (Bytes1-Bytes0) // 1024,
      GCTime is (GCTime1-GCTime0)*0.001,
      format('Garbage coll.: ~D (~D Kb, ~p sec)~n',[NGC,KBytes,GCTime])
    ).

max_list_length([],M,M).
max_list_length([L|Ls],M0,M) :-
      length(L,M1),
      M2 is max(M0,M1),
      max_list_length(Ls,M2,M).

% = normal(+Label)
% a label is normal if all occurences of unzip, unpack, 
% dl, dr, l and r have been reduced, the words are in the
% right order, and it contains no grammar-internal modes.

% normal labels can contain logical constants of the form x-X

normal(Label) :-
     normal(Label,-1,_).

normal(M-_,N0,N) :-
    (M=x ->                    
     N=N0 
    ;
     M is N0+1,
     N=M
    ).

normal(p(I,A,B),N0,N) :-
     external(I),
     normal(A,N0,N1),
     normal(B,N1,N).

normal(zip(I,A),N0,N) :-
     external_dia(I),
     normal(A,N0,N).
     
% = select_divisions(+Label,-LabelWithHole,-DList)
% when called with a Label select divisions will return a copy of that
% label with a hole on the place of the divisions in it, and a
% difference list containing Division-Hole pairs. The pairs are
% ordered in such a way that a Division is ground when all Holes
% before it one the list are filled

find_redexes(Label0,Label,List) :-
       select_redexes(Label0,Label,List,[]),
       ( List=[Label0-_] ->
         true
       ;
         format1('~nRedexes:~n~p~n===~n',[Label0]),
         portray_list(List)
       ).

remove_redexes(Label0,Interp,Label) :-
     ( check_lp(Label0) ->
       Label=Label0
     ;
       find_redexes(Label0,Label1,List),
       remove_redexes1(List,Interp),
       remove_redexes(Label1,Interp,Label)
     ).

remove_redexes1([],_).
remove_redexes1([S0-S|Rest],Interp) :-
     ( rd_table(S0,L) ->
       format1('~n~p~p~n~p~n~p~n',[S0,' <tabled> ',L,'===']),
       L=[S]
     ;
       ( breadth_star([],1,[1-S0|B],B,node(S0,0,empty,empty),Interp,[],S) ->
         format1('~n~p~p~n~p~n~p~n',[S0,' -->> ',S,'===']),
         assert(rd_table(S0,[S]))
       ;
         format1('~n~p~p~n~p~n~p~n',[S0,' -->> ','IRREDUCIBLE','===']),
         assert(rd_table(S0,[])),
         fail
       )
     ),
     remove_redexes1(Rest,Interp).

select_redexes(N-W,N-W,L,L).
select_redexes(zip(I,A0),H,L0,L) :-
     ( check_lp(A0) ->
       L0=[zip(I,A0)-H|L]
     ;
       H=zip(I,A),
       select_redexes(A0,A,L0,L)
     ).
select_redexes(unzip(I,A0),H,L0,L) :-
     ( check_lp(A0) ->
       L0=[unzip(I,A0)-H|L]
     ;
       H=unzip(I,A),
       select_redexes(A0,A,L0,L)
     ).
select_redexes(unpack(I,A0),H,L0,L) :-
     ( check_lp(A0) ->
       L0=[unpack(I,A0)-H|L]
     ;
       H=unpack(I,A),
       select_redexes(A0,A,L0,L)
     ).
select_redexes(l(I,A0),H,L0,L) :-
     ( check_lp(A0) ->
       L0=[l(I,A0)-H|L]
     ;
       H=l(I,A),
       select_redexes(A0,A,L0,L)
     ).
select_redexes(r(I,A0),H,L0,L) :-
     ( check_lp(A0) ->
       L0=[r(I,A0)-H|L]
     ;
       H=r(I,A),
       select_redexes(A0,A,L0,L)
     ).
select_redexes(p(I,A0,B0),H,L0,L) :-
     ( check_lp(A0) ->
       ( check_lp(B0) ->
         L0=[p(I,A0,B0)-H|L]
       ;
         H=p(I,A0,B),
         select_redexes(B0,B,L0,L)
       )
     ; 
       ( check_lp(B0) ->
         H=p(I,A,B0),
         select_redexes(A0,A,L0,L)
       ;
         H=p(I,A,B),
         select_redexes(A0,A,L0,L1),
         select_redexes(B0,B,L1,L)
       )
     ).
select_redexes(dr(I,A0,V),H,L0,L) :-
     ( check_lp(A0) ->
       L0=[dr(I,A0,V)-H|L]
     ;
       H=dr(I,A,V),
       select_redexes(A0,A,L0,L)
     ).
select_redexes(dl(I,V,A0),H,L0,L) :-
     ( check_lp(A0) ->
       L0=[dl(I,V,A0)-H|L]
     ;
       H=dl(I,V,A),
       select_redexes(A0,A,L0,L)
     ).

label_size(_-_,N0,N) :-
     N is N0+1.
label_size(zip(_,A),N0,N) :-
     N1 is N0+1,
     label_size(A,N1,N).
label_size(unzip(_,A),N0,N) :-
     N1 is N0+1,
     label_size(A,N1,N).
label_size(unpack(_,A),N0,N) :-
     N1 is N0+1,
     label_size(A,N1,N).
label_size(p(_,A,B),N0,N) :-
     N1 is N0+1,
     label_size(A,N1,N2),
     label_size(B,N2,N).
label_size(dl(_,_,A),N0,N) :-
     N1 is N0+1,
     label_size(A,N1,N).
label_size(dr(_,A,_),N0,N) :-
     N1 is N0+1,
     label_size(A,N1,N).
label_size(l(_,A),N0,N) :-
     N1 is N0+1,
     label_size(A,N1,N).
label_size(r(_,A),N0,N) :-
     N1 is N0+1,
     label_size(A,N1,N).

% = literal(+Syn) 
% true if Syn abbreviates lit(Syn), i.e. is a basic syntactic category

literal(X) :- 
     atom(X),
     !.
literal(X) :- 
     user:atomic_formula(X).

% = time(+Call)
% print time used to find the first solution (if any) to Call

time(Call) :-
     statistics(runtime,[T0|_]),
     call1(Call),
     statistics(runtime,[T|_]),
     Time is (T-T0)*0.001,
     write('CPU Time used: '),write(Time),nl.

% = call1(Goal) 
% call goal once, succeed always

call1(Call) :- call(Call),!.
call1(_).

catch_fail(Goal,Message,Indicator) :-
     \+ Goal ->
     write(Message),write(Indicator),nl,fail
    ;
     Goal.

% precedes(+LPNumber0,+LPNumber)
% true if LPNumber0 precedes LPNumber. LPNumbers can be either a
% number or the constant 'x' which stands for an unknown position

precedes(x,_) :- !.
precedes(X,Y) :- X @=< Y. % also includes precedes(Num,x) 

select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]) :-
       select(X,Ys,Zs).

%append([],Xs,Xs).
%append([X|Xs],Ys,[X|Zs]) :-
%       append(Xs,Ys,Zs).

member1(X,[Y|Ys]) :-
       member1(Ys,Y,X).

member1([],X,X).
member1([_|_],X,X).
member1([Y|Ys],_,X) :-
       member1(Ys,Y,X).

union([],AVL,AVL,RN,RN).
union([X|Xs],AVL0,AVL,RN0,RN) :-
       insert(AVL0,X,AVL1,RN0,RN1,_),
       union(Xs,AVL1,AVL,RN1,RN).

insert(empty,         Key, node(Key,0,empty,empty), [Key|RN], RN, 1).
insert(node(K,B,L,R), Key, Result, RN0, RN, Delta) :-
       (Key = K ->
        insert(=, Key, Result, Delta, K, B, L, R, RN0, RN)
       ;
        compare(O, Key, K),
	insert(O, Key, Result, Delta, K, B, L, R, RN0, RN)).


insert(=, Key, node(Key,B,L,R), 0, _, B, L, R, RN, RN).
insert(<, Key, Result,          Delta, K, B, L, R, RN0, RN) :-
	insert(L, Key, Lavl, RN0, RN, Ldel),
	Delta is \(B) /\ Ldel,	% this grew iff left grew and was balanced
	B1 is B-Ldel,
	(   B1 =:= -2 ->	% rotation needed
	    Lavl = node(Y,OY,A,CD),	    
	    (   OY =< 0 ->
		NY is OY+1, NK is -NY,
		Result = node(Y,NY,A,node(K,NK,CD,R))
	    ;/* OY = 1, double rotation needed */
		CD = node(X,OX,C,D),
		NY is 0-((1+OX) >> 1),
		NK is (1-OX) >> 1,
		Result = node(X,0,node(Y,NY,A,C),node(K,NK,D,R))
	    )
	;   Result = node(K,B1,Lavl,R)
	).
insert(>, Key, Result,          Delta, K, B, L, R, RN0, RN) :-
	insert(R, Key, Ravl, RN0, RN, Rdel),
	Delta is \(B) /\ Rdel,	% this grew iff right grew and was balanced
	B1 is B+Rdel,
	(   B1 =:= 2 ->		% rotation needed
	    Ravl = node(Y,OY,AC,D),
	    (   OY >= 0 ->
		NY is OY-1, NK is -NY,
		Result = node(Y,NY,node(K,NK,L,AC),D)
	    ;/* OY = -1, double rotation needed */
		AC = node(X,OX,A,C),
		NY is (1-OX) >> 1,
		NK is 0-((1+OX) >> 1),
		Result = node(X,0,node(K,NK,L,A),node(Y,NY,C,D))
	    )
	;   Result = node(K,B1,L,Ravl)
	).


is_ground(' * ') :-
      !,
      fail.
is_ground(_-_).
is_ground(zip(_,A)) :-
      is_ground(A).
is_ground(unzip(_,A)) :-
      is_ground(A).
is_ground(unpack(_,A)) :-
      is_ground(A).
is_ground(p(_,A,B)) :-
      is_ground(A),
      is_ground(B).
is_ground(l(_,A)) :-
      is_ground(A).
is_ground(r(_,A)) :-
      is_ground(A).
is_ground(dl(_,_,A)) :-
      is_ground(A).
is_ground(dr(_,A,_)) :-
      is_ground(A).
