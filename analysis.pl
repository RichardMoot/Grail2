% ==================================================================
% analysis.pl
%
% Automatic (but very slow) detection of grammar properties for use 
% with Grail
% ==================================================================

% analysis
%
% analyze postulate component of grammar for possible non-convergence
% non-transparence and discontinuity, and assert appropriate clauses 
% for correct behavior 

% ====================================================================
% create_analysis_window
% open a window with optimization parameters
% ====================================================================

create_analysis_window :-
       my_tk_interpreter(I),
       tcl_eval(I,'source analysis.tcl',_),
       findall(U,'unary mode'(U) ,Us),
       findall(B,'binary mode'(B),Bs),
       create_checkbuttons(I,Us,Bs).

update_analysis_window :-
       my_tk_interpreter(I),
       tcl_eval(I,'destroy .an.ft.l
                   destroy .an.ft.r
                   destroy .an.fb.l
                   destroy .an.fb.r

                   frame .an.ft.l -bd 2 -relief sunken
                   frame .an.ft.r -bd 2 -relief sunken
                   frame .an.fb.l -bd 2 -relief sunken
                   frame .an.fb.r -bd 2 -relief sunken

                   pack .an.ft.l .an.ft.r -padx 2 -side left -fill both -expand 1
                   pack .an.fb.l .an.fb.r -padx 2 -side left -fill both -expand 1
                   ',_),
       findall(U,'unary mode'(U) ,Us),
       findall(B,'binary mode'(B),Bs),
       create_checkbuttons(I,Us,Bs).

create_checkbuttons(I,Us,Bs) :-
       create_external_buttons(I,Us,Bs),
       create_lazy_buttons(I,Us,Bs),
       create_transparency_buttons(I,Us,Bs),
       create_cont_buttons(I,Us,Bs).

create_external_buttons(I,Us,Bs) :-
       tcl_eval(I,'label .an.ft.l.txt -text "External Modes"',_),
       tcl_eval(I,'label .an.ft.l.u -text "  Unary"',_),
       tcl_eval(I,'pack .an.ft.l.txt .an.ft.l.u -side top -anchor w',_),
       create_checkbuttonlist(Us,I,'.an.ft.l.ub',external_dia,0,_),
       tcl_eval(I,'label .an.ft.l.b -text "  Binary"',_),
       tcl_eval(I,'pack .an.ft.l.txt .an.ft.l.b -side top -anchor w',_),
       create_checkbuttonlist(Bs,I,'.an.ft.l.bb',external,0,_).

create_lazy_buttons(I,Us,Bs) :-
       tcl_eval(I,'label .an.ft.r.txt -text "Lazy Evaluation"',_),
       tcl_eval(I,'label .an.ft.r.u -text "  Unary"',_),
       tcl_eval(I,'pack .an.ft.r.txt .an.ft.r.u -side top -anchor w',_),
       create_checkbuttonlist(Us,I,'.an.ft.r.ub',lazy_unpack,0,_),
       tcl_eval(I,'label .an.ft.r.b -text "  Binary"',_),
       tcl_eval(I,'pack .an.ft.r.txt .an.ft.r.b -side top -anchor w',_),
       create_checkbuttonlist1(Bs,I,'.an.ft.r.bb',0,_).

create_transparency_buttons(I,Us,Bs) :-
       tcl_eval(I,'label .an.fb.l.txt -text "Transparency   "',_),
       tcl_eval(I,'label .an.fb.l.u -text "  Unary"',_),
       tcl_eval(I,'pack .an.fb.l.txt .an.fb.l.u -side top -anchor w',_),
       create_checkbuttonlist(Us,I,'.an.fb.l.ub',transparent_dia,0,_),
       tcl_eval(I,'label .an.fb.l.b -text "  Binary"',_),
       tcl_eval(I,'pack .an.fb.l.txt .an.fb.l.b -side top -anchor w',_),
       create_checkbuttonlist(Bs,I,'.an.fb.l.bb',transparent,0,_).

create_cont_buttons(I,Us,Bs) :-
       tcl_eval(I,'label .an.fb.r.txt -text "Continuity      "',_),
       tcl_eval(I,'label .an.fb.r.u -text "  Unary"',_),
       tcl_eval(I,'pack .an.fb.r.txt .an.fb.r.u -side top -anchor w',_),
       create_checkbuttonlist(Us,I,'.an.fb.r.ub',continuous_dia,0,_),
       tcl_eval(I,'label .an.fb.r.b -text "  Binary"',_),
       tcl_eval(I,'pack .an.fb.r.txt .an.fb.r.b -side top -anchor w',_),
       create_checkbuttonlist(Bs,I,'.an.fb.r.bb',continuous,0,_).

create_checkbuttonlist([],_,_,_,N,N).
create_checkbuttonlist([I|Is],Interp,W,F,N0,N) :-
       tcl_eval(Interp,format('checkbutton ~w~d -command {prolog {change_~w(~k)}} -variable ~w(~d) -text "~w"',[W,N0,F,I,F,N0,I]),_),
       functor(Term,F,1),
       arg(1,Term,I),
     ( Term -> tcl_eval(Interp,format('~w~d select',[W,N0]),_),Var=1
     ;         tcl_eval(Interp,format('~w~d deselect',[W,N0]),_),Var=0
     ),
       tcl_eval(Interp,format('set ~w(~d) ~d',[F,N0,Var]),_),
       tcl_eval(Interp,format('pack ~w~d -side top -anchor w',[W,N0]),_),
       N1 is N0+1,
       create_checkbuttonlist(Is,Interp,W,F,N1,N).

create_checkbuttonlist1([],_,_,N,N).
create_checkbuttonlist1([I|Is],Interp,W,N0,N) :-
       tcl_eval(Interp,format('checkbutton ~w~d -command {prolog {change_lazy(~k)}} -variable lazy(~d) -text "~w"',[W,N0,I,N0,I]),_),
     ( lazy_dl(I) -> tcl_eval(Interp,format('~w~d select',[W,N0]),_),Var=1
     ; lazy_dr(I) -> tcl_eval(Interp,format('~w~d select',[W,N0]),_),Var=1
     ;               tcl_eval(Interp,format('~w~d deselect',[W,N0]),_),Var=0
     ),
       tcl_eval(Interp,format('set lazy(~d) ~d',[N0,Var]),_),
       tcl_eval(Interp,format('pack ~w~d -side top -anchor w',[W,N0]),_),
       N1 is N0+1,
       create_checkbuttonlist1(Is,Interp,W,N1,N).

change_external(I) :-
       set_manual_opt_state,
       external(I) -> retractall(external(I))
     ; assert(external(I)).

change_external_dia(I) :-
       set_manual_opt_state,
       external_dia(I) -> retractall(external_dia(I))
     ; assert(external_dia(I)).

change_continuous(I) :-
       set_manual_opt_state,
       continuous(I) -> retractall(continuous(I))
     ; assert(continuous(I)).

change_continuous_dia(I) :-
       set_manual_opt_state,
       continuous_dia(I) -> retractall(continuous_dia(I))
     ; assert(continuous_dia(I)).

change_transparent(I) :-
       set_manual_opt_state,
       transparent(I) -> retractall(transparent(I))
     ; assert(transparent(I)).

change_transparent_dia(I) :-
       set_manual_opt_state,
       transparent_dia(I) -> retractall(transparent_dia(I))
     ; assert(transparent_dia(I)).

change_lazy_unpack(I) :-
       set_manual_opt_state,
       lazy_unpack(I) -> retractall(lazy_unpack(I))
     ; assert(lazy_unpack(I)).

change_lazy(I) :-
       set_manual_opt_state,
       lazy_dl(I) -> retractall(lazy_dl(I)),
                     retractall(lazy_dr(I))
     ; lazy_dr(I) -> retractall(lazy_dl(I)),
                     retractall(lazy_dr(I))
     ; assert(lazy_dl(I)),
       assert(lazy_dr(I)).

set_manual_opt_state :-
       retractall('opt state'(_)),
       assert('opt state'(manual)).

% =

display_opt :-
  ( 'opt state'(safe) ->
    Txt = 'Safe, not optimal'
  ; 'opt state'(optimal) ->
    Txt = 'Optimal safe'
  ; 'opt state'(manual) ->
    Txt = 'Manual'
  ; Txt = 'Unknown'
  ),
   my_tk_interpreter(I),
   tcl_eval(I,format('.an.bot.status configure -text " Status: ~w"',[Txt]),_).

analysis :-
     garbage_collect,
     my_tk_interpreter(Interp),
     tcl_eval(Interp,'set analysis_state "running"
                      set_cursor watch watch                   
                      .an.bot.cancel config -cursor left_ptr
                      .an.bot.cancel configure -state normal
                      grab .an.bot.cancel',_),
     on_exception(EXC,analysis1,
       (format1('~p',[EXC]),
        safe,
        tcl_eval(Interp,'.an.bot.status configure -text " Aborted"',_)
       )
            ),
     tcl_eval(Interp,'set_cursor left_ptr left_ptr
                      grab release .an.bot.cancel
                      .an.bot.cancel configure -state disabled',_).

analysis1 :-
     my_tk_interpreter(Interp),
     retractall('start time'(_)),
     statistics(runtime,[T0|_]),
     assert('start time'(T0)),
     tcl_eval(Interp,'.an.bot.status configure -text " Retracting Mode Declarations"',_),
     retract_all_decls,
     findall(U,'unary mode'(U),Us),
     findall(B,'binary mode'(B),Bs),
     findall(p(X,Y,Z),postulate(X,Y,Z),Post2),
     sort(Post2,Post3),
     length(Post3,NumPost),
     set_safe_buttons(Us,Bs,Interp),
     check_convergence(Us,Bs,NumPost,State),
     tcl_eval(Interp,'.an.bot.status configure -text " Analysing Transparency"',_),
     check_transparency(Us,Bs),
     tcl_eval(Interp,'.an.bot.status configure -text " Analysing Continuity"',_),
     check_continuity(Us,Bs,Post3),
     tcl_eval(Interp,'.an.bot.status configure -text " Done"',_),
     retractall('opt state'(_)),
    (var(State) ->
     assert('opt state'(safe))
    ;
     assert('opt state'(State))
    ),
     statistics(runtime,[T|_]),
     retract('start time'(T0)),
     Time is T-T0,
     Min is Time // 60000,
     Sec is Time mod 60000,
    (Min=0 ->
     format('~n% fragment analysed in ~3d sec~n',[Sec])
    ;
     format('~n% fragment analysed in ~d min ~3d sec~n',[Min,Sec])
    ).

check_convergence :-
     my_tk_interpreter(Interp),
     findall(U,'unary mode'(U),Us),
     findall(B,'binary mode'(B),Bs),
     findall(p(X,Y,Z),postulate(X,Y,Z),Post2),
     sort(Post2,Post3),
     length(Post3,NumPost),
     retractall(lazy_dl(_)),
     retractall(lazy_dr(_)),
     retractall(lazy_unpack(_)),
     set_safe_lazy_buttons(Us,Bs,Interp),
     check_convergence(Us,Bs,NumPost,_State),
     tcl_eval(Interp,'.an.bot.status configure -text " Done"',_).

check_transparency :-
     my_tk_interpreter(Interp),
     findall(U,'unary mode'(U),Us),
     findall(B,'binary mode'(B),Bs),
     retractall(transparent(_)),
     retractall(transparent_dia(_)),
     tcl_eval(Interp,'.an.bot.status configure -text " Analysing Transparency"',_),
     set_safe_trans_buttons(Us,Bs,Interp),
     check_transparency(Us,Bs),
     tcl_eval(Interp,'.an.bot.status configure -text " Done"',_).

check_continuity :-
     my_tk_interpreter(Interp),
     findall(U,'unary mode'(U),Us),
     findall(B,'binary mode'(B),Bs),
     findall(p(X,Y,Z),postulate(X,Y,Z),Post2),
     sort(Post2,Post3),
     tcl_eval(Interp,'.an.bot.status configure -text " Analysing Continuity"',_),
     set_safe_cont_buttons(Us,Bs,Interp),
     check_continuity(Us,Bs,Post3),
     tcl_eval(Interp,'.an.bot.status configure -text " Done"',_).

% safe
% 

safe :-
     my_tk_interpreter(Interp),
     findall(U,'unary mode'(U),Us),
     findall(B,'binary mode'(B),Bs),
     format1('1. === Retracting Mode Declarations~n',[]),
     retract_all_decls,
     format1('2. === Disabling Early Failure~n',[]), 
     assert(lazy_dl(_)),
     assert(lazy_dr(_)),
     assert(lazy_unpack(_)),
     set_safe_buttons(Us,Bs,Interp),
     retractall('opt state'(_)),
     assert('opt state'(safe)).

set_safe_buttons(Us,Bs,Interp) :-
     set_safe_lazy_buttons(Us,Bs,Interp),
     set_safe_trans_buttons(Us,Bs,Interp),
     set_safe_cont_buttons(Us,Bs,Interp).

set_safe_lazy_buttons(Us,Bs,Interp) :-
     set_safe_lazy_buttons_u(Us,Interp,0),
     set_safe_lazy_buttons_b(Bs,Interp,0),
     tcl_eval(Interp,'update idletasks',_).

set_safe_lazy_buttons_u([],_,_).

set_safe_lazy_buttons_u([_|Us],Interp,N0) :-
     tcl_eval(Interp,format('.an.ft.r.ub~d select',[N0]),_),
     N is N0+1,
     set_safe_lazy_buttons_u(Us,Interp,N).

set_safe_lazy_buttons_b([],_,_).

set_safe_lazy_buttons_b([_|Bs],Interp,N0) :-
     tcl_eval(Interp,format('.an.ft.r.bb~d select',[N0]),_),
     N is N0+1,
     set_safe_lazy_buttons_b(Bs,Interp,N).

set_safe_trans_buttons(Us,Bs,Interp) :-
     set_safe_trans_buttons_u(Us,Interp,0),
     set_safe_trans_buttons_b(Bs,Interp,0),
     tcl_eval(Interp,'update idletasks',_).

set_safe_trans_buttons_u([],_,_).

set_safe_trans_buttons_u([_|Us],Interp,N0) :-
     tcl_eval(Interp,format('.an.fb.l.ub~d deselect',[N0]),_),
     N is N0+1,
     set_safe_trans_buttons_u(Us,Interp,N).

set_safe_trans_buttons_b([],_,_).

set_safe_trans_buttons_b([_|Bs],Interp,N0) :-
     tcl_eval(Interp,format('.an.fb.l.bb~d deselect',[N0]),_),
     N is N0+1,
     set_safe_trans_buttons_b(Bs,Interp,N).

set_safe_cont_buttons(Us,Bs,Interp) :-
     set_safe_cont_buttons_u(Us,Interp,0),
     set_safe_cont_buttons_b(Bs,Interp,0),
     tcl_eval(Interp,'update idletasks',_).

set_safe_cont_buttons_u([],_,_).

set_safe_cont_buttons_u([_|Us],Interp,N0) :-
     tcl_eval(Interp,format('.an.fb.r.ub~d deselect',[N0]),_),
     N is N0+1,
     set_safe_cont_buttons_u(Us,Interp,N).

set_safe_cont_buttons_b([],_,_).

set_safe_cont_buttons_b([_|Bs],Interp,N0) :-
     tcl_eval(Interp,format('.an.fb.r.bb~d deselect',[N0]),_),
     N is N0+1,
     set_safe_cont_buttons_b(Bs,Interp,N).

% ============================================================
% Convergence
% ============================================================

% check for possible diverging branches in search tree, and assert
% appropriate lazy/1 declarations

% ============================================================

% the method used is the following: for every label X with principal
% constructor unpack, dl or dr which converts to two different labels 
% Y and Z (we call the pair Y,Z a critical pair) the normal form of
% X must be more general than or equal to the normal forms of Y and
% Z. If not, reducing X may commit us to a wrong branch in the search
% space so we use lazy evaluation for redexes of that type. 

check_convergence(U,B,NumPost,State) :-
     my_tk_interpreter(Interp),
     tcl_eval(Interp,'.an.bot.status configure -text " Enumerating Critical Pairs"',_),
     tcl_eval(Interp,'update idletasks',_),
     findall(X-[Y,Z],(safe_rewrite(X,Y),
                      safe_rewrite(X,Z),
                      my_numbervars(X,0,_),
                      Y \== Z),CPs0),
     keysort(CPs0,CPs1),
     collapse_cp(CPs1,CPs),
     length(CPs,NumCP),
     Count is NumCP * NumPost,
    (Count > 15000 -> EstTime = 'IMPOSSIBLY',Unless='you plan to a 2 week vacation while leaving your computer running'
    ;Count >  8000 -> EstTime = 'very, VERY',Unless='you were going to take a nap anyway'
    ;Count >  5000 -> EstTime = 'very, very',Unless='you were going to take a coffee break anyway'
    ;EstTime = very, Unless = 'performace is a real issue'), 
    (Count >  2000 ->
     tcl_eval(Interp,format('dialog .d {Warning} {~w postulates found with ~w critical pairs. Analysing this set of postulates may take ~w long.
We recommend using lazy evaluation for all modes unless ~w.} warning 0 {Use lazy evaluation} {Continue analysis}',[NumPost,NumCP,EstTime,Unless]),Answ),
     tcl_eval(Interp,'update',_),
     tcl_eval(Interp,'grab .an.bot.cancel',_),
    (Answ = "0" ->
     assert(lazy_dr(_)),
     assert(lazy_dl(_)),
     assert(lazy_unpack(_)),
     State = safe
    ;
     tcl_eval(Interp,'.an.bot.status configure -text " Analysing Convergence"',_),
     check_unary_convergence(U,0,CPs),
     check_binary_convergence(B,0,CPs),
     State = optimal
    )
    ;
     tcl_eval(Interp,'.an.bot.status configure -text " Analysing Convergence"',_),
     check_unary_convergence(U,0,CPs),
     check_binary_convergence(B,0,CPs),
     State = optimal
    ).

% = collapse_cp(+CPTail,+CPHead,-Divgs)
% 
% replace multiple critical pairs for the same label
% by a single `critical set'

collapse_cp([],[]).
collapse_cp([X|Xs],Ys) :-
     collapse_cp(Xs,X,Ys).

collapse_cp([],X,[X]).
collapse_cp([Y-L2|Ys],X-L1,Zs0) :-
    (
     X==Y ->
     append(L1,L2,L3),
     sort(L3,L),
     collapse_cp(Ys,X-L,Zs0)
    ;
     Zs0=[X-L1|Zs],
     collapse_cp(Ys,Y-L2,Zs)
    ).

% =

check_unary_convergence([],_,_).
check_unary_convergence([U|Us],N0,CPs) :-
     my_tk_interpreter(Interp),
     tcl_eval(Interp,format('.an.ft.r.ub~d configure -state active',[N0]),_),
     check_u_item_convergence(CPs,U,Interp,N0),
     tcl_eval(Interp,format('.an.ft.r.ub~d configure -state normal',[N0]),_),
     N is N0+1,
     check_unary_convergence(Us,N,CPs).

% =

check_u_item_convergence([X-Ys|_],U,Interp,Num) :-
     my_tk_interpreter(Interp),
     tcl_eval(Interp,'update',_),
     tcl_eval(Interp,format('if {$analysis_state == "cancel"} {
                             .an.ft.r.ub~d configure -state normal
                             prolog raise_exception(''Cancel'')
                             }',[Num]),_),
     normalize1(unpack(U,X),DF),
     findall(DF1,
            (member(Y,Ys),
             normalize1(unpack(U,Y),DF1)),Ss0),
     sort(Ss0,Ss),
     \+ most_general_df(Ss,DF,unpack(U,X),0),
     !,
     tcl_eval(Interp,format('.an.ft.r.ub~d select',[Num]),_).

check_u_item_convergence([_|Rest],U,Interp,Num) :-
     /* first element of list converges */
     check_u_item_convergence(Rest,U,Interp,Num).

check_u_item_convergence([],_,Interp,Num) :-
     /* all elements converge */
     tcl_eval(Interp,format('.an.ft.r.ub~d deselect',[Num]),_).

% =

check_binary_convergence([],_,_).
check_binary_convergence([B|Bs],N0,CPs) :-
     my_tk_interpreter(Interp),
     tcl_eval(Interp,format('.an.ft.r.bb~d configure -state active',[N0]),_),
     check_b_item_convergence(CPs,B,Interp,N0),
     tcl_eval(Interp,format('.an.ft.r.bb~d configure -state normal',[N0]),_),
     N is N0+1,
     check_binary_convergence(Bs,N,CPs).

% =

check_b_item_convergence([X-Ys|Rest],B,Interp,Num) :-
     atomic_sublabel(X,N-V),
     tcl_eval(Interp,'update',_),
     tcl_eval(Interp,format('if {$analysis_state == "cancel"} {
                             .an.ft.r.bb~d configure -state normal
                             prolog raise_exception(''Cancel'')
                             }',[Num]),_),
     normalize1(dl(B,N-V,X),DF),
     findall(DF1,
            (member(Y,Ys),
             normalize1(dl(B,N-V,Y),DF1)),Ss0),
     sort(Ss0,Ss),
     \+ most_general_df(Ss,DF,dl(B,N-V,X),0),
     !,
     tcl_eval(Interp,format('.an.ft.r.bb~d select',[Num]),_),
     check_b_item_convergence0([X-Ys|Rest],B,Interp,Num).

check_b_item_convergence([X-Ys|Rest],B,Interp,Num) :-
     atomic_sublabel(X,N-V),
     tcl_eval(Interp,'update',_),
     tcl_eval(Interp,format('if {$analysis_state == "cancel"} {
                             .an.ft.r.bb~d configure -state normal
                             prolog raise_exception(''Cancel'')
                             }',[Num]),_),
     normalize1(dr(B,X,N-V),DF),
     findall(DF1,
            (member(Y,Ys),
             normalize1(dr(B,Y,N-V),DF1)),Ss0),
     sort(Ss0,Ss),
     \+ most_general_df(Ss,DF,dr(B,X,N-V),0),
     !,
     tcl_eval(Interp,format('.an.ft.r.bb~d select',[Num]),_),
     check_b_item_convergence1([X-Ys|Rest],B,Interp,Num).

check_b_item_convergence([_|Rest],B,Interp,Num) :-
     /* first element of list converges */
     check_b_item_convergence(Rest,B,Interp,Num).

check_b_item_convergence([],_,Interp,Num) :-
     /* all elements converge */
     tcl_eval(Interp,format('.an.ft.r.bb~d deselect',[Num]),_).

% already found a divergence for dl, checking for
% convergence of dr

check_b_item_convergence0([X-Ys|_],B,Interp,Num) :-
     atomic_sublabel(X,N-V),
     tcl_eval(Interp,'update',_),
     tcl_eval(Interp,format('if {$analysis_state == "cancel"} {
                             .an.ft.r.bb~d configure -state normal
                             prolog raise_exception(''Cancel'')
                             }',[Num]),_),
     normalize1(dr(B,X,N-V),DF),
     findall(DF1,
            (select(Y,Ys,_),
             normalize1(dr(B,Y,N-V),DF1)),Ss0),
     sort(Ss0,Ss),
     \+ most_general_df(Ss,DF,dr(B,X,N-V),10),
     !.

check_b_item_convergence0([_|Rest],B,Interp,Num) :-
     /* first element of list converges */
     check_b_item_convergence0(Rest,B,Interp,Num).

check_b_item_convergence0([],_,_,_). 
     /* all elements converge */

% already found a divergence for dr, checking for
% convergence of dl

check_b_item_convergence1([X-Ys|_],B,Interp,Num) :-
     atomic_sublabel(X,N-V),
     tcl_eval(Interp,'update',_),
     tcl_eval(Interp,format('if {$analysis_state == "cancel"} {
                             .an.ft.r.bb~d configure -state normal
                             prolog raise_exception(''Cancel'')
                             }',[Num]),_),
     normalize1(dl(B,N-V,X),DF),
     findall(DF1,
            (select(Y,Ys,_),
             normalize1(dl(B,N-V,Y),DF1)),Ss0),
     sort(Ss0,Ss),
     \+ most_general_df(Ss,DF,dl(B,N-V,X),10),
     !.

check_b_item_convergence1([_|Rest],B,Interp,Num) :-
     /* first element of list converges */
     check_b_item_convergence1(Rest,B,Interp,Num).

check_b_item_convergence1([],_,_,_).
     /* all elements converge */

% = most_general_df(+DFs,+MG,+Label,+Tabs)
% true if MG is more general than any label in DFs
% if not, this predicate asserts a lazy_X declaration,
% produces some output and fails.

most_general_df([],_,_,_).
most_general_df([DF1|Ys],DF,X,N) :-
    (normalize1(DF,DF1) ->
     most_general_df(Ys,DF,X,N)
    ;
     assert_declaration(X),
     format1('~*+diverges: ~p~n~10+--> ~p~n~10+--> ~p~n',[N,X,DF,DF1]),
     fail
    ).

assert_declaration(unpack(U,_)) :-
     assert(lazy_unpack(U)).

assert_declaration(dr(B,_,_)) :-
     assert(lazy_dr(B)).

assert_declaration(dl(B,_,_)) :-
     assert(lazy_dl(B)).

% normalize1(+Label,?Answer)
% true if Answer is a normal form of label
% Answer may be instantiated, and may contain non-external
% modes

normalize1(Node,Answer) :-
     breadth_star2([],1,[Node|B],B,node(Node,0,empty,empty),Answer).

breadth_star2([],N0,[Node|F],B,Closed,Answer) :-
     N0 > 0,
     N is N0-1,
    ( 
     normal1(Node),
     Answer = Node,
     !
    ;
     children(Node,Children),
     union1(Children,Closed,Closed1,Children1,[]),
     breadth_star2(Children1,N,F,B,Closed1,Answer)
    ).

breadth_star2([X|Xs],N0,F,[X|B],Closed,Answer) :-
     N is N0+1,
     breadth_star2(Xs,N,F,B,Closed,Answer).

normal1(_-_).
normal1('$VAR'(_)).
normal1(p(_,A,B)) :-
     normal1(A),
     normal1(B).
normal1(zip(_,A)) :-
     normal1(A).

% ============================================================
% Transparency
% ============================================================

% check_transparency(UnaryModes,BinaryModes)
%
% check for possible reordering of material when placed in a larger
% context, and assert appropriate transparent/1 declarations

% the method used to detect this `contextual reordering' is the 
% following: for each label X which is the left hand side to a
% conversion we compute the set of labels to which it reduces, and
% check if we can produce the same word orders for all sublabels Y
% of X

check_transparency(Us,Bs) :-
     findall(p(X,Y,Z),postulate(X,Y,Z),Ps0),
     sort(Ps0,Ps),
     numberlist(Ps), 
     my_tk_interpreter(Interp),
     check_unary_transparency(Us,Ps,Interp,0),
     check_binary_transparency(Bs,Ps,Interp,0).

check_unary_transparency([],_,_,_).
check_unary_transparency([U|Us],Ps,Interp,Num0) :-
     tcl_eval(Interp,format('.an.fb.l.ub~d configure -state active',[Num0]),_),
     tcl_eval(Interp,'update idletasks',_),
     select(p(P,Q,_),Ps,_),
     sublabel(zip(U,L),P),
     findall(X,enumerate_nfs(zip(U,L),X),Orders1),
     findall(Y,enumerate_nfs(P,Y),Orders2),
     findall(Z,enumerate_nfs(Q,Z),Orders3),
    (\+ included_in(Orders2,Orders3,P,Q)
    ;
     \+ included_in(Orders2,Orders1,P,zip(U,L))
     ),
     !,
     tcl_eval(Interp,format('.an.fb.l.ub~d deselect',[Num0]),_),
     tcl_eval(Interp,format('.an.fb.l.ub~d configure -state normal',[Num0]),_),
     tcl_eval(Interp,'update idletasks',_),
     Num is Num0+1,
     check_unary_transparency(Us,Ps,Interp,Num).

check_unary_transparency([U|Us],Ps,Interp,Num0) :-
     assert(transparent_dia(U)),
     tcl_eval(Interp,format('.an.fb.l.ub~d select',[Num0]),_),
     tcl_eval(Interp,format('.an.fb.l.ub~d configure -state normal',[Num0]),_),
     tcl_eval(Interp,'update idletasks',_),
     Num is Num0+1,
     check_unary_transparency(Us,Ps,Interp,Num).

check_binary_transparency([],_,_,_).
check_binary_transparency([B|Bs],Ps,Interp,Num0) :-
     tcl_eval(Interp,format('.an.fb.l.bb~d configure -state active',[Num0]),_),
     tcl_eval(Interp,'update idletasks',_),
     select(p(P,Q,_),Ps,_),
     sublabel(p(B,L1,L2),P),
     findall(X,enumerate_nfs(p(B,L1,L2),X),Orders1),
     findall(Y,enumerate_nfs(P,Y),Orders2),
     findall(Z,enumerate_nfs(Q,Z),Orders3),
    (\+ included_in(Orders2,Orders3,P,Q)
    ;
     \+ included_in(Orders2,Orders1,P,p(B,L1,L2))
    ),
     !,
     tcl_eval(Interp,format('.an.fb.l.bb~d deselect',[Num0]),_),
     tcl_eval(Interp,format('.an.fb.l.bb~d configure -state normal',[Num0]),_),
     tcl_eval(Interp,'update idletasks',_),
     Num is Num0+1,
     check_binary_transparency(Bs,Ps,Interp,Num).

check_binary_transparency([B|Bs],Ps,Interp,Num0) :-
     assert(transparent(B)),
     tcl_eval(Interp,format('.an.fb.l.bb~d select',[Num0]),_),
     tcl_eval(Interp,format('.an.fb.l.bb~d configure -state normal',[Num0]),_),
     tcl_eval(Interp,'update idletasks',_),
     Num is Num0+1,
     check_binary_transparency(Bs,Ps,Interp,Num).

% included_in(+NFs1,+NFs2,+Label,+Sublabel)
% true if all labels in NFs1 have a sublabel in NFs2 with the
% same word order, if not prints a message and fails

included_in([Y|Ys],Xs,L,SL) :-
    (flatten_label(Y,FY,[]),
     select(X,Xs,_),
     flatten_label(X,FX,[]),
     included_el(FX,FY) ->
     included_in(Ys,Xs,L,SL)
    ; 
     format1('opaque:~n~10+~p --> ~p~n~10+~p --> ???~n',[L,Y,SL]),
     fail
    ).
included_in([],_,_,_).

% included_el(+List1,+List2)
% true if the elements of List1 appear in the same order in List2

included_el([],_).
included_el([X|Xs],[X|Ys]) :-
     !,
     included_el(Xs,Ys).
included_el([X|Xs],[_|Ys]) :-
     included_el([X|Xs],Ys).

% flatten_label(+Label,-DL)
% true if the difference list DL contains all atomic sublabels of
% Label in the correct order.

flatten_label(N-W) --> [N-W].
flatten_label(zip(_,X)) -->
     flatten_label(X).
flatten_label(p(_,X,Y)) -->
     flatten_label(X),
     flatten_label(Y).

% enumerate_nfs(+NormalLabel,-Label)
% enumerates all normal forms for NormalLabel in Label

enumerate_nfs(Node,Answer) :-
     breadth_star3([],1,[Node|B],B,node(Node,0,empty,empty),Answer).

breadth_star3([],N0,[Node|F],B,Closed,Answer) :-
     N0 > 0,
     N is N0-1,
    ( 
     Answer = Node
    ;
     children(Node,Children),
     union1(Children,Closed,Closed1,Children1,[]),
     breadth_star3(Children1,N,F,B,Closed1,Answer)
    ).

breadth_star3([X|Xs],N0,F,[X|B],Closed,Answer) :-
     N is N0+1,
     breadth_star3(Xs,N,F,B,Closed,Answer).

% ============================================================
% Continuity
% ============================================================

check_continuity(Us,Bs,Postulates) :-
     assert_all(Us,continuous_dia),
     my_tk_interpreter(Interp),
     clear_unary_modes(Us,Interp,0),
     check_continuity1(Bs,Postulates,Interp,0).

clear_unary_modes([],_,_).
clear_unary_modes([_|Us],Interp,N0) :-
     tcl_eval(Interp,format('.an.fb.r.ub~d select',[N0]),_),
     N is N0+1,
     clear_unary_modes(Us,Interp,N).

check_continuity1([B|Bs],Postulates,Interp,N0) :-
     tcl_eval(Interp,format('.an.fb.r.bb~d configure -state active',[N0]),_),
     tcl_eval(Interp,'update idletasks',_),
     select(p(X,Y,Z),Postulates,_),
     my_numbervars(X,0,_),
     select_domain1(X,B,Dom1),
     \+ select_domain1(Y,B,Dom1),
     !,
     tcl_eval(Interp,format('.an.fb.r.bb~d configure -state normal',[N0]),_),
     tcl_eval(Interp,format('.an.fb.r.bb~d deselect',[N0]),_),
     tcl_eval(Interp,'update idletasks',_),
     N is N0+1,
     format1('discontinuous: ~p -> ~p~n~15+~p~n',[X,Z,Y]),
     check_continuity1(Bs,Postulates,Interp,N).

check_continuity1([B|Bs],Postulates,Interp,N0) :-
     tcl_eval(Interp,format('.an.fb.r.bb~d configure -state normal',[N0]),_),
     tcl_eval(Interp,format('.an.fb.r.bb~d select',[N0]),_),
     tcl_eval(Interp,'update idletasks',_),
     N is N0+1,
     assert(continuous(B)),
     check_continuity1(Bs,Postulates,Interp,N).

check_continuity1([],_,_,_).

% select_domain1(N-W,B,[]).

select_domain1(zip(_,X),B,L) :-
     select_domain1(X,B,L).

select_domain1(p(I,X,Y),B,L0) :-
    (I=B ->
     domain1(X,L0,L1),
     domain1(Y,L1,[])
    ;
    (select_domain1(X,B,L0)
    ;
     select_domain1(Y,B,L0)
    )).

domain1(N-W) --> [N-W].
domain1(zip(_,X)) --> domain1(X).
domain1(p(_,X,Y)) --> domain1(X),domain1(Y).

subsequence([],_).
subsequence([X|Xs],[Y|Ys]) :-
     (X==Y ->
      subsequence1(Xs,Ys)
     ;
      subsequence([X|Xs],Ys)
     ).

subsequence1([],_).
subsequence1([X|Xs],[X|Ys]) :-
      subsequence1(Xs,Ys).

% ============================================================
% Auxiliaries
% ============================================================

retract_all_decls :-
    retractall(transparent(_)),
    retractall(transparent_l(_)),
    retractall(transparent_r(_)),
    retractall(transparent_dia(_)),
    retractall(lazy_unpack(_)),
    retractall(lazy_dl(_)),
    retractall(lazy_dr(_)),
    retractall(continuous_dia(_)),
    retractall(continuous(_)).

numberlist([]).
numberlist([p(X,_,_)|Xs]) :-
     my_numbervars(X,0,_),
     numberlist(Xs).

my_numbervars(N0-'$VAR'(N0),N0,N) :- !, N is N0+1.
my_numbervars(x-_Const,N,N). 
my_numbervars(p(_,A0,B0),N0,N) :-
     my_numbervars(A0,N0,N1),
     my_numbervars(B0,N1,N).
my_numbervars(dl(_,A0,B0),N0,N) :-
     my_numbervars(A0,N0,N1),
     my_numbervars(B0,N1,N).
my_numbervars(dr(_,A0,B0),N0,N) :-
     my_numbervars(A0,N0,N1),
     my_numbervars(B0,N1,N).
my_numbervars(l(_,A0),N0,N) :-
     my_numbervars(A0,N0,N).
my_numbervars(r(_,B0),N0,N) :-
     my_numbervars(B0,N0,N).
my_numbervars(zip(_,B0),N0,N) :-
     my_numbervars(B0,N0,N).
my_numbervars(unzip(_,B0),N0,N) :-
     my_numbervars(B0,N0,N).
my_numbervars(unpack(_,B0),N0,N) :-
     my_numbervars(B0,N0,N).

% get_modes(UnaryModes,BinaryModes)
%
% returns a set of all modes occuring in the lexicon

get_modes(UnaryModes,BinaryModes) :-
     all_lex_labels(L0),
     all_post_labels(L1),
     all_exa_labels(L2),
     append(L0,L1,L3),
     append(L2,L3,L),
     get_modes(L,U,[],B,[]),
     sort(U,UnaryModes),
     sort(B,BinaryModes).

get_modes([],U,U,B,B).

get_modes([Label|Rest],U0,U,B0,B) :-
     get_item_modes(Label,U0,U1,B0,B1),
     get_modes(Rest,U1,U,B1,B).

get_item_modes('is a variable',U,U,B,B) :- !.
get_item_modes('$VAR'(_),U,U,B,B) :- !.
get_item_modes(lit(_),U,U,B,B) :- !.
get_item_modes(_-_,U,U,B,B) :- !.
get_item_modes(zip(I,A),[I|U0],U,B0,B) :-
     !,
     get_item_modes(A,U0,U,B0,B).
get_item_modes(dia(I,A),[I|U0],U,B0,B) :-
     !,
     get_item_modes(A,U0,U,B0,B).
get_item_modes(unzip(I,A),[I|U0],U,B0,B) :-
     !,
     get_item_modes(A,U0,U,B0,B).
get_item_modes(unpack(I,A),[I|U0],U,B0,B) :-
     !,
     get_item_modes(A,U0,U,B0,B).
get_item_modes(box(I,A),[I|U0],U,B0,B) :-
     !,
     get_item_modes(A,U0,U,B0,B).
get_item_modes(p(I,A,D),U0,U,[I|B0],B) :-
     !,
     get_item_modes(A,U0,U1,B0,B1),
     get_item_modes(D,U1,U,B1,B).
get_item_modes(dl(I,A,D),U0,U,[I|B0],B) :-
     !,
     get_item_modes(A,U0,U1,B0,B1),
     get_item_modes(D,U1,U,B1,B).
get_item_modes(dr(I,A,D),U0,U,[I|B0],B) :-
     !,
     get_item_modes(A,U0,U1,B0,B1),
     get_item_modes(D,U1,U,B1,B).
get_item_modes(l(I,A),U0,U,[I|B0],B) :-
     !,
     get_item_modes(A,U0,U,B0,B).
get_item_modes(r(I,A),U0,U,[I|B0],B) :-
     !,
     get_item_modes(A,U0,U,B0,B).
get_item_modes(_,U,U,B,B).

all_lex_labels(List) :-
     findall(Syn,Syn0^W^Sem^(lex(W,Syn0,Sem),macro_expand(Syn0,Syn)),List0),
     sort(List0,List).

all_exa_labels(List) :-
     findall(Goal,String^(example(String,Goal0),macro_expand(Goal0,Goal)),List0),
     sort(List0,List).

append_dlists([],L,L).

append_dlists([p(L0,L1)|Rest],L0,L) :-
     append_dlists(Rest,L1,L).

all_post_labels(Labels) :-
     findall(X,post(X),Labels0),
     sort(Labels0,Labels).

post(X) :- postulate(X,_,_).
post(X) :- postulate(_,X,_).
post(X) :- postulate1(X,_,_).
post(X) :- postulate1(_,X,_).

safe_rewrite(D,D1) :-
     postulate(D,D1,_).
safe_rewrite('*','*') :- !,fail.
safe_rewrite(unpack(J,D),unpack(J,D1)) :-
     safe_rewrite(D,D1).
safe_rewrite(unzip(J,D),unzip(J,D1)) :-
     safe_rewrite(D,D1).
safe_rewrite(zip(J,D),zip(J,D1)) :-
     safe_rewrite(D,D1).
safe_rewrite(l(J,D),l(J,D1)) :-
     safe_rewrite(D,D1).
safe_rewrite(r(J,D),r(J,D1)) :- 
     safe_rewrite(D,D1).
safe_rewrite(p(J,D,G),p(J,D1,G)) :-
     safe_rewrite(D,D1).
safe_rewrite(p(J,G,D),p(J,G,D1)) :-
     safe_rewrite(D,D1).
safe_rewrite(dr(J,D,G),dr(J,D1,G)) :-
     safe_rewrite(D,D1).
safe_rewrite(dl(J,G,D),dl(J,G,D1)) :-
     safe_rewrite(D,D1).

atomic_sublabel(N-W,N-W).
atomic_sublabel(zip(_,A),N-W) :-
     atomic_sublabel(A,N-W).
atomic_sublabel(p(_,A,B),N-W) :-
    (
     atomic_sublabel(A,N-W)
    ;
     atomic_sublabel(B,N-W)
    ).

sublabel(Z,p(_,X,Y)) :-
     ( Z=X
     ; Z=Y
     ; sublabel(Z,X)
     ; sublabel(Z,Y)
     ).
sublabel(Z,zip(_,X)) :-
     ( Z=X
     ; sublabel(Z,X)
     ).

union1([],AVL,AVL,RN,RN).
union1([X|Xs],AVL0,AVL,RN0,RN) :-
       insert1(AVL0,X,AVL1,RN0,RN1,_),
       union1(Xs,AVL1,AVL,RN1,RN).

insert1(empty,         Key, node(Key,0,empty,empty), [Key|RN], RN, 1).
insert1(node(K,B,L,R), Key, Result, RN0, RN, Delta) :-
        compare(O, Key, K),
	insert(O, Key, Result, Delta, K, B, L, R, RN0, RN).


