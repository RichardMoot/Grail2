:- multifile portray/1.

pp_labels(S0,S) :- format1('~p -->> ~n~p~n',[S0,S]).

pp_sem1(X) :- \+ \+ (numbervars(X,21,_),format1('~p',[X])).

format1(X,Y) :-
       (display_mode(quiet) ->
        true
       ;
        format(X,Y)).

portray(tcl_error(_,Str)) :-
        !,
        format('~s',[Str]).

portray(tcl_eval(X,Y,Z)) :-
        ground(Z),
        !,
        format('tcl_eval(~p,~p,"~s")',[X,Y,Z]).

portray(X:Y-Z) :-
        !,
        print(X),
        write(' : '),
        print(Y),
        write(' - '),
        print(Z).

portray(findall(X,Y,Z)) :-
        !,
       (var(Z) ->
        format('findall(~p): ~p',[X,Y])
       ;
        format('findall(~p): ~p~n',[X,Y]),
        portray_list(Z)).

portray(one(X)) :- !,print(X),write('(1)').
portray(two(X)) :- !,print(X),write('(2)').

portray(prove(L)) :- !,nl,write('== prove =='),nl,portray_list(L).

portray(prove1(Es,E)) :- !,nl,write('== prove =='),nl,print(E),nl,write('==='),nl,portray_list(Es).
 
portray(prove(Es,E)) :- !,nl,write('== prove=='),nl,
    my_numbervars([E|Es],41,_,0,_),portray_vertices([E|Es]).

portray(prove(Es,E,_,_)) :- !,nl,write('== prove=='),nl,
    my_numbervars([E|Es],41,_,0,_),portray_vertices([E|Es]).

portray(select_goal(G,_,_)) :- !,nl,write('== select_goal =='),
    nl,print(G).

portray(select_conjugate(_,[C|_],_)) :- !,nl,write('== select_conj =='),
    nl,print(C).

portray(select_atom(A,vertex(N,_,_),_,_)) :- !,write('== select_atom
=='),
    nl,print(N),write(':'),print(A),nl.

portray(select_conj(A,vertex(N,_,_),_,_)) :- !,write('== select_conj
=='), nl, print(N),write(':'),print(A),nl.
portray(ord_union(_,_,_,New)) :- 
        var(New) -> print(New) 
       ;
        nl,write('== new children =='),
	nl,portray_list(New).

portray(children(Parent,Children)) :-
        nonvar(Children),!,
        nl,write('== parent =='),nl,print(Parent),nl,
        write('== children =='),nl,portray_list(Children).


portray(generate_sequent(Ss,Os,Ts,_,_,_)) :-
        nl,write('== seqents =='),nl,portray_list(Ss),
        nl,write('== ones =='),nl,portray_list(Os),
        nl,write('== twos =='),nl,portray_list(Ts),nl.

portray(generate_nd(NDs,Os,Ts,_,_,_,_,_)) :-
        nl,write('== nds =='),nl,portray_list(NDs),
        nl,write('== ones =='),nl,portray_list(Os),
        nl,write('== twos =='),nl,portray_list(Ts),nl.

portray(generate_fitch(NDs,Os,Ts,_,_,_,_,_,_,_,_)) :-
        nl,write('== nds =='),nl,portray_list(NDs),
        nl,write('== ones =='),nl,portray_list(Os),
        nl,write('== twos =='),nl,portray_list(Ts),nl.
        
portray(rule(N,A,S,_)) :-
        !,write_ant(A),write(' '),print(N),write('=> '),print(S).
portray(sign(A,0,B,C,_)) :-
	!,write('+'),print(C),write(':'),print(A),write(':'),
        \+ \+ (numbervars(B,23,_),print(B)).
portray(sign(A,1,B,C,_)) :-
	!,write('-'),print(C),write(':'),print(A),write(':'),
        \+ \+ (numbervars(B,23,_),print(B)).

portray(neg(A,_,S,P0,P1)) :-
        !,write('-'),print(A),write(':'),print(S),write(':'),print(P0),
        write('-'),print(P1).
portray(pos(A,_,S,P0,P1)) :-
        !,write('+'),print(A),write(':'),print(S),write(':'),print(P0),
        write('-'),print(P1).

portray(neg(A,_,S,Id,_,_)) :-
        !,
        format('-~p(~p):~p',[A,Id,S]).

portray(pos(A,_,S,Id,_,_)) :-
        !,
        format('+~p(~p):~p',[A,Id,S]).

portray(neg(A,_,_)) :-
        !,write('-'),print(A).
portray(pos(A,_,_)) :-
        !,write('+'),print(A).
portray(neg(A)) :-
        !,write('-'),print(A).
portray(pos(A)) :-
        !,write('+'),print(A).
portray(lolli(A,B)) :-
        !,write('('),print(A),write(' -0 '),print(B),write(')').
portray(tensor(A,B)) :-
        !,write('('),print(A),write(' @ '),print(B),write(')').
portray(atom(A,_M,_S)) :-
        !,print(A).

%portray(X-Y) :- nonvar(X),X='$VAR'(N),!,print('$VAR'(N)),print('-'),print(Y).
portray(X-Y) :- nonvar(X),(X=x;number(X)),(atom(Y);Y='$VAR'(_)),!,print(Y).
%portray(X-Y) :- nonvar(X), !, write('L-'),print(Y).

portray(lit(A)) :- !, print(A).


portray(sp(I,X,Y)) :-
    write('('),print(X),write(','),print(Y),write(')'),print(I),write(' ').
portray(sdia(I,X)) :-
    write('<'),print(X),write('>'),print(I),write(' ').
portray(dia(I,X)) :-
    write('^'),print(I),write('.'),print(X).
portray(box(I,X)) :-
    write('@'),print(I),write('.'),print(X).
portray(p(I,X,Y)) :-
    write('('),print(X),write(' *'),write(I),write(' '),print(Y),write(')').
portray(dr(I,X,Y)) :-
    write('('),print(X),write(' /'),write(I),write(' '),print(Y),write(')').
portray(dl(I,X,Y)) :-
    write('('),print(X),write(' \'),write(I),write(' '),print(Y),write(')').
portray(l(I,X)) :-
    write('<'),write(I),write('('),print(X),write(')').
portray(r(I,X)) :-
    write('>'),write(I),write('('),print(X),write(')').
portray(zip(I,X)) :-
    write('^'),write(I),write('('),print(X),write(')').
portray(unzip(I,X)) :-
    write('#'),write(I),write('('),print(X),write(')').
portray(unpack(I,X)) :-
    write('@'),write(I),write('('),print(X),write(')').
portray(dom(X)) :-
    write('{'),print(X),write('}').

portray(lambda(X,Y)) :- !,write('^'),write(X),write('.'),print(Y).

portray(appl(appl(appl(F,Z),Y),X)) :-
     atom(F),
     !,print(F),write('('),print(X),write(','),
     print(Y),write(','),print(Z),write(')').    

portray(appl(appl(F,Y),X)) :-
     atom(F),
     !,print(F),write('('),print(X),write(','),
     print(Y),write(')').

portray(appl(X,Y)) :- !,print(X),write('('),print(Y),write(')').

portray(pair(X,Y)) :- !,write('<'),print(X),write(','),print(Y),write(' >').

portray(pi1(X)) :- !,write('pi1('),print(X),write(')').

portray(pi2(X)) :- !,write('pi2('),print(X),write(')').

portray(bool(X,Y,Z)) :- !,write('('),print(X),write(' '),write(Y),write(' '),print(Z),write(')').

portray(quant(Q,X,T)) :- !,write(Q),write(' '),write(X),write('['),print(T),write(']').

moot_message :- format('~nAnd so it begins...~2nHiya, boss.',[]), datime(datime(Y,M,D,H,_,_)), ( M = 12, D = 24 -> Old is Y-1972,format(' ~w years old. Happy birthday, oldtimer!',[Old]) ; true),( M = 8, D = 7 -> Old is Y-1997,format(' I''m ~w years old today. Happy birthday, me!',[Old]) ; true), ( Y = 2000, M = 1, D < 10 -> format(' See, no year 2000 problem here!',[]) ; true),( H < 10 -> format(' Working early today!',[]) ; H > 18 -> format(' Late debugging session?',[]) ; true),nl.

portray_list([]) :- 
     !,
     format1('empty~n',[]).

portray_list([A]) :- 
     !,
     format1('~p~n===~n',[A]).

portray_list([A|As]) :-
     format1('~p~n',[A]),
     portray_list(As).

portray_selection([],_) :-
	nl.
portray_selection([X|Xs],Y) :-
      ( X == Y
      ->
	  format1('[~p]~n',[X])
      ;
	  format1(' ~p~n',[X])
      ),
      portray_selection(Xs,Y).

print_sentences([]).
print_sentences([X|Xs]) :- print(X),write(' => s'),nl,
     print_sentences(Xs).

portray_vertices([]) :- 
     !,
     write('empty'),
     nl.

portray_vertices([vertex(A,B,C)]) :- 
     !,
     write(A),write(' ['),
     portray_atoms(B),
     write(']-'),
     print(C),
     nl,
     write('==='),
     nl.

portray_vertices([vertex(A,B,C)|As]) :- 
     write(A),write(' ['),
     portray_atoms(B),
     write(']-'),
     print(C),
     nl,
     portray_vertices(As).

portray_atoms([]).

portray_atoms([A]) :- 
     !,
     print(A).

portray_atoms([A|As]) :- 
     print(A),
     nl,
     tab(3),
     portray_atoms(As).

my_numbervars([],Lab,Lab,Pos,Pos).

my_numbervars([vertex(_,As,_)|Rest],Lab0,Lab,Pos0,Pos) :-
    my_numbervars1(As,Lab0,Lab1,Pos0,Pos1),
    my_numbervars(Rest,Lab1,Lab,Pos1,Pos).


my_numbervars1([],Lab,Lab,Pos,Pos).
my_numbervars1([X|Xs],Lab0,Lab,Pos0,Pos) :-
    my_numbervars2(X,Lab0,Lab1,Pos0,Pos1),
    my_numbervars1(Xs,Lab1,Lab,Pos1,Pos).

my_numbervars2(neg(_,_,_,L,P0,P1),Lab0,Lab,Pos0,Pos) :-
    numbervars(L,Lab0,Lab),
    numbervars(P0,Pos0,Pos1),
    numbervars(P1,Pos1,Pos).

my_numbervars2(pos(_,_,_,L,P0,P1),Lab0,Lab,Pos0,Pos) :-
    numbervars(L,Lab0,Lab),
    numbervars(P0,Pos0,Pos1),
    numbervars(P1,Pos1,Pos).

my_numbervars2(neg(_,_,L,P0,P1),Lab0,Lab,Pos0,Pos) :-
    numbervars(L,Lab0,Lab),
    numbervars(P0,Pos0,Pos1),
    numbervars(P1,Pos1,Pos).

my_numbervars2(pos(_,_,L,P0,P1),Lab0,Lab,Pos0,Pos) :-
    numbervars(L,Lab0,Lab),
    numbervars(P0,Pos0,Pos1),
    numbervars(P1,Pos1,Pos).

write_ant(X) :- var(X),!,write(X).

write_ant(sp(I,X,Y)) :-
    !,
    write('('),write_ant(X),write(','),write_ant(Y),write(')'),print(I),write(' ').
write_ant(sdia(I,X)) :-
    !,
    write('<'),write_ant(X),write('>'),print(I),write(' ').
write_ant(Type) :-
    print(Type).

is_string([]).
is_string([C|Cs]) :-
    number(C),
    is_string(Cs).


portray_examples :-
	format('~61t~60|~n',[]),
	format('=~t Examples ~t=~60|~n',[]),
	format('~61t~60|~n',[]),
        findall(S-F,example(S,F),List),
        portray_examples(List).

portray_lexicon :-
	findall(W,X^Y^lex(W,X,Y),Ws0),
	sort(Ws0,Ws),
	length(Ws,L0),
	L is integer((L0/3)+1),
	split_num(Ws,L,Lex1,Ws1),
	split_num(Ws1,L,Lex2,Ws2),
	split_num(Ws2,L,Lex3,[]),
	format('~61t~60|~n',[]),
	format('=~t Lexicon ~t=~60|~n',[]),
	format('~61t~60|~2n',[]),
	portray_lexicon(Lex1,Lex2,Lex3).

portray_lexicon([],[],[]).

portray_lexicon([X|Xs],[Y|Ys],[Z|Zs]) :-
	format('~w~t~20|~w~t~40|~w~t~60|~n',[X,Y,Z]),
	portray_lexicon(Xs,Ys,Zs).

split_num([],N0,Ys,[]) :-
	( N0 > 0 ->
	    N is N0-1,
	    Ys = [''|Ys0],
	    split_num([],N,Ys0,[])
	;
	    Ys=[]
	).
	
split_num([X|Xs],N0,Ys,Zs) :-
	( N0 =< 0 ->
	    Zs=[X|Xs],
	    Ys=[]
	;
	    N is N0-1,
	    Ys=[X|Ys0],
	    split_num(Xs,N,Ys0,Zs)
	).
