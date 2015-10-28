% ============================================================
% clitics.pl
% ============================================================
% !labels v1.0

% This fragment goes with the analysis of French clitics
% in Kraak (1997). The fragment was written by Willemijn
% Vermaat, as an assignment for the OTS Categorial Grammar
% course. Updated with the `modally controlled' extraction
% analysis of Moortgat (LoLa, 1997).
%
% EXERCISE1. Extend the fragment with the clitic climbing
% analysis of Kraak (1997) for temporal auxiliaries `avoir'
% `etre'.
% EXERCISE2. Replace the ad-hoc treatment of feature info
% by one which uses modal control operators, cf Heylen (1997)
% and the `agreement.pl' fragment.

:- abolish(lazy_unpack/1).
:- abolish(lazy_dr/1).
:- abolish(lazy_dl/1).
:- abolish(transparent_dia/1).
:- abolish(transparent/1).
:- abolish(continuous_dia/1).
:- abolish(continuous/1).
:- abolish(external_dia/1).
:- abolish(external/1).
:- abolish(postulate/3).
:- abolish(postulate1/3).
:- abolish(macro/2).
:- abolish(lex/3).
:- abolish(example/2).

:- dynamic lazy_unpack/1,lazy_dr/1,lazy_dl/1.
:- dynamic transparent_dia/1,transparent/1.
:- dynamic continuous_dia/1,continuous/1.
:- dynamic external_dia/1,external/1.
:- dynamic postulate/3,postulate1/3.
:- dynamic macro/2,lex/3,example/2.

% ============================================================
% Postulates
% ============================================================

% Postulates:
% binairy modes:  `ph'  phrasal
%                 `h'   for clitization
% unary modes:    `h'   for phrasal head marking
%                 `lh'  for lexical head marking
%                 `i' for islands

%		`p' for modally controlled extraction

% = structural postulates

postulate(zip(lh(0),A), zip(h(0),A), 'LH').
postulate(zip(lh(1),A), zip(h(1),A), 'LH').
postulate(zip(lh(2),A), zip(h(2),A), 'LH').
postulate(zip(lh(3),A), zip(h(3),A), 'LH').
postulate(zip(lh(4),A), zip(h(4),A), 'LH').
postulate(zip(lh(5),A), zip(h(5),A), 'LH').
postulate(zip(lh(6),A), zip(h(6),A), 'LH').
postulate(zip(lh(7),A), zip(h(7),A), 'LH').
postulate(zip(lh(8),A), zip(h(8),A), 'LH').
postulate(zip(lh(8),A), zip(lh(0),A), 'T->B').
postulate(p(ph,zip(h(0),A),B), zip(h(0),p(ph,A,B)), 'K1').
postulate(p(ph,zip(h(1),A),B), zip(h(1),p(ph,A,B)), 'K1').
postulate(p(ph,zip(h(2),A),B), zip(h(2),p(ph,A,B)), 'K1').
postulate(p(ph,zip(h(3),A),B), zip(h(3),p(ph,A,B)), 'K1').
postulate(p(ph,zip(h(4),A),B), zip(h(4),p(ph,A,B)), 'K1').
postulate(p(ph,zip(h(5),A),B), zip(h(5),p(ph,A,B)), 'K1').
postulate(p(ph,zip(h(6),A),B), zip(h(6),p(ph,A,B)), 'K1').
postulate(p(ph,zip(h(7),A),B), zip(h(7),p(ph,A,B)), 'K1').
postulate(p(ph,zip(h(8),A),B), zip(h(8),p(ph,A,B)), 'K1').
postulate(p(ph,A,zip(h(0),B)), zip(h(0),p(ph,A,B)), 'K2').
postulate(p(ph,A,zip(h(1),B)), zip(h(1),p(ph,A,B)), 'K2').
postulate(p(ph,A,zip(h(2),B)), zip(h(2),p(ph,A,B)), 'K2').
postulate(p(ph,A,zip(h(3),B)), zip(h(3),p(ph,A,B)), 'K2').
postulate(p(ph,A,zip(h(4),B)), zip(h(4),p(ph,A,B)), 'K2').
postulate(p(ph,A,zip(h(5),B)), zip(h(5),p(ph,A,B)), 'K2').
postulate(p(ph,A,zip(h(6),B)), zip(h(6),p(ph,A,B)), 'K2').
postulate(p(ph,A,zip(h(7),B)), zip(h(7),p(ph,A,B)), 'K2').
postulate(p(ph,A,zip(h(8),B)), zip(h(8),p(ph,A,B)), 'K2').
postulate(p(h,zip(lh(0),A),zip(lh(1),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(0),A),zip(lh(2),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(0),A),zip(lh(3),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(0),A),zip(lh(4),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(0),A),zip(lh(5),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(0),A),zip(lh(6),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(0),A),zip(lh(7),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(0),A),zip(lh(8),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(2),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(3),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(4),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(5),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(6),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(7),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(8),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(3),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(4),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(5),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(6),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(7),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(8),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(4),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(5),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(6),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(7),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(8),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(5),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(6),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(7),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(8),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(6),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(7),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(8),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(7),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(8),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(7),A),zip(lh(7),B)), zip(lh(0),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(2),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(3),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(4),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(5),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(6),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(7),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(1),A),zip(lh(8),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(3),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(4),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(5),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(6),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(7),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(8),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(4),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(5),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(6),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(7),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(8),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(5),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(6),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(7),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(8),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(6),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(7),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(8),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(7),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(8),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(7),A),zip(lh(7),B)), zip(lh(1),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(3),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(4),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(5),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(6),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(7),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(2),A),zip(lh(8),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(4),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(5),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(6),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(7),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(8),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(5),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(6),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(7),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(8),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(6),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(7),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(8),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(7),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(8),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(7),A),zip(lh(7),B)), zip(lh(2),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(4),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(5),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(6),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(7),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(3),A),zip(lh(8),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(5),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(6),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(7),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(8),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(6),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(7),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(8),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(7),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(8),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(7),A),zip(lh(7),B)), zip(lh(3),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(5),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(6),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(7),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(4),A),zip(lh(8),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(6),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(7),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(8),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(7),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(8),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(7),A),zip(lh(7),B)), zip(lh(4),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(6),B)), zip(lh(5),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(7),B)), zip(lh(5),p(h,A,B)), 'K').
postulate(p(h,zip(lh(5),A),zip(lh(8),B)), zip(lh(5),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(7),B)), zip(lh(5),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(8),B)), zip(lh(5),p(h,A,B)), 'K').
postulate(p(h,zip(lh(7),A),zip(lh(7),B)), zip(lh(5),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(7),B)), zip(lh(6),p(h,A,B)), 'K').
postulate(p(h,zip(lh(6),A),zip(lh(8),B)), zip(lh(6),p(h,A,B)), 'K').
postulate(p(h,zip(lh(7),A),zip(lh(7),B)), zip(lh(6),p(h,A,B)), 'K').
postulate(p(h,zip(lh(7),A),zip(lh(7),B)), zip(lh(7),p(h,A,B)), 'K').
postulate(p(h,zip(lh(0),A),p(ph,B,C)), p(ph,p(h,zip(lh(0),A),B),C), 'MA').
postulate(p(h,zip(lh(1),A),p(ph,B,C)), p(ph,p(h,zip(lh(1),A),B),C), 'MA').
postulate(p(h,zip(lh(2),A),p(ph,B,C)), p(ph,p(h,zip(lh(2),A),B),C), 'MA').
postulate(p(h,zip(lh(3),A),p(ph,B,C)), p(ph,p(h,zip(lh(3),A),B),C), 'MA').
postulate(p(h,zip(lh(4),A),p(ph,B,C)), p(ph,p(h,zip(lh(4),A),B),C), 'MA').
postulate(p(h,zip(lh(5),A),p(ph,B,C)), p(ph,p(h,zip(lh(5),A),B),C), 'MA').
postulate(p(h,zip(lh(6),A),p(ph,B,C)), p(ph,p(h,zip(lh(6),A),B),C), 'MA').
postulate(p(h,zip(lh(7),A),p(ph,B,C)), p(ph,p(h,zip(lh(7),A),B),C), 'MA').
postulate(p(h,zip(lh(8),A),p(ph,B,C)), p(ph,p(h,zip(lh(8),A),B),C), 'MA').
postulate(p(ph,A,zip(p,B)), p(ph,zip(p,B),A), 'Cp').
postulate(p(ph,p(ph,zip(p,A),B),C), p(ph,zip(p,A),p(ph,B,C)), 'MAp').
postulate(p(ph,A,p(ph,zip(p,B),C)), p(ph,zip(p,B),p(ph,A,C)), 'MCp').

% = lazy evaluation

lazy_unpack(h(_)).

% = transparency

transparent(_).

transparent_dia(_).

% = continuity

continuous(h).

continuous_dia(_).

% = non internal modes

external(_).

external_dia(i).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(cl, box(h(0),s)).
macro(up(A,B,C), dl(A,dia(p,box(p,C)),B)).
macro(clitic(A,B,C), box(lh(A),dr(h,C,up(ph,C,B)))).
macro(vp(iv), dl(ph,np_n,s)).
macro(vp(tv), dr(ph,vp(iv),np_a)).
macro(vp(tdv), dr(ph,vp(iv),np_d)).
macro(vp(dtv), dr(ph,vp(tdv),np_a)).
macro(vp(inf), inf).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(marie, np_n, marie).
lex(jean, np_n, jean).
lex(jean, np_a, jean).
lex(marie, np_a, marie).
lex(une_pomme, np_a, une_pomme).
lex(un_livre, np_a, un_livre).
lex(le, clitic(4,np_a,vp(iv)), lambda(A,lambda(B,appl(appl(A,le),B)))).
lex(le, clitic(4,np_a,vp(inf)), lambda(A,lambda(B,appl(appl(A,le),B)))).
lex(la, clitic(4,np_a,vp(iv)), lambda(A,lambda(B,appl(appl(A,la),B)))).
lex(la, clitic(4,np_a,vp(inf)), lambda(A,lambda(B,appl(appl(A,la),B)))).
lex(a_jean, np_d, jean).
lex(lui, clitic(5,np_d,vp(iv)), lambda(A,lambda(B,appl(appl(A,lui),B)))).
lex(lui, clitic(5,np_d,vp(inf)), lambda(A,lambda(B,appl(appl(A,lui),B)))).
lex(me, clitic(3,np_d,vp(iv)), lambda(A,lambda(B,appl(appl(A,me),B)))).
lex(me, clitic(3,np_d,vp(inf)), lambda(A,lambda(B,appl(appl(A,me),B)))).
lex(lire, box(lh(8),dr(ph,vp(inf),np_a)), lire).
lex(manger, box(lh(8),dr(ph,vp(inf),np_a)), manger).
lex(dormir, box(lh(8),vp(inf)), dormir).
lex(dort, box(lh(8),vp(iv)), lambda(A,appl(dort,A))).
lex(mange, box(lh(8),vp(tv)), lambda(A,lambda(B,appl(appl(mange,A),B)))).
lex(donne, box(lh(8),vp(dtv)), lambda(A,lambda(B,lambda(C,appl(appl(appl(donne,B),A),C))))).
lex(laisse, box(lh(8),dr(ph,dr(ph,vp(iv),dia(i,vp(inf))),np_a)), lambda(A,lambda(B,lambda(C,appl(appl(laisse,appl(B,A)),C))))).
lex(laisse, box(lh(8),dr(ph,dr(ph,vp(iv),np_d),box(h(0),vp(inf)))), lambda(A,lambda(B,lambda(C,appl(appl(laisse,appl(A,B)),C))))).
lex(veut, box(lh(8),dr(ph,vp(iv),dia(i,box(h(0),vp(inf))))), lambda(A,lambda(B,appl(appl(veut,appl(A,B)),B)))).
lex(voulait, box(lh(8),dr(ph,vp(iv),dia(i,box(h(0),vp(inf))))), lambda(A,lambda(B,appl(appl(veut,appl(A,B)),B)))).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example([32,77,97,114,105,101,32,109,97,110,103,101,32,117,110,101,95,112,111,109,109,101,46], box(h(0),s)).
example([32,77,97,114,105,101,32,108,97,32,109,97,110,103,101,46], box(h(0),s)).
example([32,77,97,114,105,101,32,108,97,32,108,117,105,32,100,111,110,110,101,46], box(h(0),s)).
example([32,77,97,114,105,101,32,100,111,110,110,101,32,117,110,101,95,112,111,109,109,101,32,97,95,106,101,97,110,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,97,32,109,101,32,100,111,110,110,101,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,117,105,32,108,97,32,100,111,110,110,101,46], box(h(0),s)).
example([32,77,97,114,105,101,32,109,101,32,108,97,32,100,111,110,110,101,46], box(h(0),s)).
example([32,77,97,114,105,101,32,108,97,32,100,111,110,110,101,32,97,95,106,101,97,110,46], box(h(0),s)).
example([32,77,97,114,105,101,32,108,117,105,32,100,111,110,110,101,32,117,110,101,95,112,111,109,109,101,46], box(h(0),s)).
example([32,77,97,114,105,101,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,117,110,101,95,112,111,109,109,101,32,97,95,106,101,97,110,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,101,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,117,110,101,95,112,111,109,109,101,46], box(h(0),s)).
example([32,77,97,114,105,101,32,118,101,117,116,32,108,97,32,109,97,110,103,101,114,46], box(h(0),s)).
example([32,77,97,114,105,101,32,118,101,117,116,32,100,111,114,109,105,114,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,97,32,118,101,117,116,32,109,97,110,103,101,114,46], box(h(0),s)).
example([32,77,97,114,105,101,32,118,101,117,116,32,109,97,110,103,101,114,32,117,110,101,95,112,111,109,109,101,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,97,105,115,115,101,32,74,101,97,110,32,109,97,110,103,101,114,32,117,110,101,95,112,111,109,109,101,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,101,32,108,97,105,115,115,101,32,108,97,32,109,97,110,103,101,114,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,74,101,97,110,32,117,110,101,95,112,111,109,109,101,46], box(h(0),s)).
example([32,77,97,114,105,101,32,108,117,105,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,117,110,101,95,112,111,109,109,101,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,97,105,115,115,101,32,74,101,97,110,32,108,97,32,109,97,110,103,101,114,46], box(h(0),s)).
example([32,77,97,114,105,101,32,108,117,105,32,108,97,105,115,115,101,32,108,97,32,109,97,110,103,101,114,46], box(h(0),s)).
example([32,77,97,114,105,101,32,108,97,32,108,117,105,32,108,97,105,115,115,101,32,109,97,110,103,101,114,46], box(h(0),s)).
example([32,77,97,114,105,101,32,109,101,32,108,97,32,108,97,105,115,115,101,32,109,97,110,103,101,114,46], box(h(0),s)).
example([32,77,97,114,105,101,32,109,101,32,108,97,105,115,115,101,32,108,97,32,109,97,110,103,101,114,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,97,105,115,115,101,32,74,101,97,110,32,100,111,114,109,105,114,46], box(h(0),s)).
example([42,77,97,114,105,101,32,108,101,32,108,97,105,115,115,101,32,100,111,114,109,105,114,46], box(h(0),s)).
example([32,77,97,114,105,101,32,109,101,32,108,97,105,115,115,101,32,100,111,114,109,105,114,46], box(h(0),s)).
example([32,77,97,114,105,101,32,109,101,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,117,110,101,95,112,111,109,109,101,46], box(h(0),s)).
