% ============================================================
% File header
% ============================================================
% !labels v1.0

% This fragment illustrates the elementary features of the
% analysis of Verb Raising in Moortgat & Oehrle (1997).
% There's two binary modes: h for head adjunction, a for
% regular phrasal composition -- a *not* taken as associative,
% sorry for the label!
% There is also two unary control modes: h identifies phrasal
% heads, lh lexical heads. The predicates are `locked' with
% the lh feature (box). Verb raising is forced by the
% diamond distributivity laws: the h diamond percolates
% through *phrasal* structure, the lh diamond through
% lexical clusters, where it distributes strongly,
% identifying the two components of such a cluster
% as lexical heads.
% Particle incorporation is handled by a mode p
% which, in the sortal hierarchy of composition modes,
% is `in between' lexical and phrasal.

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

% = head adjunction

postulate(zip(0,A),zip(1,A),'P1').                    % lexical head
%postulate(zip(3,A),zip(2,A),'P8').        
%postulate(zip(0,A),zip(3,A),'P9').                  
postulate(p(1,zip(1,A),B),zip(1,p(1,A,B)),'P2').
postulate(p(1,A,zip(1,B)),zip(1,p(1,A,B)),'P3').
postulate(p(0,zip(0,A),zip(0,B)),zip(0,p(0,A,B)),'P4'). % vr
postulate(p(0,B,p(1,A,C)),p(1,A,p(0,B,C)),'P5').       % mx comm
postulate(p(0,A,p(1,B,C)),p(1,p(0,A,B),C),'P6').      % mx ass

postulate(zip(0,p(2,A,B)),p(1,A,zip(0,B)),'P7').

% v1 fronting/extraction

postulate1(p(1,p(1,zip(4,A),B),C),p(1,zip(4,A),p(1,B,C)),'P10').
postulate1(p(1,B,p(1,zip(4,A),C)),p(1,zip(4,A),p(1,B,C)),'P11').
postulate1(p(1,B,zip(4,A)),p(1,zip(4,A),B),'P12').

continuous_dia(_).

% = non-internal modes

external(_).

external_dia(_).

% = transparency

transparent(_).
transparent_dia(_).

lazy_unpack(1).


% ============================================================
% Macros
% ============================================================

macro(bang(M,F),dia(M,box(M,F)) ).
macro(iv,dl(1,np,s)).  % verb phrase
macro(tv,dl(1,np,iv)).
macro(qpro_main,dr(1,q_main,dl(1,bang(4,np),box(2,s))) ).
macro(qpro_sub,dr(1,q_sub,dl(1,bang(4,np),box(1,s))) ).

% = macro(Form,Replacement)

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Syn,Sem).

lex(dat,dr(1,dl(1,n,n),dl(1,bang(4,np),s)),dat).

%lex(wie,qpro_main,wie).
%lex(wat,qpro_main,wat).
%lex(welk,dr(1,qpro_main,n),welk).

%lex(wie,qpro_sub,wie).
%lex(wat,qpro_sub,wat).
%lex(welk,dr(1,qpro_sub,n),welk).

lex(boek,n,boek).
lex(meisje,n,meisje).
lex(fred,np,fred).
lex(marie,np,mary).
lex(gek,ap,silly).
lex(vinden,box(0,dl(1,ap,dl(1,np,inf))),find).
lex(vindt,box(0,dl(1,ap,tv)),find).
lex(zal,box(0,dr(0,iv,inf)),
	lambda(Inf,lambda(Subj,
		appl('FUT',appl(Inf,Subj))))
	).
lex(wil,box(0,dr(0,iv,inf)),want).
lex(moeten,box(0,dr(0,inf,inf)),must).
lex(plagen,box(0,dl(1,np,inf)),tease).
lex(plaagt,box(0,tv),tease).
lex(weg,prt,away).
lex(sturen,dl(2,prt,box(0,dl(1,np,inf))),send).
lex(stuurt,dl(2,prt,box(0,tv)),send).
lex(als,dr(1,als,box(1,s)),if).
lex(dan,dr(1,dan,box(2,s)),then).

% ============================================================
% Examples
% ============================================================

example("wie Marie plaagt",q_sub).
example("als Fred Marie plaagt",als).
example("dan zal Marie Fred weg sturen",dan).
example("welk meisje stuurt Marie weg",q_main).

% ============================================================

