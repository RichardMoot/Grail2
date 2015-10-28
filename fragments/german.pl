% ============================================================
% german.pl
% ============================================================
% !labels v1.0

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


% German Noun Phrase Agreement
% Agreement in German noun phrases involves a number of features:
% case, gender, number, person and declension class.
% There are many syncretisms for which we use underspecified lexical entries.
% The word "Frau", for instance can be left underspecified for case and
% declension class.
% We use unary modalities (box modalities) to encode feature information.
% The entry for Frau looks like this:
% lex(frau,box(c,                      (case)
%          box(fe,                     (feminine)
%          box(sg,                     (singular)
%          box(th,                     (third)
%          box(d,n))))),frau).         (declension)
% Modifiers and specifiers have the form [agr](N/N), ie the features are placed before
% the complex functor category (N/N).
% lex(guten,box(ge,
%           box(g,
%           box(n,
%           box(pers,
%           box(we,dr(m,n,n)))))),lambda(P,appl(guten,P))).
%  = [ge][g][n][pers][we](N/N)
% Inclusion postulates define a kind of type-hierarchy.
% Features like no, da, ac, ge are specified as subtypes of case (c).
% To ensure agreement behaviour we define distribution postulates.
% Agreement features are checked in modifier/head and specifier/head constructions
% by checking both the modifier/specifier and the head.
% For more details: http://wwwots.let.ruu.nl/~Dirk.Heylen/nancy.ps
% Exercises: (1) Add lexical entries.
%            (2) Define a grammar for agreement phenomena in your
%                favourite language.

% Postulates
% ============================================================

% Distribution Postulates: case
% Features:
% no: nominative
% ac: accusative
% da: dative
% ge: genitive
% c : case
% no, ac, da, ge simply distribute over m (=modifier) product
postulate( p(m,zip(no,A), zip(no,B)), zip(no,p(m,A,B)),'no').
postulate( p(m,zip(ac,A), zip(ac,B)), zip(ac,p(m,A,B)),'ac').
postulate( p(m,zip(da,A), zip(da,B)), zip(da,p(m,A,B)),'da').
postulate( p(m,zip(ge,A), zip(ge,B)), zip(ge,p(m,A,B)),'ge').

% Distribution Postulates: gender
% Features:
% ma: masculine
% fe: feminine
% ne: neuter
% g: gender
% ma, fe, ne: simply distribute over m (=modifier) product
postulate( p(m,zip(ma,A), zip(ma,B)), zip(ma,p(m,A,B)),'ma').
postulate( p(m,zip(fe,A), zip(fe,B)), zip(fe,p(m,A,B)),'fe').
postulate( p(m,zip(ne,A), zip(ne,B)), zip(ne,p(m,A,B)),'ne').

% Distribution Postulates: declension
% Features:
% st: strong
% we: weak
% mi: mixed
% d: declension
% st, we, mi: simply distribute over m (=modifier) product
postulate( p(m,zip(st,A), zip(st,B)), zip(st,p(m,A,B)),'st').
postulate( p(m,zip(we,A), zip(we,B)), zip(we,p(m,A,B)),'we').
postulate( p(m,zip(mi,A), zip(mi,B)), zip(mi,p(m,A,B)),'mi').

% Distribution Postulates: number
% Features:
% sg: singular
% pl: plural
% n: number
% sg, pl: simply distribute over m (=modifier) product
postulate( p(m,zip(sg,A), zip(sg,B)), zip(sg,p(m,A,B)),'sg').
postulate( p(m,zip(pl,A), zip(pl,B)), zip(pl,p(m,A,B)),'pl').

% Distribution Postulates: person
% Features:
% fi: first
% se: second
% th: third
% pers: person
% fi, se, th: simply distribute over m (=modifier) product
postulate( p(m,zip(fi,A), zip(fi,B)), zip(fi,p(m,A,B)),'fi').
postulate( p(m,zip(se,A), zip(se,B)), zip(se,p(m,A,B)),'se').
postulate( p(m,zip(th,A), zip(th,B)), zip(th,p(m,A,B)),'th').

% Inclusion Postulates: case
% nominative, accusative, dative, genitive -> case
postulate( zip(c,A), zip(no,A),'nc').
postulate( zip(c,A), zip(ac,A),'ac').
postulate( zip(c,A), zip(da,A),'dc').
postulate( zip(c,A), zip(ge,A),'gc').

% Inclusion Postulates: gender
% masculine, feminine, neuter -> case
postulate( zip(g,A), zip(ma,A),'mg').
postulate( zip(g,A), zip(fe,A),'fg').
postulate( zip(g,A), zip(ne,A),'ng').

% Inclusion Postulates: declension
% strong, weak, mixed -> declension
postulate( zip(d,A), zip(st,A),'sd').
postulate( zip(d,A), zip(we,A),'wd').
postulate( zip(d,A), zip(mi,A),'md').

% Inclusion Postulates: number
% singular, plural -> number
postulate( zip(n,A), zip(sg,A),'sn').
postulate( zip(n,A), zip(pl,A),'pn').

% Inclusion Postulates: person
% first, second, third -> person
postulate( zip(pers,A), zip(fi,A),'fp').
postulate( zip(pers,A), zip(se,A),'sp').
postulate( zip(pers,A), zip(th,A),'tp').


% = non-internal modes

external(_).
external_dia(_).

% = transparency

transparent(m).
transparent_dia(_).

% = continuity

continuous(m).
continuous_dia(_).

% ============================================================
% Macros
% ============================================================


% = macro(Form,Replacement)

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Syn,Sem).

% Feature are ordered thus: [case][gender][number][person][declension]

% der: genitive, femininie, singular, third, weak
lex(der,box(ge,
        box(fe,
        box(sg,
        box(th,
        box(we,dr(m,np,n)))))),lambda(P,quant(iota,X,appl(P,X)))).
% meinem: dative, masculine, singular, third, mixed
lex(meinem,box(da,
           box(ma,
           box(sg,
           box(thi,
           box(mi,dr(m,np,n)))))),lambda(P,quant(iota,X,appl(P,X)))).
% zwei: unspecified case, unspecified gender, plural, unspecified person, strong
lex(zwei,box(c,
         box(g,
         box(pl,
         box(pers,
         box(str,dr(m,np,n)))))),lambda(P,quant(iota,X,appl(P,X)))).
% frau: unspecified case, feminine, singular, third, unspecified declension
lex(frau,box(c,
         box(fe,
         box(sg,
         box(th,
         box(d,n))))),frau).
% Mann: nominative, masculine singular, third, unspecified declension
lex(mann,box(no,
         box(ma,
         box(sg,
         box(th,
         box(d,n))))),mann).
% guter: nominative, masculine, singular third, strong
lex(guter,box(no,
          box(ma,
          box(sg,
          box(th,
          box(st,dr(m,n,n)))))),lambda(P,appl(guter,P))).
% guten: genitive, unspecificied gender, unspecified number,
%        unspecified person, weak
lex(guten,box(ge,
          box(g,
          box(n,
          box(pers,
          box(we,dr(m,n,n)))))),lambda(P,appl(guten,P))).

% ============================================================
% Examples
% ============================================================

example("der guten frau",box(ge,box(fe,box(sg,box(th,box(we,np)))))).
example("guter mann",box(no,box(ma,box(sg,box(th,box(st,n)))))).
example("frau",box(ge,box(fe,box(sg,box(th,box(we,n)))))).

% ============================================================
