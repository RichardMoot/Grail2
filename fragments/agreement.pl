% ============================================================
% agreement.pl
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

% ============================================================
% Postulates
% ============================================================

% = structural postulates

postulate(p(m,zip(d,A),zip(d,B)), zip(d,p(m,A,B)), d).
postulate(p(m,zip(hb,A),zip(hb,B)), zip(hb,p(m,A,B)), hb).
postulate(p(m,zip(ho,A),zip(ho,B)), zip(o,p(m,A,B)), ho).
postulate(p(m,zip(do,A),zip(do,B)), zip(o,p(m,A,B)), do).
postulate(zip(nho,A), zip(d,A), 'I1').
postulate(zip(nho,A), zip(hb,A), 'I2').
postulate(zip(d,A), zip(do,A), 'I3').
postulate(zip(h,A), zip(ho,A), 'I4').
postulate(zip(h,A), zip(hb,A), 'I5').
postulate(zip(d,A), zip(o,A), 'I6').
postulate(zip(ho,A), zip(o,A), 'IC7').

% = lazy evaluation

% = transparency

transparent(m).
transparent(s).

transparent_dia(d).
transparent_dia(do).
transparent_dia(h).
transparent_dia(hb).
transparent_dia(ho).
transparent_dia(nho).
transparent_dia(o).

% = continuity

continuous(m).
continuous(s).

continuous_dia(_).

% = non internal modes

external(_).

external_dia(_).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(een, dr(s,np,box(o,n)), lambda(A,quant(exists,B,appl(A,B)))).
lex(de, dr(s,np,box(d,n)), lambda(A,quant(iota,B,appl(A,B)))).
lex(het, dr(s,np,box(hb,n)), lambda(A,quant(iota,B,appl(A,B)))).
lex(meisje, box(h,n), meisje).
lex(jongen, box(d,n), jongen).
lex(aardig, box(ho,dr(m,n,n)), lambda(A,appl(aardig,A))).
lex(aardige, box(nho,dr(m,n,n)), lambda(A,appl(aardige,A))).

% ============================================================
% Examples
% ============================================================

% = example(Words,Formula)

example(" de jongen", np).
example(" het meisje", np).
example("*het jongen", np).
example("*de meisje", np).
example(" het aardige meisje", np).
example("*het aardig meisje", np).
example("*de aardig meisje", np).
example("*de aardige meisje", np).
example("*het aardige jongen", np).
example("*het aardig jongen", np).
example("*de aardig jongen", np).
example(" de aardige jongen", np).
example(" een jongen", np).
example(" een meisje", np).
example("*een aardige meisje", np).
example(" een aardig meisje", np).
example(" een aardige jongen", np).
example("*een aardig jongen", np).
