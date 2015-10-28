% ============================================================
% drt.pl
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

postulate(zip(t,A), p(1,A,x-t), t).
postulate(p(1,A,x-t), zip(t,A), t).
postulate(p(0,A,p(1,B,C)), p(1,B,zip(r,p(0,A,C))), 'MC').
postulate(p(0,p(1,A,B),C), p(1,A,zip(l,p(0,B,C))), 'MA').
postulate(p(1,A,zip(r,p(0,B,C))), p(0,B,p(1,A,C)), 'MC').
postulate(p(1,A,zip(l,p(0,B,C))), p(0,p(1,A,B),C), 'MA').

% = lazy evaluation

% = transparency

transparent(0).
transparent(1).
transparent(2).

transparent_dia(i).
transparent_dia(l).
transparent_dia(p).
transparent_dia(r).
transparent_dia(t).

% = continuity

continuous(a).
continuous(n).

continuous_dia(j).

% = non internal modes

external(_).

external_dia(_).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(q(A,B,C), dia(t,dr(1,C,dl(1,box(t,A),B)))).
macro(bang(A,B), dia(A,box(A,B))).
macro(det, dr(0,gq,n)).
macro(iv, dl(0,np,s)).
macro(tv, dr(0,iv,np)).
macro(dtv, dr(0,tv,np)).
macro(rel, box(i,dl(0,n,n))).
macro(relbody, dl(0,bang(p,np),s)).
macro(relpro, dr(0,rel,relbody)).
macro(relpro(A), q(np,A,dr(0,rel,dl(0,bang(p,A),s)))).
macro(relpropp, q(np,np,relpro)).
macro(gq, q(np,s,s)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(a(A), det, lambda(B,lambda(C,merge(drs([A],[]),merge(appl(B,A),appl(C,pair(A,drs([],[])))))))).
lex(no(A), det, lambda(B,lambda(C,drs([],[not(merge(merge(drs([A],[]),appl(B,A)),appl(C,pair(A,drs([],[])))))])))).
lex(the(A,B), det, lambda(C,lambda(D,merge(merge(drs([A],[bool(A,=,B)]),appl(C,A)),appl(D,pair(A,drs([],[]))))))).
lex(every(A), det, lambda(B,lambda(C,drs([],[bool(merge(drs([A],[]),appl(B,A)),->,appl(C,pair(A,drs([],[]))))])))).
lex(another(A,B), det, lambda(C,lambda(D,merge(merge(drs([B],[bool(B,neq,A)]),appl(C,B)),appl(D,pair(B,drs([],[]))))))).
lex(bill(A), np, pair(A,drs([A],[bool(A,=,bill)]))).
lex(mary(A), np, pair(A,drs([A],[bool(A,=,mary)]))).
lex(john(A), np, pair(A,drs([A],[bool(A,=,john)]))).
lex(fred(A), np, pair(A,drs([A],[bool(A,=,fred)]))).
lex(suzy(A), np, pair(A,drs([A],[bool(A,=,suzy)]))).
lex(tlg(A), np, pair(A,drs([A],[bool(A,=,tlg)]))).
lex(he(A), np, pair(A,drs([],[]))).
lex(she(A), np, pair(A,drs([],[]))).
lex(him(A), np, pair(A,drs([],[]))).
lex(her(A), np, pair(A,drs([],[]))).
lex(it(A), np, pair(A,drs([],[]))).
lex(man, n, lambda(A,drs([],[appl(man,A)]))).
lex(woman, n, lambda(A,drs([],[appl(woman,A)]))).
lex(friend, dr(0,n,pp), lambda(A,lambda(B,merge(drs([],[appl(friend,B)]),A)))).
lex(farmer, n, lambda(A,drs([],[appl(farmer,A)]))).
lex(student, n, lambda(A,drs([],[appl(student,A)]))).
lex(mathematician, n, lambda(A,drs([],[appl(mathematician,A)]))).
lex(donkey, n, lambda(A,drs([],[appl(donkey,A)]))).
lex(girl, n, lambda(A,drs([],[appl(girl,A)]))).
lex(boy, n, lambda(A,drs([],[appl(boy,A)]))).
lex(book, n, lambda(A,drs([],[appl(book,A)]))).
lex(drinks, iv, lambda(A,merge(drs([],[appl(drink,fst(A))]),snd(A)))).
lex(walks, iv, lambda(A,merge(drs([],[appl(walk,fst(A))]),snd(A)))).
lex(smiles, iv, lambda(A,merge(drs([],[appl(smile,fst(A))]),snd(A)))).
lex(left, iv, lambda(A,merge(drs([],[appl(leave,fst(A))]),snd(A)))).
lex(adores, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(adore,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(loves, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(love,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(respects, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(respect,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(ignores, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(ignore,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(hates, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(hate,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(hate, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(hate,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(beats, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(beat,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(beat, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(beat,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(owns, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(own,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(wrote, tv, lambda(A,lambda(B,merge(merge(drs([],[appl(appl(write,fst(A)),fst(B))]),snd(A)),snd(B))))).
lex(thinks, dr(0,iv,s), lambda(A,lambda(B,merge(drs([],[appl(appl(think,A),fst(B))]),snd(B))))).
lex(believes, dr(0,iv,s), lambda(A,lambda(B,merge(drs([],[appl(appl(believe,A),fst(B))]),snd(B))))).
lex(talks, dr(0,dr(0,iv,np),dr(0,pp,np)), lambda(_,lambda(A,lambda(B,merge(merge(drs([],[appl(appl(talk,fst(A)),fst(B))]),snd(A)),snd(B)))))).
lex(gives, dtv, lambda(A,lambda(B,lambda(C,merge(merge(merge(drs([],[appl(appl(appl(give,fst(B)),fst(A)),fst(C))]),snd(A)),snd(B)),snd(C)))))).
lex(if, dr(2,dr(2,s,s),s), lambda(A,lambda(B,drs([],[bool(A,->,B)])))).
lex(and, dr(2,dl(2,s,s),s), lambda(A,lambda(B,merge(A,B)))).
lex(or, dr(2,dl(2,s,s),s), lambda(A,lambda(B,drs([],[bool(A,\/,B)])))).
lex(who, relpro, lambda(A,lambda(B,lambda(C,merge(appl(A,pair(C,drs([],[]))),appl(B,C)))))).
lex(whom, q(np,dr(0,s,dl(0,np,s)),dr(0,rel,dl(0,bang(p,np),s))), lambda(A,lambda(B,lambda(C,lambda(D,merge(appl(C,D),appl(appl(A,pair(D,drs([],[]))),B))))))).
lex(doesnt, dr(0,iv,iv), lambda(A,lambda(B,drs([],[not(appl(A,B))])))).
lex(to, dr(0,pp,np), lambda(A,merge(drs([],[appl(to,fst(A))]),snd(A)))).
lex(of, dr(0,pp,np), lambda(A,merge(drs([],[appl(of,fst(A))]),snd(A)))).
lex(himself, q(np,iv,iv), lambda(A,lambda(B,appl(appl(A,B),B)))).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example([66,105,108,108,40,98,41,32,116,97,108,107,115,32,116,111,32,104,105,109,115,101,108,102,46], s).
example([69,118,101,114,121,40,120,41,32,109,97,110,32,108,111,118,101,115,32,97,40,121,41,32,119,111,109,97,110,46], s).
example([78,111,40,120,41,32,109,97,110,32,119,104,111,32,97,100,111,114,101,115,32,77,97,114,121,40,121,41,32,100,114,105,110,107,115,46], s).
example([73,102,32,97,40,120,41,32,102,97,114,109,101,114,32,111,119,110,115,32,97,40,121,41,32,100,111,110,107,101,121,44,32,104,101,40,120,41,32,98,101,97,116,115,32,105,116,40,121,41,46], s).
example([69,118,101,114,121,40,120,41,32,102,97,114,109,101,114,32,119,104,111,32,111,119,110,115,32,97,40,121,41,32,100,111,110,107,101,121,32,98,101,97,116,115,32,105,116,40,121,41,46], s).
example([70,114,101,100,40,102,41,32,116,104,105,110,107,115,32,101,118,101,114,121,40,120,41,32,115,116,117,100,101,110,116,32,98,101,108,105,101,118,101,115,32,97,40,121,41,32,109,97,116,104,101,109,97,116,105,99,105,97,110,32,119,114,111,116,101,32,116,108,103,40,116,41,46], s).
