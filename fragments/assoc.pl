% ============================================================
% File header
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

% This fragment explores some consequences of adding Associativity
% postulates to the base logic. To understand the consequences of
% Associativity, first derive the four instances of the Geach laws.
% (You can use tx/2 if you don't want to bother about lexical
% items: tx(List_of_Formulas,Goal_Formula). ) Then quote out
% one half of the Associativity laws, and see which of the Geach
% laws depend on which of the Associativity postulates.
%
% The sample sentences show that it is sometimes pleasant to
% reason with Associativity, but sometimes not: the `locality'
% requirement on the reflexive pronoun cannot be stated without
% sensitivity to `constituent structure'. Find a systematic
% way of modally decorating your lexical entries, so that
% you recover the required discrimination, without sacrificing
% the s(2) example. (Hint: Systematic would mean you could implement
% the modalization strategy via a macro definition.)

% ============================================================
% Postulates
% ============================================================

% = structural postulates

% Associativity

% Quoted out for the extraction analysis below:

postulate(p(a,p(a,A,B),C),p(a,A,p(a,B,C)),'Ass'). % ass1
postulate(p(a,A,p(a,B,C)),p(a,p(a,A,B),C),'Ass'). % ass2

% extraction (uparrow):

postulate(p(a,B,zip(p,A)),p(a,zip(p,A),B),'Cp').
postulate(p(a,p(a,zip(p,A),B),C),p(a,zip(p,A),p(a,B,C)),'MAp').
postulate(p(a,B,p(a,zip(p,A),C)),p(a,zip(p,A),p(a,B,C)),'MCp').


% = non-internal modes

external(_).
external_dia(_).

% = lazy evaluation

lazy_dr(a).

% = transparency

transparent(a).
transparent(0).
transparent_dia(p).

% = continuity

continuous(0).
continuous_dia(_).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

%macro(s,box(a,c)).		% turning 's' into an island
macro(iv,dl(a,np,s)).
macro(tv,dr(a,iv,np)).
macro(prep,dr(a,pp,np)).
macro(gq_subj,dr(a,s,iv)).
macro(det_subj,dr(a,gq_subj,n)).
macro(gq_obj,dl(a,dr(a,s,np),s)).
macro(det_obj,dr(a,gq_obj,n)).
macro(refl,dl(a,tv,iv)).
macro(relpro,dr(a,rel,relbody) ).
%macro(relbody,dl(a,dia(p,box(p,np)),s) ).	% extraction
macro(relbody,dr(a,s,np) ).
macro(rel,dl(a,n,n) ).
macro(conj(X),dr(a,dl(a,X,X),X)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Syn,Sem).

lex(john,np,j).
lex(mary,np,m).
%lex(thinks,dr(a,iv,c),think).
lex(thinks,dr(a,iv,s),think).
lex(hates,tv,hate).
lex(loves,tv,love).
lex(read,tv,read).
lex(needs,dr(a,iv,gq_obj),need).
lex(talks,dr(a,iv,pp),talk).
lex(confronts,dr(a,dr(a,iv,pp),np),confront).
lex(about,prep,about).
lex(with,prep,with).
lex(boy,n,boy).
lex(book,n,book).
lex(girl,n,girl).
lex(yesterday,dl(a,iv,iv),lambda(X,lambda(Y,appl(yesterday,appl(X,Y)))) ).

lex(whom,relpro,lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z))))) ).
lex(that,relpro,lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z))))) ).


lex(himself,refl,lambda(R,lambda(X,appl(appl(R,X),X)))).
lex(herself,refl,lambda(R,lambda(X,appl(appl(R,X),X)))).

lex(the,dr(0,np,n),lambda(P,quant(iota,X,appl(P,X))) ).

lex(a,det_subj,
	lambda(X,lambda(Y,quant(exists,Z,bool(appl(X,Z),&,appl(Y,Z)))))
	).
lex(a,det_obj,
	lambda(X,lambda(Y,quant(exists,Z,bool(appl(X,Z),&,appl(Y,Z)))))
	).
lex(every,det_subj,
	lambda(X,lambda(Y,quant(forall,Z,bool(appl(X,Z),->,appl(Y,Z)))))
	).
lex(every,det_obj,
	lambda(X,lambda(Y,quant(forall,Z,bool(appl(X,Z),->,appl(Y,Z)))))
	).

lex(everyone,gq_subj,lambda(P,quant(forall,X,appl(P,X))) ).
lex(everyone,gq_obj,lambda(P,quant(forall,X,appl(P,X))) ).
lex(somebody,gq_subj,lambda(P,quant(exists,X,appl(P,X))) ).
lex(somebody,gq_obj,lambda(P,quant(exists,X,appl(P,X))) ).

lex(and,conj(s),lambda(P,lambda(Q,bool(P,&,Q))) ).
lex(and,conj(iv), lambda(P,lambda(Q,lambda(X,bool(appl(P,X),&,appl(Q,X))))) ).
lex(and,conj(dr(a,s,np)),lambda(P,lambda(Q,lambda(X,bool(appl(P,X),&,appl(Q,X))))) ).

% ============================================================
% Examples
% ============================================================

example("John loves himself.",s).
example("John talks about himself.",s).
example("John thinks Mary loves himself.",s).
example("Everyone loves somebody.",s).
example("Everyone hates himself.",s).
example("John loves and Mary hates somebody.",s).
example("The girl whom John talks about.",np).
example("The girl whom John confronts with Mary.",np).
example("The book that John read.",np).

% ============================================================



