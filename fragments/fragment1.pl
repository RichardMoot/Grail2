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

% ============================================================
% Postulates
% ============================================================

% = structural postulates

% Associativity

postulate(p(0,p(0,A,B),C),p(0,A,p(0,B,C)),'P1'). % ass1
postulate(p(0,A,p(0,B,C)),p(0,p(0,A,B),C),'P2'). % ass2
%postulate(p(0,A,p(0,B,zip(1,C))),p(0,p(0,A,B),zip(1,C)),'P2+').
	
% extraction (uparrow):

postulate1(p(0,p(0,zip(0,A),B),C),p(0,zip(0,A),p(0,B,C)),'P3').
postulate1(p(0,B,p(0,zip(0,A),C)),p(0,zip(0,A),p(0,B,C)),'P4').
postulate1(p(0,B,zip(0,A)),p(0,zip(0,A),B),'P5').


% = non-internal modes

external(_).
external_dia(_).

% = transparency

% = continuity

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

%macro(s,box(0,c)).		% turning 's' into an island
macro(bang(M,F),dia(M,box(M,F)) ).
macro(iv,dl(0,np,s)).
macro(tv,dr(0,iv,np)).
macro(prep,dr(0,pp,np)).
macro(gq_subj,dr(0,s,iv)).
macro(det_subj,dr(0,gq_subj,n)).
macro(gq_obj,dl(0,dr(0,s,np),s)).
%macro(gq_obj,dl(0,dr(0,s,bang(1,np)),s)).
macro(det_obj,dr(0,gq_obj,n)).
macro(refl,dl(0,tv,iv)).
%macro(refl,dl(0,dr(0,iv,bang(1,np)),iv)).
macro(relpro,dr(0,rel,relbody) ).
macro(relbody,dl(0,dia(0,box(0,np)),s) ).	% extraction
%macro(relbody,dr(0,s,dia(2,box(2,np))) ).
macro(rel,dl(0,n,n) ).
macro(conj(X),dr(0,dl(0,X,X),X)).
macro(rncconj(X,Y),dr(0,dl(0,dr(0,X,bang(2,Y)),dr(0,X,Y)),dr(0,X,bang(2,Y)) ) ).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Syn,Sem).

lex(john,np,j).
lex(mary,np,m).
lex(noam,np,noam).
lex(jim,np,jim).
lex(gottlob,np,gottlob).

%lex(thinks,dr(0,iv,c),think).
lex(thinks,dr(0,iv,s),think).
lex(hates,tv,hate).
lex(loves,tv,love).
lex(read,tv,read).
lex(needs,dr(0,iv,gq_obj),need).
lex(talks,dr(0,iv,pp),talk).
lex(confronts,dr(0,dr(0,iv,pp),np),confront).
lex(about,prep,about).
lex(with,prep,with).
lex(boy,n,boy).
lex(book,n,book).
lex(girl,n,girl).
lex(mathematician,n,mathematician).
lex(yesterday,dl(0,iv,iv),
	lambda(X,lambda(Y,appl(yesterday,appl(X,Y)))) ).

lex(who,dr(0,dl(0,n,n),dl(0,np,s)),
	lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z))))) ).
lex(whom,dr(0,dl(0,n,n),dr(0,s,np)),
	lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z))))) ).

%lex(whom,relpro,
%	lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z))))) ).
%lex(that,relpro,
%	lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z))))) ).

lex(that,dr(0,dl(0,n,n),dl(0,np,s)),
	lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z))))) ).
lex(that,dr(0,dl(0,n,n),dr(0,s,np)),
	lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z))))) ).

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
lex(but,conj(dr(0,s,np)), 
	lambda(P,lambda(Q,lambda(X,bool(appl(P,X),&,appl(Q,X))))) ).

%lex(but,rncconj(s,np),
%	lambda(P,lambda(Q,lambda(X,bool(appl(P,X),&,appl(Q,X))))) ).
% ============================================================
% Examples
% ============================================================

example("John loves Mary.",s).
example("John loves himself.",s).
example("John talks about himself.",s).
example("John thinks Mary loves himself.",s).
example("Everyone hates himself.",s).
example("Everyone loves somebody.",s).
example("John loves and Mary hates somebody.",s).
example("the girl whom John talks about",np).
example("the book that John read",np).
%example("the mathematician that Noam hates Gottlob and Jim loves",np).
