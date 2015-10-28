% ============================================================
% cf.pl
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

% =================================================================
% Multimodal analysis of choice function,  (c) 1997 Fabien Reniers 
% =================================================================
% Definition of choice functions:
%
% f is choice function (i.e. CH(f) holds) iff:
% All P [not (P = empty1) -> Exists x P(x) & f(P)=^Q.Q(x)] and
%  f(empty1) = empty2
%
% Where empty1 is the empty set of type <e,t>
%       empty2 is the empty set of type <<e,t>,t>, i.e. the empty generalized
%                                                       quantifier
%       P,Q of type <e,t>
%       x of type e  
% 
% Note that we write ^ voor lambda.  
% -----------------------------------------------------------------------
% Indefinites are treated as predicates, to which a choice function applies.
% The choice function returns a (collective) generalized quantifier, i.e. 
% a principal ultrafilter of plural individuals:
%
% (1a) John kissed three women.
% (1b) f(three(woman))(^x. kiss(j,x))
% (1c) Ef CH(f) & f(three(woman))(^x. kiss(j,x))
% 
% Sentence (1a) is translated into (1b), note that three is a predicate 
% modifier. Unselective binding at each compositional sentence level can apply
% to choice functions. In this case there is only one option: closure at the
% top, as is done in (1c).
%
% But compare:
% (2a) John thinks a woman is laughing.
% Here we have two sentence levels (i.e. de re and de dicto):
% (2b) Ef CH(f) & f(a(woman)) (^x.think(j,laugh(x)))
% (2c) think(j,Ef CH(f) & f(a(woman))(^x.laugh(x)))
%
% (The example is not chosen very well with respect to intensionality, because
% the choice function definition above does not comprise intensionality.)
% 
% A multimodal analysis of choice functions integrates the process of 
% unselective binding into the lexical semantics of choice functions.
% This is advantageous: a problem for the choice function mechanisms of 
% Reinhart and Winter is the following observation.
%
% The embedding observation:
% Given two choice functions f and g, 
% The existential closure of f cannot be wider than the
% closure of g if g is embedded in the restriction to which f applies
% (Unless the closure of g is inside the restriction)
%
% This observation is a binding principle. Compare:
% (3) It is not the case that every man(i) kissed [a woman HE(i) knows]
%
% Since the pronoun is bound by the quantifier [every man], the scope of
% the complex indefinite [a woman he knows] cannot be wider than this
% quantifier (i.e. the place where the pronoun is resolved), for this
% would make the variable unbounded)
%
% Generating (4) as an interpretation for (3) yields the wrong results:
%
% (4) Ef CH(f) & not([every man](^x. f(a woman x knows)(^y.kiss(x,y))))
%
% Let M be a model such that:
% John knows Mary. Peter knows Sue and Irene. 
% John kissed Mary. Peter kissed Sue, but not Irene.
%
% This model makes (3) false, for "every man(i) kissed a woman he(i) knows" is
% obviously true in M. However, M makes (4) true:
%
% Let f(woman John knows)  = ^Q.Q(Mary)
%     f(woman Peter knows) = ^Q.Q(Irene) 
%
% The same line of argumentation holds for sentences like (5).
%
% (5) It is not the case that every student read [a book of some professor]
% 
% Sentence (5) contains an indefinite that is embedded in an indefinite.
% If the closure of 'professor' is in the scope of 'every student', then
% the closure of the complex indefinite cannot be wider.
%
% Integrating unselective binding in the lexical semantics of choice functions
% does not suffer from the problem above:
%
% cf = q(f,GQ,s,s)/n    where q(f,A,B,C) is an island-free q-connective
% GQ = q(g,np,s,s)      where q(g,A,B,C) is an island-restricted q-connective.
%
% from the definition above it follows that a choice function picks up the
% semantic restriction and returns a generalized quantifier that has its
% semantics (i.e. closure) at the sentence level.
%
% Sentence (5) must start with the most embedded choice function. The 
% q-connective of this function restricts the binding domain of the complex
% indefinite. Hence, the embedding observation is derived.
%
% The lexical semantics of cf 
% (just a compositional way to implement existential closure):
%
% cf = ^P.^Z.Ef CH(f) & Z(f(P))      Z of type <<<e,t>,t>,t>, P of type <e,t>
%
% from the definition above it follows that the semantics of the generalized
% quantifier that is returned in situ is f(P). 


% = lazy evaluation

% = transparency

transparent_dia(l).
transparent_dia(r).
transparent_dia(t).

transparent(f).
transparent(g).
transparent(isl).
transparent(n).

% = continuity

continuous_dia(l).
continuous_dia(r).
continuous_dia(t).

% = non internal modes

external(_).
external_dia(_).

% ============================================================
% Postulates
% ============================================================

% = structural postulates

% Mode n = non-associative 'default' mode
% Mode f = island free wrapping mode, for existential closure of choice 
% functions
% Mode g = island restricted wrapping mode, for generalized quantifiers
% Mode isl = island mode

/*  old:
perm_mode(f).
perm_mode(g).

trace_mode(f).
trace_mode(g).

hop_mode(f).

% trace elimination and introduction resp.
% X oM t <--> <t> X  
postulate(zip(t,X),p(M,X,x-t),'t') :- trace_mode(M). 
postulate(p(M,X,x-t),zip(t,X),'t') :- trace_mode(M).

% Undo permutatation: to the right
% A x (B o C) <-- B o <r>(A x C)
postulate(p(n,A,p(M,B,C)),p(M,B,zip(r,p(n,A,C))),'MC') :- perm_mode(M).

% Undo association: to the left
% (A o B) x C <-- A o <l>(B x C) 
postulate(p(n,p(M,A,B),C),p(M,A,zip(l,p(n,B,C))),'MA') :- perm_mode(M).

% Permutate to the left (bookmark right) 
% A x (B o C) --> B o <r>(A x C)
postulate(p(M,B,zip(r,p(n,A,C))),p(n,A,p(M,B,C)),'MC') :- perm_mode(M).

% Right Association (bookmark left)
% (A o B) x C --> A o <l>(B x C) 
postulate(p(M,A,zip(l,p(n,B,C))),p(n,p(M,A,B),C),'MA') :- perm_mode(M).

% Lift resources in the free wrapping mode (hop-mode) across islands:

% island hopping A x(isl) (B xM C) <-> B xM (A x(isl) C)
postulate(p(M,B,p(isl,A,C)),p(isl,A,p(M,B,C)),'HL') :- hop_mode(M).
postulate(p(isl,A,p(M,B,C)),p(M,B,p(isl,A,C)),'HR') :- hop_mode(M).
*/

postulate(zip(t,X),p(f,X,x-t),'t').
postulate(p(f,X,x-t),zip(t,X),'t').
postulate(zip(t,X),p(g,X,x-t),'t').
postulate(p(g,X,x-t),zip(t,X),'t').
postulate(p(n,A,p(f,B,C)),p(f,B,zip(r,p(n,A,C))),'MC').
postulate(p(n,A,p(g,B,C)),p(g,B,zip(r,p(n,A,C))),'MC').
postulate(p(n,p(f,A,B),C),p(f,A,zip(l,p(n,B,C))),'MA').
postulate(p(n,p(g,A,B),C),p(g,A,zip(l,p(n,B,C))),'MA').
postulate(p(f,B,zip(r,p(n,A,C))),p(n,A,p(f,B,C)),'MC').
postulate(p(g,B,zip(r,p(n,A,C))),p(n,A,p(g,B,C)),'MC').
postulate(p(g,A,zip(l,p(n,B,C))),p(n,p(g,A,B),C),'MA').
postulate(p(f,A,zip(l,p(n,B,C))),p(n,p(f,A,B),C),'MA').
postulate(p(f,B,p(isl,A,C)),p(isl,A,p(f,B,C)),'islL'). 
postulate(p(isl,A,p(f,B,C)),p(f,B,p(isl,A,C)),'islR').

% ============================================================
% Macros
% ============================================================

macro(iv,dl(n,np,s)).
macro(tv,dr(n,iv,np)).
macro(bang(M,X),dia(M,box(M,X))).

macro(q(M,A,B,C),dia(t,dr(M,C,dl(M,box(t,A),B)))).   % <t>(C /M ([t]A \M B))
macro(gq,q(g,np,s,s)).
macro(det,dr(n,q(g,np,s,s),n)).

% choice function: q(f,GQ,s,s)/n
macro(cf,dr(n,q(f,gq,s,s),n)).  

macro(nmod,dr(n,n,n)).
macro(pron,q(f,np,iv,iv)).

% ============================================================
% Lexicon
% ============================================================
% Choice functions!  ^P.^Z. Ef ch(f) & Z(f(P))

lex(f, cf, lambda(P,lambda(Z, quant(exists,F, bool(appl(ch,F),&,appl(Z,appl(F,P))))))).

lex(john,np,j).
lex(peter,np,p).

lex(men,n,men).
lex(man,n,man).
lex(friend,n,friend).

lex(walked,iv,walk).
lex(laughed,iv,laugh).
lex(killed,tv,kill).

% one verb with island, and one without: solely to restrict QR-movement 
lex(thinks,dr(isl,iv,s),lambda(S,lambda(X,appl(appl(think,S),X)))).
lex(says,dr(n,iv,s),lambda(S,lambda(X,appl(appl(say,S),X)))).

lex(a,nmod,lambda(X,appl(a,X))).
lex(three,nmod,lambda(X,appl(three,X))).

lex(all,det,lambda(P,lambda(Q,quant(forall,X,bool(appl(P,X),->,appl(Q,X)))))).
lex(everyone,q(g,np,s,s),lambda(P,quant(forall,X,appl(P,X)))).

lex(his,pron,lambda(P,lambda(X,appl(appl(P,X),X)))).
lex(he,pron,lambda(P,lambda(X,appl(appl(P,X),X)))).

% island in antecedent
lex(if,dr(isl,dr(n,s,s),s),lambda(P,lambda(Q,bool(P,->,Q)))).

lex(of,dr(n,dl(n,n,n),np),lambda(X,lambda(P,lambda(Y,bool(appl(P,Y),&,appl(appl(of,Y),X))) ))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DISTR
% Distributivity is island restricted, distr applies to a collective GQ
% and yields a distributive GQ:
%
% distr = gq/gq 
% distr-operator <ett,ett> = 
% ^G./distr1(x) if there is a unique individual x s.t. G=^P.P(x)
%    \G otherwise
% 
% distr1 = ^x.^P. Forall y #(y)(x) -> P(y)
% where #(y)(x) means y is a singular individual of plural indiv. x
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lex(distr,dr(n,gq,gq),distr).

% ============================================================
% Examples
% ============================================================

example(" John walked.",s).
example(" Friend of John.",n).
example(" John thinks Peter walked.",s).
example(" f a man walked.",s).  
example(" John killed f a man.",s). 

% if projects an island 
% unrestricted unselective binding, restricted GQ-scope:
example(" If John killed f a man, Peter laughed.",s).
example(" If john killed everyone, Peter laughed.",s). 
example(" John thinks he walked.",s).

% I deleted 'a' to decrease the number of derivations
% think projects an island, say doesn't
example(" John thinks f friend of his walked.",s).
example(" John says f friend of his walked.",s).

% note that f's closure is in the scope of everyone
% note also that say is island-free
example(" Everyone says f Friend of his walked.",s).

example(" John killed f friend of everyone.",s).  
