# Report for Computational Logic for Artificial Intelligence Coursework
Open this notebook for a demo of this codebase:
[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/lucyfarnik/ComputationalLogic/blob/prolexa-plus/Demo.ipynb)

For a description of Prolexa, see the [original repository](https://github.com/simply-logical/ComputationalLogic).

This repository contains the code extending Prolexa to handle negation and existential quantification.
The `multi-meaning` branch is a work in progress -- it aims to extend Prolexa further
by adding complex conjunctions and disjuctions.

## Negation
Prolexa can now handle reasoning such as
> donald is not happy; every teacher is happy; therefore donald is not a teacher

This is implemented using the new `not()` predicate, for instance "donald is not happy" is stored as `(not(happy(donald)):-true)`.

The following clauses were added to the grammar to make this work:
```prolog
pred(teacher, 1,[n/teacher]).
pred(happy,   1,[a/happy]).
proper_noun(s,donald) --> [donald].

sentence1(C) --> determiner(N,M1,X=>not(H),C),noun(N,M1),verb_phrase(N,not(X=>H)).
sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).

verb_phrase(s,not(M)) --> [is,not],property(s,M).
verb_phrase(p,not(M)) --> [are,not],property(p,M).
verb_phrase(s,not(M)) --> [does,not],iverb(p,M).
verb_phrase(p,not(M)) --> [do,not],iverb(p,M).

question1(not(Q)) --> [is], proper_noun(N,X),[not],property(N,X=>Q).
question1(not(Q)) --> [does],proper_noun(_,X),[not],verb_phrase(_,X=>Q).
```

This was complemented by adding modus tollens to `prolexa_engine`:
```prolog
prove_rb(not(A),Rulebase,P0,P) :- % modus tollens
	find_clause((B:-A),Rule,Rulebase),
	prove_rb(not(B),Rulebase,[p(A,Rule)|P0],P).
```
## Existential quantification
Prolexa can do reasoning such as
> every genius wins; some humans are geniuses; therefore some humans win

The implementation of existential quantification is based on Skolemisation,
explained in [Chapter 2.5 of Simply Logical textbook](https://book.simply-logical.space/src/text/1_part_i/2.5.html).
"some humans are geniuses" gets translated to `(human(sk):-true),(genius(sk):-true)`,
where `sk` is a Skolem constant.

This required uncommenting out the following lines in `prolexa_grammar`:
```prolog
determiner(p,X=>B,X=>H,[(H:-B)]) --> [].
determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2 :- true)]) -->[some].

question1((Q1,Q2)) --> [are,some],noun(p,sk=>Q1), property(p,sk=>Q2).
```
as well as adding the following lines:
```prolog
pred(genius,  1,[n/genius]).

% most of this clause is already in the original codebase
noun_s2p(Noun_s,Noun_p):-
	( Noun_s=woman -> Noun_p=women
	; Noun_s=man -> Noun_p=men
	; Noun_s=genius -> Noun_p=geniuses % I only added this line
	; atom_concat(Noun_s,s,Noun_p)
	).

question1((Q1,Q2)) --> [do,some],noun(p,sk=>Q1), verb_phrase(p,sk=>Q2).

command(g(explain_question((Q1,Q2),_,Answer),Answer)) --> [explain,why],sentence1([(Q1:-true),(Q2:-true)]).
```

Making this work with `prolexa_engine` required adding support for a sentence
being translated into 2 conjunctive clauses. First, this meant extending
`prove_rb` with the following code, which simply states that to prove a conjunction,
one must prove both its parts, and that the proof of the conjunction simply consists
of the combination of the proofs required for each part:
```prolog
prove_rb((A,B),Rulebase,P0,P):-!,
	prove_rb(A,Rulebase,P0,P1),
	prove_rb(B,Rulebase,P1,P).
```

Next, it was necessary to extend `find_clause` to enable it to match clauses
like `human(sk):-X` with rules like `[(human(sk):-true),(genius(sk):-true)]`:
```prolog
find_clause(Clause,[Rule1,Rule2],[[Rule1,Rule2]|_Rules]):-
	copy_term(Rule1,Clause) ; copy_term(Rule2,Clause).
```

Lastly, I also modified `explain_question` to take into account the fact that
a query can be conjunctive.

```prolog
explain_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase,[],Proof) ->
		maplist(pstep2message,Proof,Msg),
		(
			Query = (Q1,Q2), phrase(sentence1([(Q1:-true),(Q2:-true)]),L)
			; Query \= (_,_), phrase(sentence1([(Query:-true)]),L)
		),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg,[Last],Messages),
		atomic_list_concat(Messages,"; ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).
```

### Edge case: repeating statements in proofs
The modifications of `explain_question` also required dealing with this edge
case: when proving "some humans win", the `prove_rb` predicate will first
prove `human(sk)` using the rule "some humans are geniuses"
(i.e. `[(human(sk):-true),(genius(sk):-true)]`), then prove `win(sk)`
using the rule "all geniuses win" (i.e. `win(X):-genius(X)`) combined with
"some humans are geniuses" (i.e.`[(human(sk):-true),(genius(sk):-true)]`).
Note that "some humans are geniuses" is used twice in the proof, leading to
reasoning such as
> some humans are geniuses; every genius wins; some humans are geniuses; therefore some humans win

While this is technically sound reasoning, having the same statement appear
twice is inelegant, which is why I decided to remove duplicates:
```prolog
explain_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase,[],Proof) ->
		maplist(pstep2message,Proof,Msg),
		remove_duplicates(Msg,MsgNoDups),
		(
			Query = (Q1,Q2), phrase(sentence1([(Q1:-true),(Q2:-true)]),L)
			; Query \= (_,_), phrase(sentence1([(Query:-true)]),L)
		),
		atomic_list_concat([therefore|L]," ",Last),
		append(MsgNoDups,[Last],Messages),
		atomic_list_concat(Messages,"; ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).


remove_duplicates([], []).
remove_duplicates([H|T], List) :- member(H, T), !, remove_duplicates(T, List).
remove_duplicates([H|T1], [H|T2]) :- remove_duplicates(T1, T2).
```

## Work in progress: extending grammar to handle complex conjunctions and disjunctions
I originally intended to extend Prolexa's reasoning much further by completely
re-thinking the way `prolexa_grammar` works. I wanted Prolexa to handle arbitrarily
complex conjunctive and disjunctive statements, for instance:
> everyone who is either the not the messiah or a very naughty boy is not blessed and not a cheesemaker.

This could be used, among other things, for disjunctive reasoning such as this:
> every blessed person is either a messiah or a cheesemaker;
> brian is not a messiah; brian is blessed; therefore, brian is a cheesemaker

In order to do this, I decided to completely refactor much of the code base,
but got stuck on one particular predicate and decided to abandon these efforts.

![](https://pbs.twimg.com/media/CpIlMEdVUAQlhIW?format=jpg&name=medium)

Nontheless, most of the refactoring is complete and I want to explain my
approach to it in this section. You can find the code on the
[`multi-meaning` branch](https://github.com/lucyfarnik/ComputationalLogic/tree/multi-meaning).

### High-level approach
The core idea here is to refactor the code base such that each sentence
consists of 2 parts, a noun phrase and a verb phrase, each of which can
have multiple "meanings". For example "everyone who is a naughty boy
is not the messiah and is not blessed" has the noun phrase meanings
`[naughty, boy]` and the verb phrase meanings `[not(messiah), not(blessed)]`.
Since the noun phrase is universally quantified, this gets translated into
the clauses `[(not(messiah(X)) :- naughty(X),boy(X)), (not(blessed(X)) :- naughty(X),boy(X))]`.
Note that this means that a single sentence can encode arbitrarily many clauses.

In order to handle disjunction, we use the fact that $A\implies B\lor C$ is
equivalent to $(A\land\lnot C\implies B)\land(A\land\lnot B\implies C)$
(see [Chapter 2.4 of Simply Logical](https://book.simply-logical.space/src/text/1_part_i/2.4.html) for details).
This meant that a sentence like "every blessed person is either a messiah or a cheesemaker"
would have the noun phrase meanings `[blessed, person]` and the verb phrase
meanings `[disjunction(messiah, cheesemaker)]`, which would get translated to
the clauses `[(messiah(X):-blessed(X),person(X),not(cheesemaker(X))), (cheesemaker(X):-blessed(X),person(X),not(messiah(X)))]`.

### Allowing sentences to encode arbitrarily many meanings
First, this required modifying the root-level module `prolexa.pl` to
take into accound sentences whose arguments are lists of arbitrarily many
clauses. This was done by modifying the part of `handle_utterance` that
deals with sentences:
```prolog
% A. Utterance is a sentence 
	( phrase(sentence(Rules),UtteranceList),
	  write_debug(rule(Rules)),
	  (
		all_known_rules(Rules,SessionId) -> % A1. All rules are known
			atomic_list_concat(['I already knew that',Utterance],' ',Answer)
		; otherwise -> %A2. At least one rule is new
			store_new_rules(Rules,SessionId),
			atomic_list_concat(['I will remember that',Utterance],' ',Answer)
	  )
```
where `all_known_rules` and `store_new_rules` are defined as
```prolog
store_new_rules([], _).
store_new_rules([Rule|Rest], SessionId):-
	(known_rule([Rule],SessionId) -> true
	; otherwise -> assertz(prolexa:stored_rule(SessionId,[Rule]))),
	store_new_rules(Rest, SessionId).

all_known_rules([],_).
all_known_rules([Rule|Rest],SessionId) :-
	known_rule([Rule],SessionId), all_known_rules(Rest,SessionId).
```

### Refactoring sentence structure
Sentences are now defined simply as
```prolog
sentence(Clauses) --> 
	top_level_noun_phrase(Number, Quantifier, NounPhraseMeanings),
	verb_phrase(Number, VerbPhraseMeanings),
	{clauses_to_meanings(Clauses, Quantifier,
		NounPhraseMeanings, VerbPhraseMeanings)}.
```

The top-level noun phrase can be either a list of proper nouns, or a determiner
followed by a regular noun phrase
```prolog
% the highest-level noun phrase in the parse tree (the one whose parent is the sentence)
top_level_noun_phrase(Number, no_quantifier, Meanings) -->
	proper_nouns(Number, Meanings).
top_level_noun_phrase(Number, Quantifier, Meanings) -->
	% IsNounPhraseComplex means that we have to list multiple properties
	% eg "everyone who is a human and flies" is complex while "every human" isn't
	determiner(Number, IsNounPhraseComplex, Quantifier),
	noun_phrase(Number, IsNounPhraseComplex, Meanings).

% may be a single plural noun or multiple of them
proper_nouns(singular, [Meaning]) --> proper_noun(Meaning).
proper_nouns(plural, Meanings) --> proper_nouns1(Meanings).

% multiple plural nouns
proper_nouns1([Meaning, Meaning2]) -->
	proper_noun(Meaning), [and], proper_noun(Meaning2).
proper_nouns1([Meaning|Meanings]) -->
	proper_noun(Meaning), [','], proper_nouns1(Meanings).

determiner(singular, simple, universal_quantifier) --> [every].
determiner(plural, simple, universal_quantifier) --> [all].
determiner(singular, complex, universal_quantifier) --> [everyone, who].

% noun_phrase(singular,M) --> [a],noun_phrase(singular,M).
noun_phrase(Number, simple, [Meaning]) --> noun(Number,Meaning).
noun_phrase(Number, simple, [Meaning1, Meaning2]) --> 
	degree, adjective(Meaning1), noun(Number,Meaning2).
% noun_phrase(Number, simple, not(Meaning)) --> ['non-'], noun(Number, Meaning).
noun_phrase(Number, complex, Meanings) --> properties(Number, Meanings).

degree --> []|[very]|[vewy]. % for now, this carries no semantics
```

The verb phrase is simply a list of properties, much like the noun phrase:
```prolog
verb_phrase(Number, Meanings) --> properties(Number, Meanings).
```

Properties can be conjunctively or disjunctively connected (or certain
combinations of the two):
```prolog
properties(Number, Meanings) --> property(Number, Meanings).
properties(Number, Meanings) --> 
	property(Number, Meanings1), [and], property(Number, Meanings2),
	{append(Meanings1, Meanings2, Meanings)}.
properties(Number, [Meaning|Meanings]) --> 
	property(Number, [Meaning]), [','], properties(Number, Meanings).

% `property` is defined in a nested way where each layer processes
% a different part of the sequence
% note that `property` can have 2 meanings because "a mortal human" is
% parsed as 1 property even though it has 2 meanings
% the top-level property predicate processes disjunctions
property(Number, Meanings) --> property1(Number, Meanings).
property(Number, [disjunction(Meaning1, Meaning2)]) -->
	([either] | []), property1(Number, Meaning1), [or], property1(Number, Meaning2).

% property1 processes verbs (incl. the verb "to be")
property1(Number, [Meaning]) --> iverb(Number, Meaning).
property1(Number, [NegatedMeaning]) -->
	verb_negation(Number), iverb(Number, Meaning), {negate(Meaning, NegatedMeaning)}.
property1(singular, Meanings) --> [is], property2(singular, Meanings).
property1(plural, Meanings) --> [are], property2(plural, Meanings).

verb_negation(singular) --> [does,not] | ['doesn\'t'].
verb_negation(plural) --> [do,not] | ['don\'t'].

% property2 processes negation
property2(Number, Meanings) --> property3(Number, Meanings).
property2(Number, NegatedMeanings) -->
	[not], property3(Number, Meanings),
	{negate_all(Meanings, NegatedMeanings)}.

% property3 fully processes properties which do not include a noun;
% for properties which do include a noun, it processes "a" or "the"
property3(_, [Meaning]) --> degree, adjective(Meaning).
property3(singular, Meanings) --> ([a] | [the]), property4(singular, Meanings).
property3(plural, Meanings) --> ([] | [the]), property4(plural, Meanings).

% property4 fully processes properties which include a noun
property4(Number, [Meaning]) --> noun(Number, Meaning).
property4(Number, [Meaning1, Meaning2]) --> 
	degree, adjective(Meaning1), noun(Number, Meaning2).

negate(not(X), X).
negate(X, not(X)).

negate_all([], []).
negate_all([Meaning|Meanings], [NegatedMeaning|NegatedMeanings]) :-
	negate(Meaning, NegatedMeaning),
	negate_all(Meanings, NegatedMeanings).
```

We define parts of speech similarly to how they are defined
in the original code base, except we get rid of the `=>` operator:
```prolog
adjective(Meaning)		--> [Adj],    {meaning_to_word(Meaning,1,a/Adj)}.
% adjective(not(M))	--> [Adj_neg],{meaning_to_word(_P,1,a/Adj_pos, M),adj_pos2neg(Adj_pos,Adj_neg)}.
% adjective(not(M))	--> [Adj_pos],{meaning_to_word(_P,1,a/Adj_neg, M),adj_pos2neg(Adj_pos,Adj_neg)}.
noun(singular, Meaning)	--> [Noun],   {meaning_to_word(Meaning,1,n/Noun)}.
noun(plural, Meaning)	--> [Noun_p], {meaning_to_word(Meaning,1,n/Noun), noun_s2p(Noun,Noun_p)}.
iverb(singular, Meaning)--> [Verb_s], {meaning_to_word(Meaning,1,v/Verb), verb_p2s(Verb,Verb_s)}.
iverb(plural, Meaning)	--> [Verb],   {meaning_to_word(Meaning,1,v/Verb)}.

meaning_to_word(Meaning,1,Class/Word):-
	pred(Meaning, 1, Literals),
	member(Class/Word, Literals).
```

Lastly, the vocabulary I'm using throughout this section is defined as:
```prolog
% unary predicates for adjectives, nouns, and verbs
pred(messiah, 1,[n/messiah]).
pred(naughty, 1,[a/naughty]).
pred(boy,     1,[n/boy]).
pred(blessed, 1,[a/blessed]).
pred(cheesemaker, 1, [n/cheesemaker]).
pred(released, 1,[a/released,a/weleased]).

% proper nouns
proper_noun(brian) --> [brian]|[bwian].
proper_noun(roger) --> [roger]|[woger].
```
(Note the additional accesibility feature of allowing users with speech
sound disorders (namely rhotacism) to still use the software, for instance
"woger is weleased" is correctly interpreted to be the same as "roger is released".)

### Converting between clauses and meanings
The part of this refactoring which I ultimately did not finish is the predicate
`clauses_to_meanings`, which expresses the afforementioned relation between clauses,
noun phrase meanings, and verb phrase meanings.

(Note: I initially tried a different approach to implementing this; this can
still be found in `prolexa_grammar` and is currently commented out.)

```prolog
%%% semantic processing %%%
% converts between Prolog clauses and the meanings of noun phrases and verb phrases
% usage: clauses_to_meanings(Clauses, Quantifier, NounPhraseMeanings, VerbPhraseMeanings)
clauses_to_meanings([], _, [], _). % base case: no more noun phrase meanings
clauses_to_meanings([], _, _, []). % base case: no more verb phrase meanings

clauses_to_meanings([Clause1,Clause2|ClausesRest], no_quantifier, [NounPhraseM],
					[disjunction(VerbPhraseM1,VerbPhraseM2)|VerbPhraseRest]) :-
	 % if we have a disjunction, we need to process it separately
	((nonvar(VerbPhraseM1), nonvar(VerbPhraseM2)) -> !
	; otherwise -> true),
	Clause1 = (ClauseHead1:-NegatedClauseHead2),
	Clause2 = (ClauseHead2:-NegatedClauseHead1),
	negate(ClauseHead1, NegatedClauseHead1),
	negate(ClauseHead2, NegatedClauseHead2),
	univ_with_negation(VerbPhraseM1, NounPhraseM, ClauseHead1),
	univ_with_negation(VerbPhraseM2, NounPhraseM, ClauseHead2),
	clauses_to_meanings(ClausesRest, no_quantifier, [NounPhraseM], VerbPhraseRest).
clauses_to_meanings([Clause|ClausesRest], no_quantifier, [NounPhraseM], [VerbPhraseM|VerbPhraseRest]) :- 
	Clause = (ClauseHead:-true),
	univ_with_negation(VerbPhraseM, NounPhraseM, ClauseHead),
	clauses_to_meanings(ClausesRest, no_quantifier, [NounPhraseM], VerbPhraseRest).
clauses_to_meanings(Clauses, no_quantifier, [NounPhraseM|NounPhraseRest], VerbPhraseMeanings) :-
	append(Clauses1, Clauses2, Clauses), %! this clause leads to stack overflow rn
	clauses_to_meanings(Clauses1, no_quantifier, [NounPhraseM], VerbPhraseMeanings),
	clauses_to_meanings(Clauses2, no_quantifier, NounPhraseRest, VerbPhraseMeanings).


clauses_to_meanings([Clause1, Clause2|ClausesRest], universal_quantifier, NounPhraseMeanings,
	[disjunction(VerbPhraseM1,VerbPhraseM2)|VerbPhraseRest]) :-
	 % if we have a disjunction, we need to process it separately
	((nonvar(VerbPhraseM1), nonvar(VerbPhraseM2)) -> !
	; otherwise -> true),
	Clause1 = (ClauseHead1:-(ClauseBody,NegatedClauseHead2)),
	Clause2 = (ClauseHead2:-(ClauseBody,NegatedClauseHead1)),
	negate(ClauseHead1, NegatedClauseHead1),
	negate(ClauseHead2, NegatedClauseHead2),
	univ_with_negation(VerbPhraseM1, X, ClauseHead1),
	univ_with_negation(VerbPhraseM2, X, ClauseHead2),
	conjunction_to_terms(NounPhraseMeanings, ClauseBody, X),
	clauses_to_meanings(ClausesRest, universal_quantifier, NounPhraseMeanings, VerbPhraseRest).
clauses_to_meanings([Clause|ClauseRest], universal_quantifier, NounPhraseMeanings, [VerbPhraseM|VerbPhraseRest]) :-
	Clause = (ClauseHead:-ClauseBody),
	univ_with_negation(VerbPhraseM, X, ClauseHead),
	conjunction_to_terms(NounPhraseMeanings, ClauseBody, X),
	clauses_to_meanings(ClauseRest, universal_quantifier, NounPhraseMeanings, VerbPhraseRest).

% the equivalent of the '(=..)/2' operator except it can also take
% not(functor) as the first argument
univ_with_negation(not(Functor), Arg, not(Term)) :-
	!,
	Term =.. [Functor, Arg].
univ_with_negation(Functor, Arg, Term) :-
	Term =.. [Functor, Arg].

% turn an arbitrarily long conjunction into a list of terms
% usage: conjunction_to_terms(Meanings, Terms, X)
% example: conjunction_to_terms([human, flies], [human(X), flies(X)], X)
conjunction_to_terms([], true, _).
conjunction_to_terms([Meaning], Term, X) :-
	!,
	univ_with_negation(Meaning, X, Term).
conjunction_to_terms([Meaning|MeaningsRest], Terms, X) :-
	Terms = (Term,TermsRest),
	univ_with_negation(Meaning, X, Term),
	conjunction_to_terms(conjunction(MeaningsRest), TermsRest, X).

```

Ultimately, this ended up being much more complex than anticipated due to a
mix of insufficient instantiation errors when applying predicates "in the
other direction", out of memory errors due to issues with the recursion,
and various other things.
