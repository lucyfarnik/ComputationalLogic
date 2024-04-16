%%% Definite Clause Grammer for prolexa utterances %%%

:- consult(library).

utterance(Clauses) --> sentence(Clauses).
utterance(Clauses) --> question(Clauses).
utterance(Clauses) --> command(Clauses).


%%% lexicon, driven by predicates %%%

%%% VOCABULARY %%%
% unary predicates for adjectives, nouns, and verbs
pred(messiah, 1,[n/messiah]).
pred(naughty, 1,[a/naughty]).
pred(boy,     1,[n/boy]).
pred(blessed, 1,[a/blessed]).
pred(cheesemaker, 1, [n/cheesemaker]).
pred(released, 1,[a/released,a/weleased]).
% pred(mortal,  1,[a/mortal,n/mortal]).
% pred(human,   1,[a/human,n/human,n/person]).
%pred(man,     1,[a/male,n/man]).
%pred(woman,   1,[a/female,n/woman]).
%pred(married, 1,[a/married]).
%pred(bachelor,1,[n/bachelor]).
%pred(mammal,  1,[n/mammal]).
% pred(bird,    1,[n/bird]).
%pred(bat,     1,[n/bat]).
% pred(penguin, 1,[n/penguin]).
% pred(sparrow, 1,[n/sparrow]).
% pred(fly,     1,[v/fly]).
% proper nouns
proper_noun(brian) --> [brian]|[bwian].
proper_noun(roger) --> [roger]|[woger].
% proper_noun(tweety) --> [tweety].
% proper_noun(peter) --> [peter].

%%% GRAMMAR %%%
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

noun_s2p(Noun_s,Noun_p):- % convert singular noun to plural
	( Noun_s=human -> Noun_p=humans
	; Noun_s=person -> Noun_p=people
	; atom_concat(Noun_s,s,Noun_p)
	).

verb_p2s(Verb_p,Verb_s):- % convert plural verb to singular
	( Verb_p=fly -> Verb_s=flies
	; 	atom_concat(Verb_p,s,Verb_s)
	).

% adj_pos2neg(Adj_pos,Adj_neg):-
% 	( Adj_pos=nice -> Adj_neg=naughty
% 	).

%%% sentences %%%
sentence(Clauses) --> 
	top_level_noun_phrase(Number, Quantifier, NounPhraseMeanings),
	verb_phrase(Number, VerbPhraseMeanings),
	{clauses_to_meanings(Clauses, Quantifier,
		NounPhraseMeanings, VerbPhraseMeanings)}.

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
%! TODO implement existential quantifiers into the new structure
% determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2 :- true)]) -->[some].

% noun_phrase(singular,M) --> [a],noun_phrase(singular,M).
noun_phrase(Number, simple, [Meaning]) --> noun(Number,Meaning).
noun_phrase(Number, simple, [Meaning1, Meaning2]) --> 
	degree, adjective(Meaning1), noun(Number,Meaning2).
% noun_phrase(Number, simple, not(Meaning)) --> ['non-'], noun(Number, Meaning).
noun_phrase(Number, complex, Meanings) --> properties(Number, Meanings).

degree --> []|[very]|[vewy]. % for now, this carries no semantics

verb_phrase(Number, Meanings) --> properties(Number, Meanings).

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




	
	







% when there's only one meaning for the verb phrase and no quantification,
% we can just process each noun phrase separately
% clauses_to_meanings([Clause|ClausesRest], no_quantifier, [NounPhraseM|NounPhraseRest], [VerbPhraseM]) :-
% 	nonvar(Clause),
% 	!, % if clauses are defined, we need to iterate over them to avoid instantiation errors
% 	clauses_to_meaning1(Clause, no_quantifier, NounPhraseM, VerbPhraseM),
% 	clauses_to_meanings(ClausesRest, no_quantifier, NounPhraseRest, [VerbPhraseM]).
% clauses_to_meanings(Clauses, no_quantifier, [NounPhraseM|NounPhraseRest], [VerbPhraseM]) :- 
% 	clauses_to_meaning1(Clauses1, no_quantifier, NounPhraseM, VerbPhraseM),
% 	clauses_to_meanings(Clauses2, no_quantifier, NounPhraseRest, [VerbPhraseM]),
% 	append(Clauses1, Clauses2, Clauses).

% % when there's only one meaning for the verb phrase and universal quantification,
% % the noun phrase meanings become conjunctive if there's more than one
% clauses_to_meanings(Clauses, universal_quantifier, [NounPhraseMeaning], [VerbPhraseM]) :- 
% 	clauses_to_meaning1(Clauses, universal_quantifier, NounPhraseMeaning, VerbPhraseM).
% clauses_to_meanings(Clauses, universal_quantifier, NounPhraseMeanings, [VerbPhraseM]) :- 
% 	clauses_to_meaning1(Clauses, universal_quantifier,
% 		conjunction(NounPhraseMeanings), VerbPhraseM).

% % when there's multiple verb phrase meanings, each of them can be processed independently
% clauses_to_meanings(Clauses, Quantifier, NounPhraseMeanings, [VerbPhraseM|VerbPhraseRest]) :- 
% 	clauses_to_meanings(Clauses1, Quantifier, NounPhraseMeanings, [VerbPhraseM]),
% 	clauses_to_meanings(Clauses2, Quantifier, NounPhraseMeanings, VerbPhraseRest),
% 	append(Clauses1, Clauses2, Clauses).

% % converts between a single noun phrase meaning and a single verb phrase meaning
% % to a set of Prolog clauses
% % usage: clauses_to_meaning1(Clauses, Quantifier, NounPhraseMeaning, VerbPhraseMeaning)
% % note that the Clauses are a list (to handle disjunction)
% clauses_to_meaning1([(Term :- true)], no_quantifier, NounPhraseMeaning, VerbPhraseMeaning) :-
% 						univ_with_negation(VerbPhraseMeaning, NounPhraseMeaning, Term).
% % if the predicate receives a VerbPhraseMeaning containing a disjunction...
% clauses_to_meaning1(Clauses, universal_quantifier, NounPhraseMeaning, VerbPhraseMeaning) :-
% 	nonvar(NounPhraseMeaning), nonvar(VerbPhraseMeaning), % make sure meanings are instantiated
% 	VerbPhraseMeaning = disjunction(VerbPhraseMeaning1, VerbPhraseMeaning2),
% 	!, % if we've found a disjunction, this is the only valid way to handle it
% 	univ_with_negation(VerbPhraseMeaning1, X, ClauseHead1),
% 	univ_with_negation(VerbPhraseMeaning2, X, ClauseHead2),
% 	univ_with_negation(NounPhraseMeaning, X, ClauseBody),
% 	% this relies on the fact that (A implies (B or C)) is equivalent to
% 	% (((A and not C) implies B) and ((A and not B) implies C))
% 	Clauses = [(ClauseHead1:-(ClauseBody,negate(ClauseHead2))),
% 				(ClauseHead2:-(ClauseBody,negate(ClauseHead1)))].
% % if the predicate receives a NounPhraseMeaning containing a conjunction...
% clauses_to_meaning1(Clauses, universal_quantifier, NounPhraseMeaning, VerbPhraseMeaning) :-
% 	nonvar(NounPhraseMeaning), nonvar(VerbPhraseMeaning), % make sure meanings are instantiated
% 	NounPhraseMeaning = conjunction(_),
% 	!, % if we've found a conjunction, this is the only valid way to handle it
% 	univ_with_negation(VerbPhraseMeaning, X, ClauseHead),
% 	conjunction_to_terms(NounPhraseMeaning, ClauseBody, X),
% 	Clauses = [(ClauseHead :- ClauseBody)].
% %! TODO handle disjunctions and conjunctions from clauses to meanings (right now it only works from meanings to clauses)
% %! TODO handle disjunction in the noun phrase (as well as combinations of all of these)
% clauses_to_meaning1([(ClauseHead :- ClauseBody)], universal_quantifier,
% 						NounPhraseMeaning, VerbPhraseMeaning) :-
% 							univ_with_negation(VerbPhraseMeaning, X, ClauseHead),
% 							univ_with_negation(NounPhraseMeaning, X, ClauseBody).

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

negate(not(X), X).
negate(X, not(X)).

negate_all([], []).
negate_all([Meaning|Meanings], [NegatedMeaning|NegatedMeanings]) :-
	negate(Meaning, NegatedMeaning),
	negate_all(Meanings, NegatedMeanings).


%%% questions %%%

question(Query) --> 
	qword, question1(NounPhraseMeaning, VerbPhraseMeaning),
	{univ_with_negation(VerbPhraseMeaning, NounPhraseMeaning, Query)}.

qword --> [].
%qword --> [if]. 
%qword --> [whether]. 

%! FIX THE FIRST CLAUSE HERE with "who"
% question1(X, VerbPhraseMeaning) -->
% 	[who], verb_phrase(singular, [VerbPhraseMeaning]).
question1(NounPhraseMeaning, VerbPhraseMeaning) -->
	[is], proper_noun(NounPhraseMeaning), property2(singular, [VerbPhraseMeaning]).
question1(NounPhraseMeaning, [VerbPhraseMeaning]) -->
	[does], proper_noun(NounPhraseMeaning), iverb(singular, VerbPhraseMeaning).
question1(NounPhraseMeaning, [negate(VerbPhraseMeaning)]) -->
	[does], proper_noun(NounPhraseMeaning), [not], iverb(singular, VerbPhraseMeaning).
question1((Q1,Q2)) --> [are,some],noun(p,sk=>Q1), property(p,sk=>Q2).
question1((Q1,Q2)) --> [do,some],noun(p,sk=>Q1), verb_phrase(p,sk=>Q2).


%%% commands %%%

% These DCG rules have the form command(g(Goal,Answer)) --> <sentence>
% The idea is that if :-phrase(command(g(Goal,Answer)),UtteranceList). succeeds,
% it will instantiate Goal; if :-call(Goal). succeeds, it will instantiate Answer.
% See case C. in prolexa.pl
% Example: 
%	command(g(random_fact(Fact),Fact)) --> [tell,me,anything].
% means that "tell me anything" will trigger the goal random_fact(Fact), 
% which will generate a random fact as output for prolexa.

command(g(retractall(prolexa:stored_rule(_,Clauses)),"I erased it from my memory")) -->
	forget, ([] | [that]), sentence(Clauses). 
command(g(retractall(prolexa:stored_rule(_,_)),"I am a blank slate")) --> forgetall. 
command(g(all_rules(Answer),Answer)) --> kbdump. 
command(g(all_answers(PN,Answer),Answer)) --> tellmeabout,proper_noun(PN).
command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence([(Q:-true)]).
% command(g(explain_question((Q1,Q2),_,Answer),Answer)) --> [explain,why],sentence1([(Q1:-true),(Q2:-true)]).
command(g(random_fact(Fact),Fact)) --> getanewfact.
%command(g(pf(A),A)) --> peterflach. 
%command(g(iai(A),A)) --> what. 
command(g(rr(A),A)) --> thanks.

% The special form
%	command(g(true,<response>)) --> <sentence>. 
% maps specific input sentences to specific responses.

command(g(true,"I can do a little bit of logical reasoning. You can talk with me about humans and birds.")) --> [what,can,you,do,for,me,minerva]. 
%command(g(true,"Your middle name is Adriaan")) --> [what,is,my,middle,name]. 
%command(g(true,"Today you can find out about postgraduate study at the University of Bristol. This presentation is about the Centre for Doctoral Training in Interactive Artificial Intelligence")) --> today. 
%command(g(true,"The presenter is the Centre Director, Professor Peter Flach")) --> todaysspeaker. 

thanks --> [thank,you].
thanks --> [thanks].
thanks --> [great,thanks].

getanewfact --> getanewfact1.
getanewfact --> [tell,me],getanewfact1.

getanewfact1 --> [anything].
getanewfact1 --> [a,random,fact].
getanewfact1 --> [something,i,'don\'t',know].

kbdump --> [spill,the,beans].
kbdump --> [tell,me],allyouknow.

forget --> [forget].

forgetall --> [forget],allyouknow.

allyouknow --> all.
allyouknow --> all,[you,know].

all --> [all].
all --> [everything].

tellmeabout --> [tell,me,about].
tellmeabout --> [tell,me],all,[about].

rr(A):-random_member(A,["no worries","the pleasure is entirely mine","any time, peter","happy to be of help"]).

random_fact(X):-
	random_member(X,["walruses can weigh up to 1900 kilograms", "There are two species of walrus - Pacific and Atlantic", "Walruses eat molluscs", "Walruses live in herds","Walruses have two large tusks"]).


%%% various stuff for specfic events

% today --> [what,today,is,about].
% today --> [what,is,today,about].
% today --> [what,is,happening,today].
% 
% todaysspeaker --> [who,gives,'today\'s',seminar].
% todaysspeaker --> [who,gives,it].
% todaysspeaker --> [who,is,the,speaker].
% 
% peterflach --> [who,is],hepf.
% peterflach --> [tell,me,more,about],hepf.
% 
% what --> [what,is],iai.
% what --> [tell,me,more,about],iai.
% 
% hepf --> [he].
% hepf --> [peter,flach].
% 
% iai --> [that].
% iai --> [interactive,'A.I.'].
% iai --> [interactive,artificial,intelligence].
% 
% pf("According to Wikipedia, Pieter Adriaan Flach is a Dutch computer scientist and a Professor of Artificial Intelligence in the Department of Computer Science at the University of Bristol.").
% 
% iai("The Centre for Doctoral Training in Interactive Artificial Intelligence will train the next generation of innovators in human-in-the-loop AI systems, enabling them to responsibly solve societally important problems. You can ask Peter for more information.").
% 
