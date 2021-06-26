{% raw %}
Note: This post is a bit miandering and stream of thought. I've been thinking a lot about this, but I haven't reached a conclustion. Here are a few things I don't want to forget.

## Everything Inside ℕ

One of my main preoccupations recently has been engaging with a philosophy of finitism. I haven't reached any clear conclusion, but this post gives a few notes on the topic.

One of my main motivations has been to engage with the obvious fact that all reasoning humans ever do is purely finite. In mathematics, we often talk about infinite and noncomputable things, but the only things we can ever say about such things, the only methods by which we can even define such things, are fundamentally finite. How, then, could we talk about and with such things without pretending we're doing something infinite?

My interest in this came from my last few projects on isomorphisms between natural numbers and various data structures. There are limits to what can be exhibited this way. Namely, functions from natural numbers to natural numbers are more numerous than natural numbers themselves... or are they? From a set-theoretical point of view, this question is unambiguous; a function is its graph and the set of graphs from ℕ to ℕ is uncountable. But that's not what such functions are in the real world. From a computability standpoint, the collection of definable functions are exactly and only those definable in a Turing complete model of computation. And there are, in fact, countably many of those. But there are redundancies in that description; the real functions from ℕ to ℕ are the total, computable programs from ℕ to ℕ quotiented by extensional equivalence. This becomes a countable class, though one which doesn't have any obvious enumeration.

These thoughts led me to consider Countablism; a philosophy of mathematics where only countable things exist. This isn't really a philosophy as such, but is broadly equivalent to the more mainstream idea of Predicativism. Predicativism is the viewpoint that impredicative definitions shouldn't be used. It has its origins in the earliest foundational crisis of set theory. Frege's original set theory used the axiom of comprehension which stated that, for any predicate, there exists a set of things satisfying that predicate. Russell famously pointed out that this would allow one to define a set of all things which do not contain themselves, thus, paradoxically, defining something which both did and did not contain itself. Some people concluded that the problem was allowing objects to be defined in terms of themselves or in terms of classes that contain the thing being defined, and Predicativism generally advocates the abolishment of any and all such definitions.

Personally, I don't find the central premise of Predicativism to be all that convincing. Part of that is the retrospect offered by systems like [Cedille](https://github.com/cedille/cedille-developments) which uses impredicative definitions for damn near everything while also having a relatively simple proof of consistency. All universal properties in category theory are also impredicative. A Predicativist would contend that such things, rather than being considered definitions, should be thought of as constraints of a sort; in Cedille impredicative definitions are turned into finite algorithms which perform tests on the syntax of programs; in category theory, universal properties describe things without necessarily asserting their existence. To me, this sort of highlights my feelings that the problem isn't impredicativity itself which is clearly fine in most of its usage, but something else which I'm not personally concerned with. What I am concerned with is the usage of countable universes by Predicativism.

One more recent view on Predicativism which I found nice was Nik Weaver's "Conceptualism".

- [Mathematical Conceptualism](https://arxiv.org/pdf/math/0509246.pdf) by Nik Weaver

In this framework, he defines a form of Predicativism which distinguishes between what he calls ι-sets and ι-classes. The ι-sets are those things that are effectively enumerable while the ι-classes are those things that are "effectively recognizable". I still don't understand what the latter means. He mentions infinite streams of bits as an example, but I honestly don't believe we'd be able to recognize such a thing if we encountered it in real life. Weaver makes several super-computation-based arguments which he might use for this, but I find such arguments terminally unconvincing. Something more reasonable is to point out that things like infinite streams have finite representations in terms of automata. "Recognition", in this case, is just type-checking the automata.

This leads me to consider two different classes of types; those defined by an ability to enumerate them and those defined by a finitary test, say a semidecision procedure. The former is something I hit upon a few years ago. I had the thought, inspired by implicative algebras, that we could replace typing with a notion of subtyping and replace types with nondeterministic programs whose inhabitants were all the possible normal forms. If "a+b" represents a program which can nondeterministically evaluate to either "a" or "b" we could, for example, define the booleans as "λx.λy.x+y" or "(λx.λy.x)+(λx.λy.y)".

Here, evaluation order does matter quite a bit. If we don't save the decision until the very last moment, we can often make a decision too soon. We can define the class of naturals as `λz.λs.Y((λx.z)+s)`, where `Y` is the y-combinator. If we made the choice immediately we could have `λz.λs.Ys` at the first step, which doesn't even have a normal form. This does highlight a theoretical concern. Should something like this be considered as an inhabitant of the type defined by `λz.λs.Y((λx.z)+s)`? I'd say no, and declare only the normal forms of lambda expressions to be proper data, but that does ring a bit of arbitrary bureaucracy to me. From an implementation standpoint, this merely means evaluating the program lazily, but as an equational theory, we can't discount the possibility of making the decision up-front. Here's a simple lazy evaluator for the language;

```mathematica
lift[n_, x_Integer] := x + 1 /; x >= n;
lift[n_, λ[x_]] := λ[lift[n + 1, x]];
lift[n_, y_[x_]] := lift[n, y][lift[n, x]];
lift[n_, l_] := l;

sub[e_, n_, x_Integer] := If[x >= n, If[x == n, e, x - 1], x];
sub[e_, n_, λ[x_]] := λ[sub[lift[0, e], n + 1, x]];
sub[e_, n_, y_[x_]] := sub[e, n, y][sub[e, n, x]];
sub[e_, n_, l_] := l;

eval[f_] := spine[f, {}]

spine[f_[a_], {r___}] := spine[f, {a, r}]
spine[λ[k_], {x_, r___}] := spine[sub[x, 0, k], {r}]
spine[a, {x_, y_, r___}] := spine[x, {r}] ∪ spine[y, {r}]
spine[e, {x_, y_, r___}] := {}
spine[r_, l_] := folda[r, l]

folda[f_, {r___, l_}] := 
  Flatten[Outer[#1[#2] &, folda[f, {r}], spine[l, {}]], 1]
folda[e, {}] := {}
folda[f_, {}] := {f}
```

`e` stands for a failure mode; the empty type, and something to prune the space of outputs. `eval[λ[λ[a[0][1]]]]` will return `{λ[λ[0]], λ[λ[1]]}`, the two inhabitants of the booleans.

While I think this sort of system has potential, I stopped working on it due to concerns that the class of types it could represent was too limited. Most type theories are based on syntax testing, and that seemed more powerful than enumeration. I now believe they are equivalent, though not in an obvious way.

I will define a test to simply be a semidecision procedure. We can enumerate all things satisfying that procedure by enumerating all possible data and dovetailing the execution of the procedure on the full stream. In the other direction, any enumeration can be converted into a test by looking for something within the enumeration. There are a few loose ends; specifically, an enumeration assumes a canonical total ordering which a semidecision procedure doesn't provide. Though, I suppose you could encode it manually in the step count so the dovetailing produces the correct ordering, but that's a bit overcomplicated. The real missing piece is structure. Both of these conceptions only provide notions of infinite sets; they don't give us a way to define structures like groups or spaces. We can't take quotients under either conception. That's the real limitation.

But, using encodings, we can carve out structures. This can be done with lambda expressions, or sets, or, as my last few posts have emphasized, natural numbers. Types in traditional type theories correspond to tests performed on the syntax of programs. Encoding lists of lambda expressions within the lambda calculus isn't hard. From;

- ["A self-interpreter of lambda calculus having a normal form"](http://people.dm.unipi.it/berardu/Art/1992Self-interpreter/efficient.pdf) by Berarducci and Bohm
- ["Eff‌icient Self—Interpretation in Lambda Calculus"](https://www.researchgate.net/publication/2673832_Efficient_Self-Interpretation_in_Lambda_Calculus) by Torben Mogensen

we can define a simple HOAS encoding of lambda terms into the lambda calculus via;

`[λx.y] := λla.l(λx.[y])`
`[xy] := λla.a[x][y]`
`[v] := v` where `v` is a variable.

through this encoding, we can directly access the syntax of a program. We can define an evaluator/unquote function as 

```
unquote q :=
  q (λf.λx.unquote (f x))
    (λx.λy.(unquote x)(unquote y))
```

which can be encoded internally via the Y combinator. Through it we can enumerate all the programs who's syntax passes some test generated from a datatype. Issueing that to `unquote` will get us the type of those programs virbatum.

I would like to have a nice self-contained system with countable types as data, countable data as data, countable functions as data, etc. but perhaps I'm asking too much and flirting with the exact sorts of things that breed inconsistency. Partial answers are certainly possible. The system I described has enumerable types, though they can't be enumerated up to type equivalence. Similar things are true of some predicative systems, such as J2, a countable set universe that supports the standard set-theoretic encodings of things, but has the same limitation of not being able to encode inhabitants up to isomorphism. I think this should be possible.

I find some of these ideas quite nice, but the story isn't complete. I feel like I don't really understand how these ideas integrate with data types themselves. How do we get from this conception of types to the richness of, say, dependent type theory? Implementing a type checker for one isn't hard, but that's not what I'm asking about. What would motivate us, a priory, to care about the particular class of tests defined by, say, the simply-typed lambda calculus or System F? If I were a formalist, perhapse I would be satisfied, but I do believe in semantics, so I want more.

I have a dream to see everything, all at once. Perhaps I already have and didn't realized it.

## Very Big Things Inside ℕ

Consider computable functions. Conventional wisdom says there cannot be a bijective encoding of such things up to extensional equivalence since this would allow one to solve the halting problem. However, an encoding only needs to be computable in one direction, the decoding from a natural number. On the other hand, the ability to solve the halting problem comes from the encoding of equivalence classes into natural numbers. The reason we cannot encode computable functions is simpler and not neccessarily related to halting.

Assume we have a computable enumeration of computable functions, `e : ℕ → (ℕ → ℕ)`. Define a new function with the following definition;

`f n = 1 + (e n n)`

Since `f` differes from every function within `e` on at least one input, `f` must not be within `e`. This together with our assumptions which make `f` computable shows that `e` must not be complete.

I believe a similar fixed-point argument would occure when considering equivalence classes of concepts within any theory sufficiently expressive theory. Does such a thing have to hold for the concepts in a foundation of mathematics as well?

Ultimately, the question boils down to modeling infinite things using finite things. As a simple example of this, consider the following;

The natural numbers are a model of the ordinal ω under the ordinary ordering relation. If we modify the ordering relation so it's ordinary ordering still applies between numbers of equivalent parity (even/oddness). But when comparing an even number to an odd number, we always return true. Under this ordering, 1 is now an internal model of ω, with the whole of natural numbers being a model for 2ω.

We can separate the natural numbers into an infinite number of infinite families; for example by separating numbers by the number of 1s at the end of a number's binary expansion. We can keep the ordinary ordering within these families, but between them, we always order numbers with more 1s above those with less. For each n, nω is now internally represented and the natural numbers as a whole model ω². We can subdivide a countable set into a countably infinite family of countably infinite families. We may do this subdivision on each of these families. We can do this subdivision an arbitrarily large but finite number of times. We can further define a countably infinite family containing each infinite family divided n times for all n. In this way, there are infinite many layers of infinite subdivisions of the natural numbers. We can define bijections between natural numbers and some ordinal notation. We can then define an ordering for this notation and map it back onto the natural numbers making them a model for any countable ordinal. 

The key to these constructions is the ordering relation which is finite so long as its computable. This suggests an inescapable need to interface with some notion of computability. Computation is a method to get countable models of uncountable things. One of the main limitations of conceptualizing concepts via computational principals is the infinite nature of non-halting programs. However, we don't actually gain any expressiveness by allowing such things. As pointed out by, for example;

- [Turing-completeness totally free](https://strathprints.strath.ac.uk/60166/1/McBride_LNCS2015_Turing_completeness_totally_free.pdf) by Conor McBride

anything computable by a non-total language can be computed by a total language with a parameter stating how many steps a computation should run. If a program halts, we just set the parameter high enough so that enough iterations pass to reach this halting state. In this way all programs terminate while also removing the need for describing any limiting behavior. There are a few corollaries; for example that anything which can be computed by a Turing machine can be computed by a finite circuit. This is, of course, common wisdom; but it does not seem like the relation between circuits and totality is not often appreciated. Though note that functions can represent infinite families of circuits, so we lose some schematic convenience in using them exclusively. And, of course, as mentioned before the functions which are well-typed in a simple theory with intersection types are exactly the total ones.

In this way, we can embed the totality of a computational model into a countable domain. In this way, we can create countable models of uncountable domains. We can characterize the computable reals, complex numbers, etc. We can do this for arbitrary metric spaces, as in some constructive models all metric spaces are separable;

- [Every metric space is separable in function realizability](https://arxiv.org/abs/1804.00427) by Andrej Bauer and Andrew Swan

But this is, arguably, a classic result. From the Löwenheim–Skolem theorem we learn that all infinite theoryies with countably many sentences and variables have countable models. The way it goes about proving this is honestly rather clumsy; though someone who sees more signifigance to the particular notation of first-order logic might disagree. At the very least, there are nicer countable models of heiarchies of set universes.

## HITs

There is a nice construction of a cumulative heirarchy of sets in homotopy type theory via higher inductive types. It's described in one of the chapters of the HoTT book as a minor modification of the ordinary definition of W-types. I don't have a good understanding for how to formulate such things without an ambient theory like I can with (dependent) inductive types as described in my previous post on bijective encodings. If such a framework could be made then creating intuitive, countable models of such things should be straightforward.

One obvious direction is to piggy-back off CaTT, as I described in some detail in my previous post;

- [Omega Categories Made Easy](http://anthonylorenhart.com/2020-10-09-Omega-Categories-Made-Easy/)

That construction, though complicated, is essentially combinatorial and can easily create countable models for finitely genertated ω-categories and ω-groupoids.

Through the existing constructions of coinductive types, this would also give countable acounts of all the synthetic topoligical spaces.

- [NON-WELLFOUNDED TREES IN HOMOTOPY TYPE THEORY](https://hott.github.io/M-types/m-types.pdf) by BENEDIKT AHRENS, PAOLO CAPRIOTTI, AND RÉGIS SPADOTTI
- [Partiality, Revisited: The Partiality Monad as a Quotient Inductive-Inductive Type](https://arxiv.org/abs/1610.09254) by Thorsten Altenkirch, Nils Anders Danielsson, Nicolai Kraus
- [Various new theorems in constructive univalent mathematics written in Agda](https://github.com/martinescardo/TypeTopology) by Martin Escardo

Though the construction of M-types requires the usage of Pi-types, meaning we need a prestablished notion of function beforehand. 

One direction for this which seems promising is some work coming out of the Wolfram Model program. This is a physics research program funded by Wolfram Research trying to use local graph transformations as models for physics. One aspect of it is various automated theorem proving tequniques for graph equalities. This is described in this paper for proving things about ZX-calculus diagrams.

- [ZX-Calculus and Extended Wolfram Model Systems II: FastDiagrammatic Reasoning with an Application to Quantum Circuit](https://arxiv.org/abs/2103.15820) by Jonathan Gorard, Manojna Namuduri, Xerxes D. Arsiwalla

Near the end a connection with higher-inductive types is mentioned. Essentially, each possible replacement rule acts as a 1-homotopy. Some different applications of rewrite rules converge on the same outcome; such things are said to have 2-homotopies between them. This extends to an (honestly rather trivial) notion of n-homotopy. This is elaborated on in a more recent paper;

- [Homotopies in Multiway (Non-Deterministic) Rewriting Systems as n-Fold Categories](https://arxiv.org/abs/2105.10822) by Xerxes D. Arsiwalla, Jonathan Gorard, Hatem Elshatlawy

Though I found it to be a bit disapointing. A future paper by the same group was teased during on of their presentations (I think it was the one given at GRETA) seems to say something more interesting. Part of the automated theorem proving system they use utilizes a combination of knuth-bendix, superposition, resolution, and paramodulation to elaborate all the different ways various terms within a local region of rewrite space might be related. Each of these tequniques gives a proof-net like graph subject to its own simplification/rewrite rules. This may give insights into the foudnations of logic, and if enough control could be exerted one might be able to give a generic account of higher-inductive types with a particular ease for formalizing logics ameanable to first-order unification based reasoning. But, we'll have to see.
































{% endraw %}
