## A New Foundation for Realizability

I read a rather fascinating paper recently called "[Implicative algebras: a new foundation for realizability and forcing](https://arxiv.org/abs/1802.00528)". I thought this was a good paper, and I need content for this blog, so I figure giving a summary of it would be a good idea.

The paper itself, from the very beginning, spends an inordinate amount of time on classical realizability, implicative algebras, and complete lattices. By my account, there are two ?most? important insights early in the paper, neither of which require an understanding of any of these. Of course, this assessment is colored by my personal priorities, so another reader can feel free to disagree. The two insights are, firstly, a way to talk about terms and typing on the level of propositions rather than judgments, and secondly, a notion of type-type application, from which term-term application can be recovered as a special case.

The first insight is a simple trick for allowing one to talk about realizer-like terms/proofs and typing/realizing at the level of propositions. The idea is very simple.

1. Define a notion of sub-typing within your logic. `A ⪯ B` means that any realizer for `A` is also a realizer for `B`.
2. Define a function `[[p]]` which takes any term `p` and turns it into a type with one realizer, being `p` itself.
3. You can now state that a term `p` realizes a type `P` on the level of propositions by asserting that `[[p]] ⪯ P`.

To give some examples of this, we can interpret `λx y. x` as `∀X Y. X → Y → X` and `λx y. y` as `∀X Y. X → Y → Y`. We can then observe that both terms are booleans, defined impredicatively as `∀X. X → X → X`, by observing that `∀X Y. X → Y → X ⪯ ∀X. X → X → X` and `∀X Y. X → Y → Y ⪯ ∀X. X → X → X`.

This technique is obviously not generally applicable. While some terms have obvious interpretations, most terms don't. In particular, terms not in normal form cannot, in general, be trivially given types in ordinary type theory. This is where that type-type application I mentioned earlier comes in. With type-type application, which I'll simply denote by juxtaposition, a term like `(λx y. y)(λx. x x)` will be given the type `[[λx y. y]][[λx. x x]]`.

I'll talk about exactly what type-type application means in a bit. Before I do, I want to digress a bit into where I think this idea has the most potential. In some circles, there has been a push to approach logic from a semantics-first angle. In such an approach, the semantics "is" the logic, and any operator which is interpretable into the semantics is a valid operator of the logic. Everything else, such as axiomatics, is secondary to the semantic interpretation. By giving a valid interpretation to these operators within a semantics-first logic, we may use this technique to talk about whatever counts as a proof for that logic, within that logic. A prominent example of this is Computability Logic (CoL), where formulas are interpreted as games, and any formula, where its game has a winning strategy, is interpreted as a theorem. CoL historically has not had the ability to talk about strategies, but by following the previous description, it seems intuitively simple to define a game `[[s]]` which is winnable only by strategy `s`, and an operator `A ⪯ B`, meaning that any strategy for winning a game `A` also wins a game `B`. `A ⪯ B` may even be interpretable as an elementary game. Through this, we can talk about specific strategies winning specific games.

Some of that had to do with the idiosyncrasies of CoL itself, but I hope the general principal is clear. To be closer to the mark, there are several extensional type theories, such as PRLs and Cedille, which could make use of this technique. `⪯` may potentially be modeled as a kind of `Cast` type, for instance. However, it would be most valuable to single-sorted approaches to logic, such as CoL or Illative Combinatory Logic.

Anyway, the type-type application, `AB`, is interpreted as the type of thing you get when applying an `A` to a `B`. For example, `(X → Y)X ⪯ Y` since a `Y` results from applying a function `X → Y` to an `X`. Generically, `A ⪯ B → A B`, meaning that, if we interpret `A` as a function taking `B`'s then it is a function which returns the sort of thing you get when applying a `B` to an `A`.

In the paper, it's mentioned that ordinary conjunction can be used as a valid notion of application. Indeed, `(X → Y)X`, interpreted as `(X → Y) ∧ X`, gives `Y` by modus ponens. However, we'd have that `AB` is the same thing as `BA`, which is not going to be true in general. The exact notion of application we get will largely depend on our notion of sub-typing. In a Heyting-algebra approach to logic, `A ⪯ B` means, essentially, that `A` implies `B`, which is a valid approach but is incongruent with the sub-typing interpretation I've been emphasizing here. Generally, given a category of types with sub-typing acting as morphism, type-type application is left-adjoint to implication. That is `AB ⪯ C` if and only if `A ⪯ B → C`. Or, more intuitively, (the sort of thing one gets when applying an `A` to a `B` is always a `C`) if and only if (`A`'s are always functions taking `B`'s and returning `C`'s).

We can see where application commutativity breaks down when we look at higher-order quantifiers from the likes of System F. Generically, applying an `∀X.F` to an `A` will give the same sort of thing as applying an `F` (with every instance of `X` substituted for `A`) to an `A`. That is, `(∀X.F)A ⪯ (F[A/X])A`. This is just a special case of the observation that `∀X.F ⪯ F[A/X]`, for any `A`. Some example calculations:

    A = ∀Y . Y → Y
    B = ∀X . X → X → X

    AB = (∀Y . Y → Y)B
       ⪯ (B → B)B
       ⪯ B
       = ∀X . X → X → X

    BA = (∀X . X → X → X)A
       ⪯ (A → A → A)A
       ⪯ A → A
       = (∀Y . Y → Y) → ∀Y . Y → Y

And from here we can catch sight of the recovery of ordinary lambda calculus. If we interpret `[[λx . F]]` as `∀X . X → [[F]]{[[x]] := X}`, then all lambda expressions can be interpreted as valid types, and `[[A]] ⪯ [[B]]` will generically hold whenever `A` evaluates to `B`.

From here, we can prove things like

    [[λf g a. f a (g a)]]
    = ∀F . F → ∀G . G → ∀A . A → F A (G A)
    = ∀X Y Z . (X → Y → Z) → (X → Y) → X → Z,
    
giving a unique typing to the S combinator (assuming `A ⪯ B` and `B ⪯ A` implies `A = B`).

One of the main fixations of the paper is that of a complete lattice. This gives us universal meets `⋏ X . F X` and joins `⋎ X . F X`. Generically, `A` is a subtype of `⋎ X . F X` whenever `F A` holds, and `⋏ X . F X` is a subtype of `A` whenever `F A` holds. They should satisfy the following principals;
    
    ⋏Witness : ∀ A F . (∀ X . F X → A ⪯ X) → A ⪯ ⋏ X . F X
    ⋏Elim : ∀ A F . F A → ⋏ X . F X ⪯ A
    
    ⋎Witness : ∀ A F . F A → A ⪯ ⋎ X . F X
    ⋎Elim : ∀ A F . (∀ X . F X → X ⪯ A) → ⋎ X . F X ⪯ A

These operators are similar to existential and universal types. Compare:

    ∀Witness : ∀ A F . (∀ X . A ⪯ F X) → A ⪯ ∀ X . F X 
    ∀Elim : ∀ A F . ∀ X . F X ⪯ F A

    ∃Witness : ∀ A F . F A ⪯ ∃ X . F X  
    ∃Elim : ∀ A F . (∀ X . F X ⪯ A) → ∃ X . F X ⪯ A

Using these meets and joins, we can define an application for a particular notion of implication as `A B = ⋏X . A ⪯ B → X`. That is, `A B` is a subtype of anything, `X`, that can result when interpreting `A` as a function that takes `B`s as inputs.

This is all stuff from the paper, and it goes much further, so I recommend reading it all if you're interested. Stepping a bit outside the paper, we can define a variety of operators valuable to type theory using meets and joins. For example;

Pi types:

    Π X Y = ⋎Z . ∀x. x ⪯ X ⇒ Z x ⪯ Y x

Implicit product types:

    ∀ᵢ X Y = ⋎Z . ∀x. x ⪯ X ⇒ Z ⪯ Y x

Function types (should be provably equivalent, though not necessarily equal, to `X → Y`):

    F X Y = ⋎Z . ∀x. x ⪯ X ⇒ Z x ⪯ Y

Intersection Types:

    ∩ X Y = ⋎Z . Z ⪯ X & Z ⪯ Y

Dependent Intersection Types:

    ι X Y = ⋎Z . Z ⪯ X & Z ⪯ Y Z

where `⇒` and `&` are appropriate (?propositional?) notions of implication and conjunction. Note that I haven't verified the correctness of these definitions, they just seem intuitively correct, but there may be some subtlety that's fooled me. It all seems very promising to me, and it makes me wonder what minimal set of operators is necessary for a full foundation of mathematics. Cedille already doesn't use much, but I think these can go even further.

It's worth noting that, in Illative Combinatory Logic, the `Ξ` combinator acts as a sort of sub-typing operator, and all the aforementioned things can be defined with it plus a type of all realizers alone. Though, such systems are inconsistent, and require interpreting realizers on the same level as types and construing typability with application, which is obviously wrong to me. I don't know how to reconcile these things, but I think there's something here.
