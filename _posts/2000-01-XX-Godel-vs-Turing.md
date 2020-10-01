- [Introduction](#heading1)
- [Turing Completeness](#heading1p5)
- [Godel's Incompleteness Theorems](#heading2p5)
- [Final Thoughts](#headingF)
- [Bits and Bobs](#headingL)

<a name="heading1"></a>
## Introduction

I've recently been thinking about a peice of hypocracy pertaining to the foundations of computing and mathematics which few seem to realize exists. This occured to me when reading one of [Gregory Chaitin's books](https://www.amazon.com/Proving-Darwin-Making-Biology-Mathematical/dp/1400077982) which makes frequent use and abuse of this hypocrasy, but that was hardly the first place I've seen it. The conflict comes from two common interpretations of some famous mathematics in the early 20th century. Namely the idea that;

1. The Church-Turing thesis; that Turing proved the existence of universal computers; machines which can compute anything which can be computed in principal.
2. Godel's Incompleteness Theorems; That Godel disproved the existence of universal logics; logics capable of proving anything which can be proved in principal.

To quote from Chaitin's book;

> Instead of certain, mechanical knowledge, Gödel found incompleteness and Turing found uncomputability. But in the process Turing also found complete/universal programming languages, hardware, software, and universal machines.

Both of these interpretations of specific formal results, in fact, contradict each other. The fact that so many people, computer scientists and mathematicians included, believe both at the same time is a testament to how thoroughly misunderstood both of those results are. While I doubt anyone reading this post is unfamiliar with both of those results, I don't expect very many people reading this to understand why those two positions contradict each other.

My goal with this post is to explain the formal content of both of those results, cultivate an intuitve explanation for both of them, and explain how we get each of those interpretations. One things to keep in mind is that the above two statements are interpretations; they are not, themselves, the content of the theorems they refer to. To get from the theorems to those interpretations requires making specific value judgments which I'll detail after explaining the formal results. The judgments required to come to one interpretation conflict with the judgments required to come to the other. By the end of this post, I hope the reader understands the formal content of both theorems and had a better grasp on the implications of interpreting each.


<a name="heading1p5"></a>
## The Church-Turing thesis

The first idea I'd like to tackle is the Church-Turing thesis. The actual formal statement of the thesis is virtually never given anywhere. Usually, something like the following is said;

> it was Turing’s 1936 paper “On Computable Numbers, with an Application to the Entscheidungsproblem” that created the idea of flexible machines, universal machines, the general-purpose computer [...] 

In other words, and interpretation is given, rather than anything resembling the formal result. Reading that paper tels a different story. As the title suggests, it's about computable numbers. [That paper](https://www.cs.virginia.edu/~robins/Turing_Paper_1936.pdf) does contain the original impetus for the Church-Turing thesis in an appendix, where we find a proof of;

> The theorem that all effectively calculable (A-definable) sequences are computable and its converse are proved below in outline. 

In other words, the formal statement of the church-Turing thesis is that all (church encoded) streams of (church encoded) natural numbers (he calls them integers for whatever historical reason) in the lambda calculus can be expressed as programs on a universal Turing machine, and vice versa. The proof essentially involves constructing two compilers; one going from lambda calculus to turing machines and vice versa. After that, we need to show that the constructs are, in some sense, inverses of eachother.





!!!FINISH ME!!!


I won't go through the full proof here because it's tedious and unenlightening. That being said the proof is only three pages long and, despite the paper's nearly century age, not too hard to follow for someone familiar with both models of computation; so knock yourself out if you feel so inclined !!!FINISH ME!!!. 








!!!FINISH ME!!!



One thing that should be clear from this proof is that it does not even try to address compilation of programs in general. It makes frequent and explicit use of the structure of inputs and outputs. Comsiter what's actually proved; that the function space `ℕ → ℕ` definiable in the lambda calculus is equivalent to the function space `ℕ → ℕ` definable on Turing machines. That's a far cry from proving that the lambda calculus and Turing machines are equivalent. This actual statement is faithfully stated in some sources; for example in the [Wikipedia article (2020/10/01)](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis) clearly says that the Church–Turing Thesis

> [...] states that a function on the natural numbers can be calculated by an effective method if and only if it is computable by a Turing machine. 

So long as "effective method" is understood as pertaining to a specific computational model, such as the lambda calculus, that's entirely accurate. Is that so hard?

Okay, so, I hear you ask, what's the problem? Sure, Turing only covered `ℕ → ℕ`, but that doesn't mean other types aren't covered. Are there examples outside of that type where we can't translate programs back and forth?

Yes. Really. And it's not hard to make examples. The differences come up at second order and higher. The differences come up due to a fundamental limitation of the lambda calculus that Turing machines don't have. Turing machine programs can examine their own syntax while lambda expressions cannot. You can write a Turing machine program which does different things if its input has, say, an even number of `1`s instead of an odd number. There's no analog of this in the lamdba calculus. You cannot write a lambda expression which, for example, counts th enumber of applications or bindings in its input. Turing machine programs can also set up mechanisms to count the number of steps an arbitrary program takes to calculate while similar programs cannot, in general, be written in the lambda calculus. When a lambda expression is fed a function as an argument, the only way it can get information out of it is to feed it arguments. If two functions, `f` and `g`, give the same outputs on the same inputs, if they are extensionally equivalent, then they cannot be distinguished by any program in the lambda calculus. I don't know if this theorem has a name, but the admisibility of function extensionality is a common assumption in presentations of the lambda calculus. For example, section 4.1 of [this](https://www.irif.fr/~mellies/mpri/mpri-ens/biblio/Selinger-Lambda-Calculus-Notes.pdf).

This extensional equivalence respecting property of the lambda calculus isn't special. Lots of real world models respect the same principal. Circuits, for instance, cannot count the number of gates of the components they are attached to, for instance. Black boxes exist in practice, if not in theory. Different models of computation say different things about them. The lambda calculus treats everything as a black box.

Since lambda expressions can't distinguish between extensionally equivalent programs but Turing machines can, we can immediately see how to write programs for Turing machines which can't exist for lambda expressions. Something like the following can easily be defined on a Turing machine;

```
f : (ℕ → ℕ) → Bool
f(x) := if |x| > 1000 then True else False
```

`f(id)` would certainly return `True`, while `f((+1) ∘ (-1) ∘ (+1) ∘ (-1) ∘ ...)` would return `False`. Since both functions are extensionally equivelent, this behaviour cannot be represented by any lamdba expression.

Does this disprove the Church-Turing thesis? Well, that depends on who you ask. Obviously, what Turing actually proved still stands, but the original interpretation does not. The most common attempt to resque the pop version of Church-Turing is via a sort of simulation argument. I don't know what this should be called, but it's similar to what's called the "maximality thesis" in the [SEP page](https://plato.stanford.edu/entries/church-Turing/). Essentially, the argument goes, "but you *can* simulate Turing machines on lambda expressions and vice versa, so they can compute the same things". To be clear, the Church-Turing thesis *does not claim this*, but I've seen statements along these lines conflated with the thesis.

The statement itself is true. While it's a somewhat involved exercise, it's not that hard to write a simulation for [some of the simpler](https://mathworld.wolfram.com/TuringMachine.html) Turing machines in the lambda calculus. If that's so, then how could we really make the distinction I just did?

To answer this, we must clarify what it means to represent a natural number on a computer. The generic answer to this question comes from the theory of realizability. This can be presented in many different ways, but I'll explain a version of it that's particularly intuitive to me. For a more generic account see;

* [Realizability: An Introduction to its Categorical Side](https://www.elsevier.com/books/realizability/van-oosten/978-0-444-51584-1)

When considering a mathematical structure, we must isolate the universal property of that structure, represent it as an impredicative encoding, and we can automatically turn it into a statement about predicates that representations must satisfy. For the natural numbers, there are a bunch of formulations of its universal property. [The most standard](https://ncatlab.org/nlab/show/natural+numbers+object#CCC) states that there's a canonical element, `z`, a canonical endomorphism, `s`, and, for any other objects `A` with appropriate elements, `q`, and endomorphisms, `f`, a coherent way to turn `z`s into `q`s and `s`s into `f`s. The impredicative encoding essentially defines the natural numbers as containing any `A` with the appropriate components.

```
ℕ := ∀ A . (q : A) → (f : A → A) → A
```

All this is saying that (one representation) of the natural numbers is essentially a function which takes an `A`, and endomorphism over `A`s ,and returns an `A`. We can turn this into a generic logical relation defining what it means to "act like a natural number".

```
N(n) := ∀ P .
        ∀ q . P(q) → 
        ∀ f . (∀ a . P(a) → P(f(a))) →
        P(n(q, f))
```

This is the simplest example of what might be called a realizability interpretation of the natural numbers. Rather than answer the question "what is a natural number", it tries to answer the question "what does a natural number act like?". In this formulation, a natural number is anythin that implements itteration, essentially. This tells us that any program `p` satisfying `N(p)` is a natural number.

There are other representations of the natural numbers. We could, for example, define the natural numbers in terms of it being the initial algebra over `X ↦ 1 + X`, or the initial object in the category of [rigs](https://ncatlab.org/nlab/show/rig), or the loop space over [the homotopic circle](https://ncatlab.org/nlab/show/circle#as_a_homotopy_type).<sup>1</sup> Each would give a different "API" of sorts that the natural numbers has to specify. But these are all equivalent in a reasonable sense. Specifically, if we have another predicate `N'(p)` defining the natural numbers, then there should exist functions `p` and `q` such that `p(q(x)) = x` and `q(p(x)) = x` such that

```
∀ x . (N(x) → N'(x)) & (N'(x) → N(x))
```

In other words, the programs satisfying the two predicates are isomorphic;
```
{ x | N(x) } ≅ { x | N'(x) }
```
though we may need weaker forms of equivalence when dealing with more nuanced datatypes than the natural numbers.

It's important to understand how this interface works. We only need that programs are allowed to take in arguments. Technically, we don't even need them to be programs, we just need to be able to interpret function application, meaning we can give realizability interpretations to things like game strategies which aren't programs. These interfaces work for any appropriately structured model of computation. For Turing machines, we need the [Smn theorem](https://en.wikipedia.org/wiki/Smn_theorem) to be satisfied so we can interpret function application; but any reasonable universal Turing machine should satisfy it.

From here, we can ask about simulation. We can give a realizability interpretation for Turing machines themselves. From there, we can establish a realizability interpretation for Turing machine programs. Fixing a machine, `U`, and a predicate `T(x)` asserting that `x` acts like a program for `U`<sup>2</sup>, we can reproduce the natural number realizability at the level of the simulation via;

```
NT(n) := T(n) &
         ∀ P .
         ∀ z . T(z) → P(U(z)) → 
         ∀ f . T(f) → (∀ a : A . T(a) → P(U(a)) → P(U(f, a))) →
         P(U(n, (z, f)))
```

I *think* this is right. I've not really done this sort of reasoning before, so this formulation may be off. This essentially establishes what it means for a program to act like a natural number from the perspective of `U`. From here, we can assert a few things. Most notably that any program satisfying `NT` can be turned into one satisfying `N` and vice versa, without losing information. That

```
{ x | N(x) } ≅ { x | NT(x) }
```

Furthermore, we can define the predicate function spaces representing `ℕ → ℕ`;

```
N2(f)  := N(x) → N(f(x))

N2T(f) := T(f) & (NT(x) → NT(U(f, x))
```
These essentially say that a term `f` is a function from `ℕ` to `ℕ` whenever it acts like an `ℕ` when given something else that acts like an `ℕ` as input.

The Church-Turing thesis is essentially the statement that;

```
{ f | N2(f) } ≅ { f | NT2(f) }
```

Or, at least, that's one formulation of it. This breaks down at higher arity. We know that, given

```
N3(g)  := N2(f) → N(g(f))

N3T(g) := T(g) & N2T(f) → NT(U(g, f))
```
representing `(ℕ → ℕ) → ℕ`,
```
{ f | N3(f) } ≇ { f | NT3(f) }
```

What this esstablishes is that simulated Turing machine programs cannot, in general, be lifted to regular lambda calculus programs. This has practical significance. This means we cannot, in general, write compilers from Turing machine programs into the lambda calculus; we must run them on Turing machines instead. You can write a compiler from x86 assembly to Arm assembly. You don't have to, in principal, write a virtual machine to run x86 code on an Arm proccessor. This shows that Arm and x86 assembly are equivalent in a different and stronger way than Turing machines and lamdba calculus is.

These differences also express themselves in how programs within the same system interact. On turing machines, we can, in general, automatically modify a program so that it keeps track of how many computation steps it takes. Similar automatic modifications can be made to curciuts so they conduct their own timings. We can do something similar in the lambda calculus following methods like [Lightweight Semiformal Time Complexity Analysis](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.66.2327&rep=rep1&type=pdf), but it can't be done generically nor automatically since we can't inspect a lambda expressions AST with another lambda expression. This means that there exists a form of self-timing functions for turing machines which is isomorphic to the ordinary non-timing version of those functions. On the otherhand, there can exist no such thing for lambda expressions.

I've been fixating on functions which can be written on Turing machines but not lambda calculus since it's easy to explain. This does not imply that Turing machines can express strictly more things on its top level. At higher orders, when contemplating functions which take functions that take functions as arguments etc., the expressiveness flips and we start getting functions that you can write at top-level in the lambda calculus but not on Turing machines.<sup>3</sup>

We can, if we want, state that the mutual simulatability of the lambda calculus and Turing machines makes them equivalent. This is certainly true, for some notion of "equivalent", but clearly they are not the same. One cannot, generally, translate programs from one to another. Doing so requires placing these programs in virtual machines. Repeating this requires nesting more and more virtual machines from which there is no guarantee of escape. If the two models were truly equivalent, we'd always be able to return to the top level where we aren't simulating anything. It's not generally possible to reflect simulated types to the top level.

Additionally, and, perhaps, more importantly, the two function spaces can't interface with each other. They can't cleanly interact they implement two completely incompatible APIs. This has significant implications for real-world usage. That a system can "simulate anything" isn't useful if doing these simulations incurs an unreasonable amount of technical debt which cannot ever be repaid.

Depending on the context, this whole thing may not be important. If you're already using a good model of computation, it's, perhaps, not worth worrying about the power of your system beyond a certain point. But if you're studying computational models and seeing their relationships then the differences I've described are extremely impotent. In computer science and mathematics we often want to vary the sense in which two things are the same. When counting cows in a field, we are implicitly assuming that, for the purposes of counting, the differenced between the cows don't matter, and so, are considering them the same in a sense that other things in that field are not. When counting brown cows, we are using a stricter notion of equivalence. When we make our choice of equivalence, we are making a context-sensitive value judgement.


For more information on this topic, see;
* [Notions of Computability at Higher Types](https://homepages.inf.ed.ac.uk/jrl/Research/notions1.pdf)
* [Higher-Order Computability](https://www.springer.com/gp/book/9783662479919)

<a name="heading2p5"></a>
## Godel's Incompleteness Theorems

Godel's Second Incompleteness Theorem is probably, for how rarely it's understood, the most widely cited theorem in mathematics. This, in my opinion, mostly comes from the extremely obscure and convoluted methods that Godel originally used. Not that he's at fault; there weren't better alternatives at the time. However, we live in the future! (reletively), and we don't need to use Godel's original methods like some kind of caveman.

At the very begining of Chaitin's book, he quotes a segement from Gian-Carlo Rota about "Problem Solvers and Theorizers". One line that stuck out to me was

> To the theorizer, the supreme achievement of mathematics is a theory that sheds sudden light on some incomprehensible phenomenon. Success in mathematics does not lie in solving problems but in their trivialization. The moment of glory comes with the discovery of a new theory that does not solve any of the old problems but renders them irrelevant.

I suspect that the exherpt is meant to be somewhat self-congradulatory; directing the audience to think of Chaitin as a new, great theorizer who will trivialize much of established biology with the content of his book. I suppose only time will tell on that matter, but the quote did remind me of someone. More than any other mathemetician, this quote reminded me of William Lawvere who meets that description to a T. He trivialized many, many things through his advances in category theory and topos theory. One of them was, as it so happens, Godel's Incompleteness Theorem. In the paper [Diagonal Arguments and Cartesian Closed Categories](http://tac.mta.ca/tac/reprints/articles/15/tr15.pdf) he writes;

> The original aim of this article was to demystify the incompleteness theorem of G̈odel and the truth-definition theory of Tarski by showing that both are consequences of some very simple algebra in the cartesian-closed setting. It was always hard for many to comprehend how Cantor's mathematical theorem could be re-christened as a "paradox" by Russell and how G̈odel’s theorem could be so often declared to be the most significantresult of the 20th century.

Lawvere skimmed over the hard parts of Godel's theorems, but I'll fill in those details as well, mostly using the tequniques described in [Gödel’s Incompleteness after Joyal](https://arxiv.org/pdf/2004.10482.pdf)

Lawvere dows his proving inside a cartesian closed categories. I like working in type theories, so we can represent his proofs by working in a theory with product types, function types, and a unit type, at least. I'll add some other things in certain special cases. The general result which will be used to do all our most abstruce work will be Lawvere's fixed point theorem. This sais, essentially, that the existance of an epimorphism from some object `T` to the internal hom `[T, Y]` implies that every endomorphism over `Y` has a fixed point. Let's lay out the definitions more formally;

* An epimorphism `to : A ↠ B` is defined to have another function, `from : B → A` such that `to (from x) ≡ x`.

* A fixed point of a function `α : Y → Y` is a term `x : Y` such that `α x ≡ x`.

Simple enough. To prove our theorem, we start by assuming that we have an epimorphism `to : T ↠ (T → Y)` and an endofunction `α : Y → Y`. To prove our theorem we need to identify the fixed point. It's simply 
```
fp : Y
fp = to (from (λ x . α (to x x)))
        (from (λ x . α (to x x)))`
```
It may be helpful to verify that this term is well-typed.

To complete our proof, we need to prove that `α fp = fp`.
```
fp
= to (from (λ x . α (to x x))) (from (λ x . α (to x x)))
= (λ x . α (to x x)) (from (λ x . α (to x x)))
= α (to (from (λ x . α (to x x))) (from (λ x . α (to x x))))
= α fp
```
We just evaluate `fp`, apply the fact that `to (from x) ≡ x`, and simplify.

This calculation is closely related to the proof of `Y f = f (Y f)` for `Y = λ f . (λ x . f (x x)) (λ x . f (x x))`.
```
Y f
= (λ f . (λ x . f (x x)) (λ x . f (x x))) f
= (λ x . f (x x)) (λ x . f (x x))
= f ((λ x . f (x x)) (λ x . f (x x)))
= f ((λ f . (λ x . f (x x)) (λ x . f (x x))) f)
= f (Y f)
```
In some sense, an epimorphism of the sort assumed gives us the exact resources neccessary to set up that kind of self-referential loop.

Okay, now that that's out of the way, how is it useful? In reality, we usually want the contrapositive; that the existance of an endomorphism without a fixed point implies that such an epimorphism doesn't exist.

I'll use cantor's theorem as an example. Cantors theorem states that there are more real numbers than natural numbers. The original version of cantor's diagonalization argument essentially instructs us to imagine the infinite decimal expansion of a countable list of real numbers and then constructing a number out of that list ysing the digits on the diagonal.

We can significantly simplify the formal presentation of the proof using Lawvere's Fixed Point theorem. We can rephrase the theorem as stating that there doesn't exist an epimorphism from `ℕ` to `ℝ`. The intermediate steps only pertain to the infinite decimal expansions, which we can model as a a function from `ℕ` to `Fin(10)`, to a finite set with ten elements. In otherwords, the core of our proof is that there doesn't exist an epimorphism `ℕ ↠ (ℕ → Fin(10))`. There are lots of endofunctions over `Fin(10)` which don't have fixed points; any permutation, really. For example, we could use
```
f(n) := if n == 9 then 0 else n + 1
```
By mapping this function over a list of infinite decimal expansions, we will get an infinite decimal expansion of something not in that list, thus completing the proof. Of course, `ℕ → Fin(10)` isn't the real numbers. Really, the (nonegative) real numbers are [`ℕ → ℕ`](https://www.sciencedirect.com/science/article/pii/S1571066105802725). We can reproduce the same proof for the nonnegative reals proper by exibiting the function `λ x . x + 1` instead, which, of course, has no fixed point.

Now that a clear example has been made, we can target Godel's Incompleteness Theorems proper. While the core of the theorems is already here, most of the technical work of the theorems is involved in the setup. This is similar, really, to cantor's theorem; most of our work is making the problem look like it's asserting the nonexistance of an appropriatly shaped epimorphism.  








Both of Godel's theorems take place in some logic, `T`, which has a quotation mechanism. That is, for any formula in the logic, `φ`, the quoted `"φ"` is also a term in the logic. For some logics, figuring out how to represent this quotation mechanism can be tricky. Furthermore, we assume `T` is expressive enough to talk about itself and reason about the provability of various statements. There should be some expression in the language of the form `Pr(E)`, meaning that the quoted expression `E` is provable. We may also assume the existance for a proof coding; that there are terms in `T` representing proofs. By `Pr(S, φ)`, We'd mean that `S` is a proof for the theorem `φ`.

The main technical acheivement of either theorem, really, was the observation that godel encodings are possible in a system as simple as Peano Arithmetic.



 The essential statements of the incompleteness theorems are;

* First: There are true but unprovable statements in `T`.

Our endofunction is simply the following;

```
E : T → T
E(x) = ∀ s . ¬ Pr(s, x)
```

In other words; There is some formula `φ` which is unprovable by `T` such that `T` can't prove `Pr("φ") → φ`.


* Second: 


 There is some formula `φ` which is unprovable by `T` such that `T` can't prove `Pr("φ") → φ`.

* Second: There does not exist any refutable `φ` such that `T` can prove `Pr("φ") → φ`.





<a name="headingF"></a>

## Final Thoughts


<a name="headingL"></a>

## Bits and Bobs

Well, that's it. I wanted this post to be streamlined, but there were other interesting digressions I didn't go in to. This part of the post contains some footnotes expanding on some constructions mentioned but not expanded upon in the main post.

<sup>1</sup>

If you're wondering what the interfaces for the other definitions would look like, here you go.

* initial algebra over `X ↦ 1 + X`

FINISH ME!


* the initial object in the category of rigs

FINISH ME!

* the loop space over the homotopic circle



FINISH ME!


<sup>2</sup>

A Turing machine usually accepts binary strings as inputs. Bits and binary strings are, of course, just another datatype. As such, we can give them a realizability interpretation too.

```
Bit := ∀ X . X → X → X

B(b) := ∀ P .
        ∀ t . P(t) →
        ∀ f . P(f) →
        P(b(t, f))
```
This states, essentially, that bits implement the `if` function interface.

```
BitString := ∀ X . X → (Bit → X → X) → X

BS(s) := ∀ P .
         ∀ n . P(n) →
         ∀ c . (∀ b t . B(b) → P(t) → P(c(b, t))) →
         P(s(n, c))

T(p) := BS(p)
```



<sup>3</sup>

Here's an example of a program which can be written in the lambda calculus but not on Turing machines.

FINISH ME








