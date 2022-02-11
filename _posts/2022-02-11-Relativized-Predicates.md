{% raw %}
Over the past few months I've been obsessively reading about various topics related to finitism. I may or may not write a post in the future covering the topic more broadly; this post is intended to focus on one particular technique I learned about from the book;

- [Predicative Arithmetic](https://web.math.princeton.edu/~nelson/books/pa.pdf) by Edward Nelson

It really opened my eyes to the possibility of formalizing mathematics in extremely weak systems. It's main technique is the topic of this post.

That book is about formalizing mathematics in a system called Robinson's Q. This is essentially just Peano Arithmetic without mathematical induction. In other words, it's axioms are just first order logic plus the recursive definitions of addition and multiplication plus the assertion that everything is either zero or the successor of something plus the assertion that zero isn't the successor of any number.

At first glance, it seems like it's impossible to get anything done in the system. Without induction we can't even prove very basic properties, such as associativity of addition. Instead of naively trying such a thing, Nelson instructs us to think laterally.

Firstly, what is the definition of a number? For the purposes of Robinson's Q, it's the following;

```
c(x) := x = 0 ∨ ∃y. x = y + 1 ∧ c(y)
```

Or, more usefully,

```
c(0) ∧ ∀x. c(x) → c(x+1)
```

Either works as a definition. Here I chose 'c' for 'counting' number. We cannot prove that everything in Robinson's Q meets this predicate, but we know, meta-theoretically, that either definition covers the whole semantically, and so we can use it as a fine definition. We can use it as an assumption when reasoning about numbers. But this still doesn't allow us to prove much of anything.

Next, consider the following, which I'll call 'associative' numbers;

```
a(x) := c(x) ∧ ∀y z. (y + (z + x)) = ((y + z) + x)
```

Since the second conjunct is redundant in the usual setting, we might think that this doesn't do anything for us. But we couldn't originally proof associativity, so we might with something that simply assumes it outright.

Can we justify using this definition internally, though? What prevents us from assuming just anything?

Firstly, we can trivially prove that

```
∀x. a(x) → c(x)
```

This tells us that `a(x)` defines, at most, the same elements as `c(x)`. Furthermore, we can observe the following;

`a(0) ∧ ∀x. a(x) → a(x+1)`

This is just the definition of `c`, but with `c` replaced with `a`. The proof of this is fairly simple. I'll leave it as an exercise to the interested reader so as to not distract from its significance. Because `a` meets the definition of `c`, we know that `a` defines, at least, the same things as `c`.

So `a` is at most and at least `c`; in other words, it's semantically the same. This isn't surprising, really, but that we can exhibit this in Robinson's Q is somewhat surprising. We can't complete the bijection

```
∀x. c(x) ↔ a(x)
```

but we can exhibit all the evidence that the two are, really, the same things internally anyway. Of course, can we actually prove associativity with it?

```
-------------------------------------------------------------------
c(x), (y + (z + x)) = ((y + z) + x) ⊢ (y + (z + x)) = ((y + z) + x)
--------------------------------------------------------------------------
c(x), ∀y z. (y + (z + x)) = ((y + z) + x) ⊢ (y + (z + x)) = ((y + z) + x)
-------------------------------------------------------------------------
a(x) ⊢ (y + (z + x)) = ((y + z) + x)
---------------------------------------
⊢ a(x) → (y + (z + x)) = ((y + z) + x)
```

Yes, we can! And we can go further! Ultimately, Nelson ends up proving that all algebraic properties of addition and multiplication are provable via relativized predicates in Robinson's Q. Further, he proves something like that any inductively provable sentence which makes use of arbitrary usage of addition and finitary usage of multiplication relativizes. He goes a bit further using an additional operation which he denotes `#`, but that's a sideshow to the real moral of the book.

Near the end he presents an elaborate proof that the totality of exponentiation doesn't relativize. That is to say, if we define an operation by

```
a^0 = 1
a^(b+1) = a * (a^b)
```

He proves that it's not possible to define any predicate, `e` in Robinson's Q such that;

```
∀x y z. e(x) ∧ e(y) ∧ x^y = z → e(z)
```

He goes on to elaborate more philosophical points attempting to cast doubt on the idea that exponentiation really is total. Throughout the book, he gives various arguments about why mathematical induction might not really be true, and he uses various facts, such as this one, to give specific examples for where induction might actually fail in practice. Near the end of his life, he was writing a book

- [Elements](https://arxiv.org/abs/1510.00369) by Edward Nelson

which was attempting to prove that Peano Arithmetic was, in fact, inconsistent.

This is all very interesting, but I don't believe Nelson was on the right track; and not for the reasons of most mathematicians. Nelson was wrong about Robinson's Q in the first place. Q is, in fact, able to relativize the totality of exponentiation, just not the way Nelson was trying it. Instead of relativizing the predicate defining  numbers, we need to relativize the relation defining exponentiation.

Assume `m` is a predicate defining numbers which are closed under multiplication. In other words, we know that;

```
m(0)
∀x. m(x) → m(x+1)
∀x y. m(x) ∧ m(y) → m(x * y)
```

Now observe that exponentiation, as a ternary relation, is defined as;

```
^(x, 0, 1)
^(x, y, z) → ^(x, y+1, x * z)
```

Define a new ternary relation;

```
^2(x, y, z) := ^(x, y, z) ∧ (m(x) → m(z))
```

We can observe that;

```
^2(x, y, z) → ^(x, y, z)
^2(x, 0, 1)
^2(x, y, z) → ^2(x, y+1, x * z)
m(x) ∧ m(y) ∧ ^2(x, y, z) → m(z)
```

These are all fairly easy to prove. The first is trivial. The second is a trivial consequence of `⊢ m(1)`. The last is trivial. The only nontrivial one is the third. We can prove it by deriving;

```
g1. ^(x, y+1, x * z)
g2. m(x) → m(x * z)
```

from the assumptions

```
a1. ^(x, y, z) 
a2. m(x) → m(z)
```

`g1` can be obtained from `a1` by the definition of `^`. `g2` can be obtained trivially from our assumption that `m(x) ∧ m(z) → m(x * z)` and `a2`.

This shows that Nelson was wrong. While his proof isn't incorrect, he didn't actually show that exponentiation doesn't relativize; he just showed that one particular method doesn't work. I do wonder what he would think of this construction, but I guess we'll never know.

Moving on, we can push this even further. As far as I'm concerned, induction is effectively provable in Robinson's Q.

Fix a binary predicate which I'll suggestively call `ϵ`. I will not make any additional assumptions about `ϵ`. 

Next, consider the following two predicate;

```
i(x) := c(x) ∧ ∀p. ϵ(p, 0) → (∀n. ϵ(p, n) → ϵ(p, n+1)) → ϵ(p, x)
```

This predicate relativizes `c` in a straightforward manner.

```
∀x. i(x) → c(x)
```

which indicates that i contains, at most, the same things as c, is trivial. `i(0)`, too, is trivial. To prove

```
∀x. i(x) → i(x+1)
```

We need to prove;

```
∀x. c(x) ∧ ∀p. ϵ(p, 0) → (∀n. ϵ(p, n) → ϵ(p, n+1)) → ϵ(p, x)
→ (c(x+1) ∧ ∀q. ϵ(q, 0) → (∀n. ϵ(q, n) → ϵ(q, n+1)) → ϵ(q, x+1))
```

The `c(x+1)` goal can be discharged from `c(x)` by the definition of `c`. For the rest, we can split this into the following assumptions;

```
a1. c(x)
a2. ∀p. ϵ(p, 0) → (∀n. ϵ(p, n) → ϵ(p, n+1)) → ϵ(p, x)
a3. ϵ(q, 0)
a4. ∀n. ϵ(q, n) → ϵ(q, n+1)
```

And the goal

```
g1. ϵ(q, x+1)
```

From `a4` we need to prove

```
g2. ϵ(q, x)
```

By specializing `p` in `a2` to `q`, we have the new assumption;

```
a5. ϵ(q, 0) → (∀n. ϵ(q, n) → ϵ(q, n+1)) → ϵ(q, x)
```

Applying `a5` to `g2` we get the two goals;

```
g3. ϵ(q, 0)
g4. ∀n. ϵ(q, n) → ϵ(q, n+1)
```

which are both assumed as `a3` and `a4` respectively.

The fact that `i(0) ∧ ∀x. i(x) → i(x+1)` holds and is the defining formula for `c` show that `i` contains at least the content of `c`. Since `i` is both at most and at least `c`, we can conclude that `i` is really the same thing as `c`. In other words, `i` is, meta-theoretically, an alternative definition for `c`, but we can prove more things with it. In particular, we can prove the following;

```
∀p. ϵ(p, 0) → (∀n. ϵ(p, n) → ϵ(p, n+1)) → ∀n. i(n) → ϵ(p, n)
```

To prove this, we start by noting the assumptions;

```
a1. ϵ(p, 0)
a2. ∀n. ϵ(p, n) → ϵ(p, n+1)
a3. c(n)
a4. ∀q. ϵ(q, 0) → (∀n. ϵ(q, n) → ϵ(q, n+1)) → ϵ(q, n)
```

and the goal

```
g1. ϵ(p, n)
```

By specializing q in 4 to p we obtain

```
a5. ϵ(p, 0) → (∀n. ϵ(p, n) → ϵ(p, n+1)) → ϵ(p, n)
```

by applying this to our goal we obtain

```
g2. ϵ(p, 0)
g3. ∀n. ϵ(p, n) → ϵ(p, n+1)
```

these are assumed as `a1` and `a2`, respectively.

This shows that induction over arbitrary fixed binary predicates `ϵ` is, in fact, provable in Robinson's Q. Of course, since we cant define a transparent truth predicate in Q, we can't define an `ϵ` such that something like the schema

```
ϵ("P", n) ↔ P(n)
```

holds. However, this doesn't really bother me. We might, instead, be able to define an `ϵ` such that `ϵ("P", -)` relativizes `P` for all `P`. I think something like a standard Godel-style provability predicate would do, but I haven't worked out the details.

I also suspect that the "truth" predicate presented in 

- [The Provability of Consistency](https://arxiv.org/abs/1902.07404) by Sergei Artemov

would work. One way of interpreting the moral of that paper, I think, is that the consistency of PA, while not being immediately provable, is actually relativizable; though Artemov doesn't phrase things that way.

Also, the induction predicate I gave is adapted directly from Cedille;

- [Generic derivation of induction for impredicative encodings in Cedille](https://dl.acm.org/doi/abs/10.1145/3167087) by Denis Firsov and Aaron Stump

The moral of Cedille is essentially that induction (and coinduction) for arbitrary inductive (coinductive) datatypes relativizes in surprisingly weak higher-order logics. Something like this is made explicit in this presentation on the future of Cedille;

- [Plan for Cedille 2](https://www.youtube.com/watch?v=2DNL90cAF4w&t=744s)

Relativized predicates are something very powerful which I don't think has been fully appreciated. Even Nelson, who clearly appreciated them more than anyone else, vastly underestimated their true power.

{% endraw %}
