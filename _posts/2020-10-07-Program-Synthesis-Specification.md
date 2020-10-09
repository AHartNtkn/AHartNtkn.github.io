This post will be a small note on program specifications. One of my long-term goals is that of a bootstrapping program synthesizer. That is, a program synthesizer that can synthesize better program synthesizers. One important step is specifying, formally, what this means. I've recently figured out how to properly formulate this.

The generic program synthesis problem is essentially one of finding a program meeting a relational specification. That is, we have some relation, `R`, and we want to find a computable function, `f`, such that `f(x) = y` implies `x R y`. Some examples of relational specifications are;

* x is-sorted-to y :=
  * y is-permutation-of x
  * is-sorted y

* (max_weight, items) has-knapsack-solution x :=
  * x ⊆ items
  * max_weight ≥ total_weight(x)
  * ∀ y ⊆ items, max_weight ≥ total_weight(y) ⇒ total_value(x) ≥ total_value(y)

A relational specification is a relation specifying which outputs can come from which inputs. A relational specification is not necessarily functional; the knapsack specification doesn't preclude there being multiple equivalent maximal knapsacks.

In the ideal case, we can just give the following series of inequalities;

* R is-satisfied-by f :=
  * f ⊆ R
  * f · f° ⊆ id
  * id ⊆ f° · f

That first condition states that any pair `(x, y)` such that `f(x) = y` must also satisfy `x R y`. That is, a function meeting the relational specification is a sub relation. The second condition states that `f` is functional, that every input is mapped onto, at most, a single output. That third condition is totality, stating that everything is mapped onto, at least, one output. By solving this system of inequalities using relational algebra, as described, for instance, in [Program Design by Calculation](http://www4.di.uminho.pt/~jno/ps/pdbc.pdf), we can derive a function meeting this specification algebraically.

There are a few problems with this approach. The biggest is that, often, we want to specify programs in terms of uncomputable relations. That is, relations that don't have computable functions meeting them. Program synthesis is one such relation. It's not generally decidable whether or not there exists a program that meets a specification. As a consequence, while "is-satisfied-by" is a perfectly good relation between `f` and `R`, it doesn't have a solution when we set `R` to it.

Dropping the totality condition from "is-satisfied-by" will make the meta-problem solvable. While it's not decidable if there exists a program that meets a specification, it is hemidecidable. That is, there exists a program that will find a function if it exists. If it doesn't exist, then the program may run forever. However, doing this introduces degenerate solutions, like `f` mapping everything onto nothing (e.g. looping infinitely on every input). We'd need a way to measure how well different solutions performed; we need a way to measure the fitness of different solutions. Maybe, under the right circumstances, "is-satisfied-by" without totality could be used to formally prune the search space in a reasonable way before applying a genetic algorithm or some other fitness optimizer.

We could try fixing this by replacing totality with a condition like

  * R° · R ⊆ f° · f

which asserts that all inputs assigned to something by the relation are also assigned to something by the function. However, this approach puts us back into the same position we were before when the relation is, itself, total but non-computable. If we explicitly formulate our problem as a hemidecision procedure, though, our relation won't be total this may work just fine.

When we have a noncomputable relation in general, our only hope is to approximate a solution. We can specify a fitness function that maximizes in the case that our function actually does meet the relation. Otherwise, our task is simply to maximize this fitness function. The simplest way to specify this fitness function is to use algorithmic probability; a function of high fitness should maximize the algorithmic probability of correct outputs.

The algorithmic probability of an output, `y`, conditioned on some input, `x`, denoted `m(y|x)`, is simply the likelihood that a random program will produce `y` on input `x`. A random program, `p`, is selected from a distribution according to the number of bits, `I(p)` in it; according to the number of binary decisions made during its construction. There's a 50/50 split at every bit/decision, and the programs with more bits are proportionally less likely. Specifically, the likelihood of a program `p` will be `2 ^ -I(p)`, so programs become exponentially less likely as the number of decisions made in its construction increases. This makes sense since each decision doubles the number of possible programs.

```
m(y|x) = Σ{ p(x) = y } 2 ^ -I(p)
```

From there, a function, `f`, of good fitness should maximize `m(y|f(x))` when `x R y`. However, since there may be many `y`s which `R` assigned to `x`, and our `f` can, at best, pick only one, we must select whichever `y` is closest. Our full fitness function is essentially the expectation of this when `x` is sampled from the universal distribution;

```
fit(R, f) := 𝔼{ x ~ m } [ max{ x R y } m(y|f(x)) ]
```

If `R` doesn't assign `x` to anything, then anything the program outputs is valid. In such cases, `max{ x R y } m(y|f(x))` simply becomes `m(f(x)|f(x))`, which is greater than or equal to `m(id)`. I'm not sure what the best way to handle this is; presumably, it should be to restrict the `x`s which are actually assigned by `R` to some output rather than allow irrelevant inputs to affect the fitness.

Passing through the coding theorem, we could, of course, formulate this in terms of Kolomogorov complexity;

```
NLfit(R, f) := 𝔼{ x ~ m } [ min{ x R y } K(y|f(x)) ]
```

where minimizing `NLfit` is equivalent to maximizing `fit`.

Here, `m` is the universal distribution assigning to `x` the likelihood that it will be produced by a random program; its algorithmic likelihood.

```
m(x) = Σ{ p = x } 2 ^ -I(p)
```

This essentially makes the fitness function prioritize algorithmically simpler inputs when considering their contribution to the expected fitness.

If `f` meets our specification exactly, it will be the case that,

```
fit(R, f) ≥ m(id)
```

In the case that `id` is also the simplest program, which is the case for Turing machines and the lambda calculus, then

```
NLfit(R, f) = K(id)
```

If there's a global optimum, then we'd just argmax over the fitness function, but program synthesis for noncomputable relations is generally monotone. That is, no matter how good our program does, we could always make a better one. This is like a "name the biggest number" problem; we could always name a bigger number. As an example, a relation assigning `true` to halting programs and `false` to nonterminating programs is just such a noncomputable relation. However, we could make progressively better and better functions for determining termination. We could always make improved heuristics for determining the termination of wider and wider classes of functions. We could also just memorize examples. Either would improve our function's fitness. This means there isn't a well-defined argmax, and we must, instead, settle for specifying a function improver. Something like;

* (R, f) improves-to f' := fit(R, f) ≤ fit(R, f')

This is essentially the version of program synthesis that can be used for bootstrapping. A better program synthesizer is essentially one that improves fitness faster; though one which improves it by `0` is a valid improver. This allows the usage of a simple modified identity function as a starting point for improvement.

Of course, the fitness function could probably be improved. A good thing to do would be replacing the algorithmic probability with the Levin probability, which just divides the probability of each program, `p`, by the runtime of `p`. This, and perhaps other modifications to punish bad performance, could be made, but the description so far is a solid starting point.

Now all I need is an algorithm that can accept this specification...
