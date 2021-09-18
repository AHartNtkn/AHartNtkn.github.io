{% raw %}
In an [earlier post](http://anthonylorenhart.com/2021-09-04-Extremely-Simple-Self-Interpretation/), I mentioned a potential project where I'd talk about, among other things, logic programming within the lambda calculus a la MiniKanren. As part of that, I've started seriously considering the nature of logic programming.

MiniKanren takes a very opinionated approach to the subject. Rather than having a dedicated language, it, instead, has a domain specific language which acts similar to prolog, implementing unification and search, and whatnot. One can then write programs in whatever top-level language one's using which makes calls to this DSL.

After some thought, I've come to the conclusion that this is the wrong approach. Or, at least, I believe one can do much better. There are a few problems with using a DSL. It isolates the functionality of logic programming from the top-level language in an artificial way. Rather than seeing logic programs as just another species of programs, DSLs separate out such things into a completely separate layer. It's as if we need an API to access some external peice of hardware, but we're only accessing garden variety programs which could be implemented without a DSL. This makes such programs less flexible, as modifications to things like the search procedure require modifying the underlying DSL, and less interoperable with top-level programs.

Scheme, the original language of MiniKanren, is more suited to meta-programming than some other languages, so the separation is not necessarily so stark, but the point of it being a superfluous hindrance is still largely correct.

There's a more fundamental flaw with the approach, though. I've, for a number of years, tried to track down what one might consider a lambda calculus for logic programming. There are a few papers which claim such a thing, but I've not been convinced by any of them. Recently, I had a breakthrough on this quest; not a final answer but a valuable epiphany all the same. Whatever such a calculus might look like, it cannot resemble any mainstream logic programming language as it must not use unification.

To explain, look at this simple program which counts the leaves of a binary tree written in Haskell.

```haskell
leafCount Leaf = 1
leafCount (Branch l r) = leafCount l + leafCount r
```

Note how this program uses pattern matching. If we took a naive approach to compilign this program, we might use a bunch of equational checks on an input to ensure it matched one of the two patterns. In scheme, one would certainly use `conde`; implementing pattern matchin manually. But that's not what we would do in the lambda calculus. There are a few options for implementing these binary trees, but any will do to demonstrate my point. The most common is probably the Church encoding which defines trees as terms of type

```
∀ X . X → (X → X → X) → X
```

This essentially defines them in terms of their folds. We'd have the constructors;

```
leaf = λl b. l
branch = λt1 t2 l b. b (t1 l b) (t2 l b)  
```

As an example the tree `Branch Leaf (Branch Leaf Leaf)` would become `λl b. b l (b l l)`. Using this encoding, the leaf counting program would be compiled to;

```
leafCount = λ t. t 1 (+)
```

Notice what happened here; pattern matching doesn't occure at all. Instead, we design the data representation itself to route information where it needs to. The program just placed information in the slots, and the data can take or leave it. We can do this since there's an *intended type* to our inputs. The `leafCount` program expects one of two things. We don't need to pattern match, we can, instead, simply provide a table describing what to do in both cases.

This is comperable to a common trick used in imparative programming called a "branchless if statement". If we have a program like 

```c
if (t) {
  return a;
} else {
  return b;
}
```

we can convert it into an equivalent

```c
return a * (t % 2) + b * (1 - (t % 2));
```

This will simultaniously give both results, but one will be deleted while the other is unchanged. If we compare an equivalent lambda expression program;

```
t a b
```

where `t` is a church encoded lambda expression with

```
true  = λa b. a
false = λa b. b
```

we can see that a similar thing is happening. Rather than branching, we simply give both and delete the ones that aren't needed.

To summarize; true pattern matching creates branches, but lambda encoded programs are branchless by nature. Rather than doing any kind of equality checking, data is simply routed anywhere it may go and is disgarded if it ends up unused.

If we were in a completely symbolic language then there are no types which can tell us what to expect. Mathematica and Lisps by nature are like this. In Mathematica we'd have

```mathematica
leafCount[leaf] := 1
leafCount[branch[x_, y_]] := leafCount[x] + leafCount[y]
```

this is doing true pattern matching; it cannot compile it away the way Haskell could. The reason is that every Mathematica function anticipates litterally anything being issued into any function at any time. Because of this, it actually must decide what to do when offered an infininte multitude of posibilities. Pattern matching allows it to identify, post-hoc, which patterns might match an arbitrary input. This does has its benefits. We can, for instance, add a new case

```mathematica
leafCount[tranch[x_, y_, z_]] := leafCount[x] + leafCount[y] + leafCount[z]
```

and it will not interfere with anything else. In Haskell, this can't be done since the original type has no `Tranch` constructor. We'd have to make a new function which takes this new type. And, of course, the lambda encoded version would have to be modified in detail to compensate. Specifically, we'd have to modify the way we encode the tree data from the very start. Ultimately, though, I much prefer the lambda calculus where one must encode the intention into the design of data itself rather than giving informative names to symbols which are nothing but vacuous tokens.

Back to logic programming, I have to ask, why is unification used at all? Should there not exist similar methods which avoid it altogether? I think so. It seems to me that unification plays the same role in relational programming that pattern matching does in functional programming. It exists principally as a way to compensate for the expectation that any symbol at all might appear anywhere. But, of course, in practice we do know what might appear where. Take this commonplace example of a relational addtion function;

```prolog
add(z, X, X).
add(s(X), Y, s(Z)) :- add(X, Y, Z).
```

Why would we need unification for this? We know what might appear in all three slots. There are only two options, a zero case and a successor case. We should be able to use a lambda encoding and route around the data appropriately. We should be able to do this in general and not require unification ever to execute logic programs. A true lambda calculus for relational programming, whatever that might look like, would certainly never do unification; it would be branchless by nature just like the functional lambda calculus.

The point of this post is not to describe a relational lambda calculus. I'm not convinced that such a thing is desirable in the first place. Instead, I want to understand how to implement logic programs in a functional setting without relying on a DSL. I want to understand how to implement the dynamics of logic programs without recapitulating their conventional forms which are rife with inefficiencies.

A good place to start is the logic monad, originally described in

- [Backtracking, Interleaving, and Terminating, Monad Transformers](http://okmij.org/ftp/papers/LogicT.pdf) by Kiselyov et al.

Unfortunately, this doesn't implement logic programming at all. If we look at the type of the `LogicT` monad transformer, we see it's;

```haskell
logic :: (forall r. (a -> r -> r) -> r -> r) -> Logic a 
```

This is just a wrapper over the church-encoded list type. All it is, under the hood, are lists which look like

```haskell
\c n -> c 1 (c 2 (c 3 n))
```

etc. If that's all it is, then what does it even do for us? As a first attempt at capturing logic programming, we could try taking seriously the formalization of relations as functions into `Bool`. We could define the addition relation as;

```haskell
data Nat = Z | S Nat deriving (Show, Eq)

add Z y z = y == z
add (S x) y (S z) = add x y z 
add _ _ _ = False
```

To actually run this relation, we can use it as a filter for a procedure which enumerates all `Nat`s. For example, we can define subtration in terms of this addition as;

```haskell
nats = Z:map S nats

sub z x = filter (\y -> add x y z) nats
```

Of course, when we run this, we get an output which runs forever;

```haskell
> sub (S (S (S Z))) (S Z)

[(S (S Z))
```

The list of answers is returned, but since there are no additional outputs, it stalls forever. A proper logic program would halt at this point, but I'll address that descrpency in a bit.

The utility of the logic monad becomes clear if we write a query with multiple variables.

```haskell
sumTo z = 
  filter (\(x, y) -> add x y z) $
  nats >>= \n ->
  nats >>= \m ->
  return (n, m)
```

When we run it, we get a similar result;

```haskell
> sumTo (S(S(S(S Z))))

[(Z,S (S (S (S Z))))
```

however, the stalling occures prior to echausting all possible answers. The reason is that the list monad binder is attempting to append the infinite list consisting of pairs of the form `(Z, x)` to the infinite lists of pairs of the form `(S Z, x)`, etc. for each natural number. Since the first appending never ends, we only ever consider pairs of that first form and never get to the others. The logic monad has a different binder which riffles lists together, allowing for a complete search. Using the logic monad binder instead, we'd have

```haskell
sumTo z = 
  filter (\(x, y) -> add x y z) $
  nats >>- \n ->
  nats >>- \m ->
  return (n, m)
```

```haskell
> sumTo (S(S(S(S Z))))

[(Z,S (S (S (S Z))))
,(S Z,S (S (S Z)))
,(S (S (S (S Z))),Z)
,(S (S Z),S (S Z))
,(S (S (S Z)),S Z)
```

Now we see all the answers before the program stalls, but we can certainly do better. What are logic programms doing which allows them to halt despite an infinite number of possibilities remaining? Consider a hypothetical predicate which returns all numbers less than 3.

```prolog
ltt(Z).
ltt(S(Z)).
ltt(S(S(Z))).
```

Instead of enumerating all the numbers as a list, instead we could posit a non-deterministic superposition combinator, which I'll call `+`, that chooses between two options. We can then define all the natural nubmers as

```
nats = Z + S(nats)
```

Evaluating this will nondeterministically return all natural numbers. Running `ltt` on this we have

```
ltt(nats)
= ltt(Z + S(nats))
= ltt(Z) + ltt(S(nats))
= Z + ltt(S(Z + S(nats)))
= Z + ltt(S(Z)) + ltt(S(S(nats)))
= Z + S(Z) + ltt(S(S(Z + S(nats))))
= Z + S(Z) + ltt(S(S(Z))) + ltt(S(S(S(nats))))
= Z + S(Z) + S(S(Z))
```

This hypothetical computation fails on `S(S(S(nats)))` since its pattern doesn't match anything its defined over. The previous programs have to wait for things to be fully generated before they can be evaluated, but logic programs may only look at a few layers before halting. In order to mimick this kind of computation, we need to modify our `Nat` type to include the superposition operator, failure, and recursion. Something like;

```haskell
data NatS = ZS | SS NatS | Fail | Choice NatS NatS | Rec (NatS -> NatS)
```

We can define a function which converts a superposition of natural numbers into the stream it represents;

```haskell
natL :: NatS -> [Nat]
natL ZS = [Z]
natL (SS x) = map S (natL x)
natL Fail = []
natL (Choice a b) = concat [natL a, natL b]
natL (Rec f) = natL (f (Rec f))
```

and the original stream of all natural numbers can be represented syntactically as;

```haskell
natsS = Rec (\x -> Choice Z (S x))
```

Note that this is basically defining the natural numbers as `y (λx. z + s x)`, where `y` is the y combinator.

After coming up with this, I found a paper which developes a very similar idea;

- [Fixing Non-determinism](https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/ifl2015_post.pdf) by Alexander Vandenbroucke, Tom Schrijvers, and Frank Piessens

But it's not quite complete. They describe this recursive search type, but it doesn't allow data anywhere but the leaves. This means we're still in a situation where data must be fully evaluated before it's examined by our functions. Instead, a more general construction must pass back and forth between data constructors and search constructors.


{% endraw %}
