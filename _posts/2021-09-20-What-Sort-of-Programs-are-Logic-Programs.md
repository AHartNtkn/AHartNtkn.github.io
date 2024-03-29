{% raw %}
In an [earlier post](http://anthonylorenhart.com/2021-09-04-Extremely-Simple-Self-Interpretation/), I mentioned a potential project where I'd talk about, among other things, logic programming within the lambda calculus a la MiniKanren. As part of that, I've started seriously considering the nature of logic programming.

MiniKanren takes a very opinionated approach to the subject. Rather than having a dedicated language, it has a domain-specific language that acts similar to prolog, implementing unification and search, and whatnot. One can then write programs in whatever top-level language one's using which makes calls to this DSL.

After some thought, I've concluded that this is the wrong approach. Or, at least, I believe one can do much better. There are a few problems with using a DSL. It isolates the functionality of logic programming from the top-level language in an artificial way. Rather than seeing logic programs as just another species of program, DSLs separate out such things into a completely separate layer. It's as if we need an API to access some external piece of hardware, but we're only accessing garden variety programs that could be implemented without a DSL. This makes such programs less flexible, as modifications to things like the search procedure require modifying the underlying DSL, and are less interoperable with top-level programs.

Scheme, the original language of MiniKanren, is more suited to meta-programming than some other languages, so the separation is not necessarily so stark, but the point of it being a superfluous hindrance is still largely correct.

There's a more fundamental flaw with the approach, though. I've, for several years, tried to track down what one might consider a lambda calculus for logic programming. There are a few papers that claim such a thing, but I've not been convinced by any of them. Recently, I had a breakthrough on this quest; not a final answer but a valuable epiphany all the same. Whatever such a calculus might look like, it cannot resemble any mainstream logic programming language as it must not use unification.

To explain, look at this simple program which counts the leaves of a binary tree written in Haskell.

```haskell
leafCount Leaf = 1
leafCount (Branch l r) = leafCount l + leafCount r
```

Note how this program uses pattern matching. If we took a naive approach to compile this program, we might use a bunch of equational checks on an input to ensure it matched one of the two patterns. In Scheme, one would certainly use `cond`; implementing pattern matching manually. But that's not what we would do in the lambda calculus. There are a few options for implementing these binary trees, but any will do to demonstrate my point. The most common is probably the Church encoding which defines trees as terms of type

```
∀ X . X → (X → X → X) → X
```

This essentially defines them in terms of their folds. We'd have the constructors;

```
leaf = λl b. l
branch = λt1 t2 l b. b (t1 l b) (t2 l b)  
```

As an example, the tree `Branch Leaf (Branch Leaf Leaf)` would become `λl b. b l (b l l)`. Using this encoding, the leaf counting program would be compiled to;

```
leafCount = λ t. t 1 (+)
```

Notice what happened here; pattern matching doesn't occur at all. Instead, we design the data representation itself to route information where it needs to go. The program just placed information in the slots, and the data can take or leave it. We can do this since there's an *intended type* to our inputs. The `leafCount` program expects one of two things. We don't need to pattern match, we can, instead, simply provide a table describing what to do in both cases.

This is comparable to a common trick used in imperative programming called a "branchless if statement". If we have a program like 

```c
if (t) {
  return a;
} else {
  return b;
}
```

we can convert it into an equivalent

```c
return a * t + b * (1 - t);
```

This will simultaneously give both results, but one will be deleted while the other is unchanged. If we compare an equivalent lambda expression program;

```
t a b
```

where `t` is a church encoded boolean expression with

```
true  = λa b. a
false = λa b. b
```

we can see that a similar thing is happening. Rather than branching, we simply give both and delete the ones that aren't needed.

To summarize; true pattern matching creates branches, but lambda encoded programs are branchless by nature. Rather than doing any kind of equality checking, data is simply routed anywhere it may go and is discarded if it ends up unused.

If we were in a completely symbolic language then there are no types that can tell us what to expect. Mathematica and Lisps by nature are like this. In Mathematica, we'd have

```mathematica
leafCount[leaf] := 1
leafCount[branch[x_, y_]] := leafCount[x] + leafCount[y]
```

this is doing true pattern matching; it cannot compile it away the way Haskell could. The reason is that every Mathematica function anticipates literally anything being issued into any function at any time. Because of this, it actually must decide what to do when offered an infinite multitude of possibilities. Pattern matching allows it to identify, post-hoc, which patterns might match an arbitrary input. This does have its benefits. We can, for instance, add a new case

```mathematica
leafCount[tranch[x_, y_, z_]] := leafCount[x] + leafCount[y] + leafCount[z]
```

and it will not interfere with anything else. In Haskell, this can't be done since the original type has no `Tranch` constructor. We'd have to make a new function which takes this new type. And, of course, the lambda encoded version would have to be modified in detail to compensate. Specifically, we'd have to modify the way we encode the tree data from the very start. Ultimately, though, I much prefer the lambda calculus where one must encode the intention into the design of data itself rather than giving informative names to symbols that are nothing but vacuous tokens.

Back to logic programming, I have to ask, why is unification used at all? Should there not exist similar methods which avoid it altogether? I think so. It seems to me that unification plays the same role in relational programming that pattern matching does in functional programming. It exists principally as a way to compensate for the expectation that any symbol at all might appear anywhere. But, of course, in practice, we do know what might appear where. Take this commonplace example of a relational addition function;

```prolog
add(z, X, X).
add(s(X), Y, s(Z)) :- add(X, Y, Z).
```

Why would we need unification for this? We know what might appear in all three slots. There are only two options, a zero case, and a successor case. We should be able to use a lambda encoding and route around the data appropriately. We should be able to do this in general and not require unification ever to execute logic programs. A true lambda calculus for relational programming, whatever that might look like, would certainly never do unification; it would be branchless by nature just like the functional lambda calculus.

The point of this post is not to describe a relational lambda calculus. I'm not convinced that such a thing is necessary in the first place. Instead, I want to understand how to implement logic programs in a functional setting without relying on a DSL. I want to understand how to implement the dynamics of logic programs without recapitulating their conventional forms which are rife with inefficiencies.

A good place to start is the logic monad, originally described in

- [Backtracking, Interleaving, and Terminating, Monad Transformers](http://okmij.org/ftp/papers/LogicT.pdf) by Kiselyov et al.

Unfortunately, this doesn't implement logic programming at all. If we look at the type of the `Logic` monad constructor, we see it's;

```haskell
logic :: (forall r. (a -> r -> r) -> r -> r) -> Logic a 
```

This is just a wrapper over the church-encoded list type. All it is, under the hood, are lists that look like

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

To actually run this relation, we can use it as a filter for a procedure that enumerates all `Nat`s. For example, we can define subtraction in terms of this addition as;

```haskell
nats = Z:map S nats

sub z x = filter (\y -> add x y z) nats
```

Of course, when we run this, we get an output that runs forever;

```haskell
> sub (S (S (S Z))) (S Z)

[(S (S Z))
```

The list of answers is returned, but since there are no additional outputs, it stalls forever. A proper logic program would halt at this point, but I'll address that discrepancy in a bit.

The utility of the logic monad becomes clear if we write a query with multiple variables. This will look for pairs of numbers that sum to a particular number, `z`.

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

however, the stalling occurs before exhausting all possible answers. The reason is that the list monad binder is attempting to append the infinite list consisting of pairs of the form `(Z, x)` to the infinite lists of pairs of the form `(S Z, x)`, etc. for each natural number. Since the first appending never ends, we only ever consider pairs of that first form and never get to the others. The logic monad has a different binder that riffles lists together, allowing for a complete search. Using the logic monad binder instead, we'd have

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

Now we see all the answers before the program stalls, but we can certainly do better. What are logic programs doing which allows them to halt despite an infinite number of possibilities remaining? Consider a hypothetical predicate that returns all numbers less than 3.

```prolog
ltt(Z).
ltt(S(Z)).
ltt(S(S(Z))).
```

Instead of enumerating all the numbers as a list, we could posit a non-deterministic superposition combinator, which I'll call `+`, that chooses between two options. We can then define all the natural numbers as

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

This hypothetical computation fails on `S(S(S(nats)))` since its pattern doesn't match anything it's defined over. The previous programs have to wait for things to be fully generated before they can be evaluated, but logic programs may only look at a few layers before halting. To mimic this kind of computation, we need to modify our `Nat` type to include the superposition operator and syntactic recursion. Something like;

```haskell
data NatS = ZS | SS NatS | Or NatS NatS | Rec (NatS -> NatS)
```

We can define a function that converts a superposition of natural numbers into the stream it represents;

```haskell
natL ZS = [Z]
natL (SS x) = map S (natL x)
natL (Or a b) = concat [natL a, natL b]
natL (Rec f) = natL (f (Rec f))
```

and the original stream of all natural numbers can be represented syntactically as;

```haskell
nats = Rec (\x -> Or Z (S x))
```

Note that this is basically defining the natural numbers as `y (λx. z + s x)`, where `y` is the y combinator.

We can then define equality between natural numbers relationally as;

```haskell
natEq (ZS, ZS) = [(Z, Z)]
natEq (SS x, SS y) = fmap (\(x, y) -> (S x, S y)) $ natEq (x, y)

natEq (Rec f, b) = natEq (f (Rec f), b)
natEq (a, Rec f) = natEq (a, f (Rec f))

natEq (Or a b, x) =
  interleave (natEq (a, x)) (natEq (b, x))
natEq (x, Or a b) =
  interleave (natEq (x, a)) (natEq (x, b))

natEq _ = []
```

Notice the `SS` case where we post-compose with an appropriate injection to preserve information during recursion.

A simple example is `natEq (nats, nats)`, which essentially executes the query

```prolog
?- natEq(X, Y)
```

By pre- or post- composing with appropriate injections, we can specialize to other queries. For example, 

```haskell
map (\(x, S y) -> (x, y)) $ natEq (nats, SS nats)
```

essentially executes 

```prolog
?- natEq(X, s(Y))
```

and 

```haskell
map (\(S(S(S Z)), y) -> y) $ natEq (SS (SS (SS ZS)), nats)
```

essentially executes

```prolog
?- natEq(s(s(s(z))), X)
```

```haskell
> map (\(S(S(S Z)), y) -> y) $ natEq (SS (SS (SS ZS)), nats)

[S (S (S Z))]
```

Notice that it halts without having to consider all infinite possibilities.

By repeating this idea, we can get a relational implementation of addition;

```haskell
add (ZS, x, y) = fmap (\(x, y) -> (Z, x, y)) $ natEq (x, y)
add (SS x, y, SS z) = fmap (\(x, y, z) -> (S x, y, S z)) $ add (x, y, z)

add (Rec f, b, c) = add (f (Rec f), b, c)
add (a, Rec f, c) = add (a, f (Rec f), c)
add (a, b, Rec f) = add (a, b, f (Rec f))

add (Or a b, x, y) =
  interleave (add (a, x, y)) (add (b, x, y))
add (x, Or a b, y) =
  interleave (add (x, a, y)) (add (x, b, y))
add (x, y, Or a b) =
  interleave (add (x, y, a)) (add (x, y, b))

add _ = []
```

Take special note of those first two cases. Both take care to post-compose with appropriate injections so we don't lose information. The `ZS` case, in particular, knows what the results of `natEq` will look like and adjusts them appropriately to match what `add` needs to return.

With it, we can define the standard example of subtraction as a query into addition;

```haskell
sub z x = map (\(x, y, z) -> y) $ add (x, nats, z)
```

```haskell
> sub (SS (SS (SS (SS ZS)))) (SS (SS (SS ZS)))

[S Z]

> sub (SS ZS) (SS (SS ZS))

[]
```

And we can do similar with the previous `sumTo` function;

```haskell
sumTo z  = map (\(x, y, z) -> (x, y)) $ add (nats, nats, z)
```

```haskell
> sumTo (SS(SS(SS(SS ZS))))

[(Z,S (S (S (S Z))))
,(S Z,S (S (S Z)))
,(S (S Z),S (S Z))
,(S (S (S Z)),S Z)
,(S (S (S (S Z))),Z)
]
```

There are, clearly, many redundancies here. We should be able to automate much of this.

After coming up with this, I found a paper that develops a very similar idea in more general form;

- [Fixing Non-determinism](https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/ifl2015_post.pdf) by Alexander Vandenbroucke, Tom Schrijvers, and Frank Piessens

But it's not quite complete. They describe a recursive search type, but it doesn't allow data anywhere but the leaves. This means we're still in a situation where data must be fully evaluated before it's examined by our functions. Instead, a more general construction must pass back and forth between data constructors and search constructors. Trying to do this with the type in that paper will generate an infinite type and I couldn't find any way to stop that from happening. However, what I've made in this post is not too elegant, and I feel there's a way to make it quite simple, and something like what's in that paper may be closer than what I've made so far.

We can at least generalize the previous construction to be a variation of the fixed-point type.

```haskell
data FixS m f = Rec (FixS m f -> m (f (FixS m f)))
```

We can define the previous `NatS` type using the standard functor for natural numbers.

```haskell
data NatF r = Z | S r

type NatS = FixS [] NatF
```

We can define `nats` in a largely similar way to before;

```haskell
nats :: (Monad m, Alternative m) => FixS m NatF
nats = Rec (\x -> return Z <|> return (S x))
```

As a further examples, we can define zero and the successor function as;

```haskell
rc = Rec . const . return

zero = rc Z

suc = rc . S
```

We can implement the previous functions in a way that is debatably simpler;

```haskell
natEq (Rec x, Rec y) = do
  xp <- x (Rec x)
  yp <- y (Rec y)
  case (xp, yp) of
    (Z, Z) -> return (Fix Z, Fix Z)
    (S xpp, S ypp) ->
      fmap (\(x, y) -> (Fix $ S x, Fix $ S y)) $ natEq (xpp, ypp)
    _ -> empty

add (Rec x, y, Rec z) = do
  xp <- x (Rec x)
  case xp of
    Z -> fmap (\(x, y) -> (Fix Z, x, y)) $ natEq (y, Rec z)
    (S xpp) -> do
      zp <- z (Rec z)
      case zp of
        (S zpp) ->
          fmap (\(x, y, z) -> (Fix $ S x, y, Fix $ S z)) $ add (xpp, y, zpp)
        _ -> empty
```

```haskell
> sub (suc $ suc $ suc $ suc zero) (suc $ suc $ suc zero)

[Fix (S (Fix Z))]
```

`Rec` is essentially acting as a syntactic thunk. But by taking for granted laziness, thunks become redundant. We can, instead, treat superpositions as codata; that is, infinite data structures that we may freely recurse on. We could define `nats` instead as;

```haskell
nats :: (Monad m, Alternative m) => m (Fix (Compose NatF m))
nats = return (Fix $ Compose Z) <|> return (Fix $ Compose $ S nats)
```

This is literally what we get if the definition in terms of the y combinator mentioned before is used verbatim. This gives us far more elegant implementations than before;

```haskell
natEq (Fix (Compose Z), Fix (Compose Z)) = return (Fix Z, Fix Z)
natEq (Fix (Compose (S x)), Fix (Compose (S y))) = 
  x >>= \xp -> y >>= \yp ->
  fmap (\(x, y) -> (Fix $ S x, Fix $ S y)) $ natEq (xp, yp)
natEq _ = empty

add (Fix (Compose Z), y, z) =
  fmap (\(y, z) -> (Fix Z, y, z)) $ natEq (y, z)
add (Fix (Compose (S x)), y, Fix (Compose (S z))) =
  x >>= \xp -> z >>= \zp ->
  fmap (\(x, y, z) -> (Fix $ S x, y, Fix $ S z)) $ add (xp, y, zp)
add _ = empty
```

Of course, the data inputted to these functions are no longer a superposition making our interfaces a bit different. `sub` would now be defined as;

```haskell
sub z x = nats >>= \n -> fmap (\(x, y, z) -> y) $ add (x, n, z)
```

For a usage example, we'd have

```haskell
zero = Fix $ Compose Z

suc = Fix . Compose . S . return
```

```haskell
> sub (suc (suc (suc (suc zero)))) (suc (suc (suc zero))) :: [Fix NatF]
[Fix (S (Fix Z))]
```

We can reformulate these programs as a recursion scheme which may make future developements simpler. If we think of the recursive call structure of `natEq`, we realize there are two salient cases captured by the type;

```haskell
data NatEqF r 
  = NatEqZero 
  | NatEqSucc r
  deriving Functor
```

The algebra is verbatim over this type, while the coalgebra is actually over `Compose m NatEqF`, for some logic monad `m`.

```haskell
natEqAlg NatEqZero = return (Fix Z, Fix Z)
natEqAlg (NatEqSucc r) = fmap (bimap (Fix . S) (Fix . S)) r

natEqCoalg (Fix (Compose Z), Fix (Compose Z)) = return NatEqZero
natEqCoalg (Fix (Compose (S x)), Fix (Compose (S y))) = 
  liftM2 (\x y -> NatEqSucc (x, y)) x y
natEqCoalg _ = empty
```

From here, we can combine these simply using a special case of a hylomorphism.

```haskell
logHylo :: (MonadLogic m, Functor f)
     => (f (m b) -> m b) 
     -> (a -> m (f a))
     -> a 
     -> m b
logHylo a c =
  join . fmap a . getCompose . 
  fmap (logHylo a c) .
  Compose . c

natEq :: MonadLogic m
  => (Fix (Compose NatF m), Fix (Compose NatF m))
  -> m (Fix NatF, Fix NatF)
natEq = logHylo natEqAlg natEqCoalg
```

and we can implement `add` using the same scheme.

```haskell
data AddF o r 
  = AddZero o
  | AddSucc r
  deriving Functor

addAlg (AddZero o) = 
  fmap (\(y, z) -> (Fix Z, y, z)) $ natEq o
addAlg (AddSucc r) = 
  fmap (\(x, y, z) -> (Fix $ S x, y, Fix $ S z)) r

addCoalg (Fix (Compose Z), x, y) = Compose $ return $ AddZero (x, y)
addCoalg (Fix (Compose (S x)), y, Fix (Compose (S z))) =
  Compose $ liftM2 (\x z -> AddSucc (x, y, z)) x z
addCoalg _ = Compose empty

add :: MonadLogic m
  => (Fix (Compose NatF m), Fix (Compose NatF m), Fix (Compose NatF m))
  -> m (Fix NatF, Fix NatF, Fix NatF)
add = logHylo addAlg addCoalg
```

...

A loose end that is left is that of variable duplication. How would one, for instance, represent the query

```prolog
?- add(X, X, Y)
```

There's a simple but wrong way that will inefficiently get the right answer, but what should really happen is the usage of something like

```haskell 
\(x, y) -> return (Z, Z) <|> return (S x, S y)
```

though this is ill-typed in the version I've made so far. Even if I did make it well-typed, it's not clear how this could interface elegantly with any of my implementations. This is closely related to proper conjunction. While something like conjunction can be implemented with a simple bind, if we have a more sophistocated set of conditions which are not entirely conditional then one needs a simultanious match on multiple datatypes. The above would be equivalent to something like;

```prolog
(X = z,  Y = z); (X = s(X'), Y = s(Y'))
```

repeated. This isn't the same thing as `natEq` from earlier; that function returns a list of pairs of equal numbers, but we need something that only does the work of `eqNat` for only a single layer before recursing. This suggests we need a more fundamental modification of `add` to get the duplicating query. We can implement `add(X, X, Y)` as

```prolog
addC(X, Y) :- natEq(X, Z), add(X, Z, Y).
```



So we don't need a copy construct per say; we can do with something like a simultanious matching.

...

This will be part of a larger project where I want to create an extremely efficient relational interpreter for the purpose of program synthesis. 

{% endraw %}
