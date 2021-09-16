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

and it will not interfere with anything else. In Haskell, this can't be done since the original type has no `Tranch` constructor. We'd have to make a new function which takes this new type. And, of course, the lambda encoded version would have to be modified in detail to compensate. Ultimately, though, I much prefer the lambda calculus where one must encode the intention into the design of data itself rather than giveing informative names to symbols which are nothing but vacious tokens.

Back to logic programming, I have to ask, why is unification used at all? Should there not exist similar methods which avoid it alltogether? I think so. It seems to me that unification plays the same role in relational programming that pattern matching does in functional programming. It exists principally as a way to compensate for the expectation that any symbol at all might appear anywhere. But, of course, in practice we do know what might apear where. Take this commonplace example of a relational addtion function;

```prolog
add(z, X, X).
add(s(X), Y, s(Z)) :- add(X, Y, Z).
```

Why would we need unification for this? We know what might appear in all three slots. There are only two options, a zero case and a successor case. We should be able to use a lambda encoding and route around the data appropriately. We should be able to do this in general and not require unification ever to execute logic programs. A true lambda calculus for relational programming, whatever that might look like, would certainly never do unification; it would be branchless by nature just like the functional lambda calculus.

The point of this post is not to describe a relational lamdba calculus. I'm not convinced that such a thing is desirable in the first place. Instead, I want to understand how to implement logic programs in a functional setting without relying on a DSL. As part of that, I want to understand how to implement the dynamics of logic programs without recapitulating their conventional forms.












{% endraw %}
