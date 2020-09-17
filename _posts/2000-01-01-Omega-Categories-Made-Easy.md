A few years ago, a new proof theory called CaTT was described in the paper
 - [A Type-Theoretical Definition of Weak ω-Categories](https://arxiv.org/abs/1706.02866)

It contains what is, in retrospect, the simplest and easiest to reason about presentation of ω-categories anywhere I'm aware of. ω-categories are a somewhat essoteric concept, infinite dimensional algebraic structures which seems impossible to effectively reason about at first glance. And yet, reasoning up to arbitrarily many dimensions within them can be made, not just simple but trivial.

My goal with this post is not to merely summarize that paper, but to expand on its contents providing intuition and additional exposition. Hopefully, by the end of it, ω-categories will seem extremely simple to you.

The first off, let's get some some basic intuition for reasoning about higher categories. This intuition will drive all later developments. The second part will describe ways of formalizing this intuition into an actual logic.

The basic intuition behind categories is what I call "puzzle piece reasoning". Think about four puzzle pieces being put together.

![Puzzle](../img/omegacat/puzzle1.png)

Given this sort of puzzle, we can imagine defining operations for composing smaller pieces into bigger ones. We can compose horizontally

![Horizontal Composition](../img/omegacat/Horiz.png)

or vertically. 

![Vertical Composition](../img/omegacat/Vert.png)

We can then come up with equations about these compositions. One of the most obvious is that horizontally composing two peices then vertically composing the results is the same as vertically composing the peices then horizontally composing the results.

![Puzzle Composition Coherence](../img/omegacat/Coh.png)

We can see this by observing that completing either operation will end up with the same thing. All higher categorical reasoning essentially boils down to this. Given two sequences of operations which compose some higher-dimensional cells together, if both end up with the same thing then the sequences of operations are equivalent. If you want to know if two sequences are the same, just look at what you get as a result. You should think back to this intuition throughout this post. This is the exact same reasoning used to justify that horizontal composition commutes with vertical composition in actual 2-categories.

![Composition Coherence](../img/omegacat/GlobCoh.png)

Before formalizing this intuition, I'd like to talk about monoids. They are a simple algebra with a multiplication, `∘`, a unit, `1`, and the equations;
```
1 ∘ a = a ∘ 1 = a
(a ∘ b) ∘ c = a ∘ (b ∘ c)
```
These equations can then be used to justify other, more sophisticated equations. For example;
```
(a ∘ b) ∘ (c ∘ (d ∘ e)) = a ∘ (b ∘ (c ∘ (d ∘ e)))
```
The presentation I've just given is called a "biased" presentation. This is because I'm prioritizing a single operation and especting all others to be formed out of it. Monoids have an infinite number of n-ary operations which can be defined out of multiplication.
```
3∘1(a, b, c)    = (a ∘ b) ∘ c 
3∘2(a, b, c)    = a ∘ (b ∘ c)
4∘1(a, b, c, d) = 3∘1(a, b, c) ∘ d
...
```
and an infinite number of coherence equations to go along with them. For example;
```
a ∘ 3∘2(b, c, d) = 4∘(a, b, c, d)
```
A presentation of a monoid in terms of these infinite operators and coherences is said to be "unbiased". While doing this isn't so useful for monoids, it is neccesary for more sophisticated algebras, like ω-categories, where one can't generate all the operations and equations from a small subset. It will be helpful to see how to do this for monoids since its one of the simplest cases.

It will he helpful to look at one of the most important monoids, lists. Given any type `a`, the type of lists of `a`s is a monoid with concatonation as multiplication and the empty list as the unit.
```
([a, b] ++ [c, d, e]) ++ [f] = [a, b] ++ ([c, d, e] ++ [f]) = [a, b, c, d, e, f]
[] ++ [a, b, c] = [a, b, c] ++ [] = [a, b, c]
```
The monoidal nature of lists is readily apearent from the way they are writen. We can conanicalize an expresion involving lists into a unique representation. To see the monad laws, we can simply normalize the lists and see that the results on either side of the equations are the same. This unique, canonical representation allows us to take the monoid laws for granted. This special property actually makes lists important in the theory of monoids; the type of lists of `a`s is the free monoid over `a`.

With this in mind, we can easily define the full, unbiased theory of monoids. We'll use lists as a way to canonically represent any operation so that we can take all expressions for granted.

I'll define our theory of monoids like a type theory. I'll denote the ambient type `*` which will contain all the things within our monoid. Using lists, we can define canonical representations for any operation. These lists will simply be contexts of things pulled from `*`.
```
Monoid context formation rules
              ⊢ Γ ctx     x free in Γ
--------      -----------------------
⊢ [] ctx         ⊢ Γ , (x : *) ctx
```
The reason we want to use contexts rather than simply lists is so that we can substitute into them. A substitution will simply be a list of things pulled from our theory of the correct type;
```
Monoid substitution formation rules
                  Δ ⊢ σ sub Γ     Δ ⊢ a : A[σ/Γ]
-------------     ------------------------------
Δ ⊢ [] sub []         Δ ⊢ σ, a sub Γ, (x : A)
```
Where `A[σ/Γ]` is substitition `σ` applied to every variable in `Γ` within the type `A`. Our current theory only has one type, `*`, so that's all `A` can be. When we look at categories `A` will also be able to be a morphism between two objects. With substitutions in hand, we can define an operation as an appropriate substitution into a context.

```
Monoid operation formation rule
⊢ Γ ctx   Δ ⊢ σ sub Γ
---------------------
   Δ ⊢ op Γ σ : *
```
For example, for any monoid it's theory will have
```
                     ⊢ a : *                ⊢ a : *       ⊢ b : *  
--------------   --------------------   ------------------------------
⊢ op [] [] : *   ⊢ op [x : *] [a] : *   ⊢ op [x : *, y : *] [a, b] : *


⊢ a : *       ⊢ b : *       ⊢ c : *      
----------------------------------------
⊢ op [x : *, y : *, z : *] [a, b, c] : *

...
```
`op [] []` will act as our `1`,  `op [x : *, y : *] [a, b]` will be `a ∘ b`. From here, from here, we can define an function, `coh`, which generates all the coherence equations. `coh` will take a context and a substitution, just like `op`. It will also take an equation who's sides are canonically represented by the context. For example, the sides of
```
(x ∘ y) ∘ z = x ∘ (y ∘ z)
```
have `[x : *, y : *, z : *]` as their canonical representation. The type of `coh` will be an equation with the variables of that equation with the substitution applied. For example
```
coh [x : *, y : *, z : *] [a, b, a] ((x ∘ y) ∘ z = x ∘ (y ∘ z)) : (a ∘ b) ∘ a = a ∘ (b ∘ a)
```
That ability to substitute is the reason we deal with contexts. When I first read the paper, the contexts seemed like they were overcomplicating things. I tried reformulating the theory without them, but you really need to be able to substitute into them in order to even state many basic theorems.
```
Monoid coherence formation rule
        ⊢ Γ ctx    Δ ⊢ σ sub Γ
Γ ⊢ L : *   Γ ⊢ R : *    FV(L) = FV(R) = FV(Γ)
----------------------------------------------
    Δ ⊢ coh Γ σ (L = R) : L[σ/Γ] = R[σ/Γ]
```
where `FV(X)` is simply the set of all free variables in `X`.

That's it. We now have a canonical way to construct any and all infinite equations for all the infinite operations. For example,
```
1               = op [] []
a ∘ b           = op [x : *, y : *] [a, b]

coh [x : *] [a] (x ∘ 1 = x) : a ∘ 1 = a

3∘1(a, b, c)    = (a ∘ b) ∘ c 
3∘2(a, b, c)    = a ∘ (b ∘ c)
4∘1(a, b, c, d) = 3∘1(a, b, c) ∘ d

coh [x : *, y : *, z : *, w : *]
    [a, b, c, d]
    (x ∘ 3∘2(y, z, w) = 4∘1(x, y, z, w))
   : a ∘ 3∘2(b, c, d) = 4∘1(a, b, c, d)
```
The justification for that final rule is super duper simple. Literally all we do is check that the expressions are well formed and that both sides of the equation mention all the variables in the context. This will also be the only thing we need going all the way to ω-categories. Before we get there, let me digress a bit.

Let's make a type theory where every object is a magma. Objects within a magma can only be constructed in one way, via a multiplication operation. This operation satisfies no equations whatsoever. However, in the unbiased theory, there are equations. For example;
```
3∘1(a, b, c)    = (a ∘ b) ∘ c 
4∘1(a, b, c, d) = (a ∘ b) ∘ (c ∘ d)
```
gives us
```
4∘1(a, b, c, d) = 3∘1(a, b, c ∘ d)
```
Magmas, despite their triviality, do have an equational theory. This comes form the fact that binary trees can be used as a canonical representation of terms in a magma in much the same way lists are for terms in a monoid. If we modify the context formation rules so they are built like trees;
```
Magma context formation rules.
                  ⊢ Γ ctx    ⊢ Δ ctx   FV(Γ)∩FV(Δ) = ∅
-------------     ------------------------------------
⊢ (x : *) ctx                 ⊢ Γ ∘ Δ ctx
```
and similarly modify substitutions
```
Magma substitution formation rules.
    Δ ⊢ t : *       Δ ⊢ σ1 sub Γ1      Δ ⊢ σ2 sub Γ2
-----------------   --------------------------------
Δ ⊢ t sub (x : *)         Δ ⊢ σ1 ∘ σ2 sub Γ1 ∘ Γ2
```
The operation and coherence rules are exactly the same. Using them, we can expose the interesting equational theory of an unbiased magma.
```
a ∘ b        = op ((x : *) ∘ (y : *)) (a ∘ b)
3∘1(a, b, c) = op (((x : *) ∘ (y : *)) ∘ (z : *)) ((a ∘ b) ∘ c) 

coh (((x : *) ∘ (y : *)) ∘ (z : *))
    ((a ∘ b) ∘ c)
    (3∘1(x, y, z) = (x ∘ y) ∘ z)
  : 3∘1(a, b, c) = (a ∘ b) ∘ c
```
We can summarize the general procedure for getting unbiased theories as follows;

1. Define a notion of context which is constructed using the same procedure as objects within an X, with judgmental equalities coinciding with the equations of X.
2. Define a notion of substitution into these contexts.
3. Assert the existance of operations which allow us to construct terms wherever there are valid substitutions into contexts.
4. Assert a coherence construction allowing equations and other theorems to be deduced wherever there are valid substitutions into contexts where those theorems are well-formed.

There are some caviats to this. This assumes that there are no non-trivial interactions between context formation and the data structure. However, in theories with cancelation laws which may delete a variable, such as with groups, contexts won't be able to express this since contexts require new variables to be fresh. Outside of such structures, this procedure is generically very effective.

Let's go up one level in sophistocation before getting to full ω-categories. Let's talk about the unbiased theory of regular categories. 

Our type theory will now have two types of things;
```
Category type formation rules
           ⊢ a : *    ⊢ b : *
--------   ------------------
⊢ * type      ⊢ a → b type
```
The biased theory of categories has only two operations; morphism composition and identity formation. These correspond to the two operations in a monoid, but with extra formation conditions. We can canonically represent compositions of morphisms in the same way we represented multiplications of objects in a monoid; via lists. These lists are called "pasting schemes". The formation rules are a bit different, though. We need to enforce the source and target conditions of a category. Given;
```
f : a → b
g : b → c
h : c → d
```
we can canonically represent `(f ∘ g) ∘ h` as `[a, f, b, g, c, h, d]`. Or, at least, this is what I'd do if I were defining the fre category inside of some kind of library. We want this to be a context, though.
```
[a : *, f : a → b, b : *]
``` 
is not a valid context since `b` is bound after it's mentioned in `f`. We need to modify the context so that the targets are declared right before the morphisms. With that, our canonical composition will be `[a, b, f, c, g, d, h]`. Using this represention, we can formulate the context formation rules. These are a bit more complicated due to the term shuffling, but they're not hard to implement.
```
Category context formation rules
⊢ Γ ps (x : *)                        ⊢ Γ ps (f : x → y)
--------------  --------------------  ------------------
   ⊢ Γ ctx      ⊢ (x : *) ps (x : *)    ⊢ Γ ps (y : *)

           ⊢ Γ ps (x : *)                      
----------------------------------------
⊢ Γ, (y : *), (f : x → y) ps (f : x → y)
```
The `ps` judgement, standing for "pasting scheme", allows us to locate the source of any prospective morphisms we might want to add to our context. This will, thankfully, only need to be barely modified when we go to higher dimensions. Substitution is identical to the rule used for monoids;
```
Category substitution formation rules
                  Δ ⊢ σ sub Γ     Δ ⊢ a : A[σ/Γ]
-------------     ------------------------------
Δ ⊢ [] sub []         Δ ⊢ σ, a sub Γ, (x : A)
```
The operation formation rules need to be a bit different. Since the type of a composition will vary, the operation formation rules needs to have a target type the way previous coherence formers did. 
```
Category operation formation rule
⊢ Γ ctx    Δ ⊢ σ sub Γ    FV(tar(Γ)) = FV(Γ)
   Γ ⊢ S → T type      FV(src(Γ)) = FV(S)    
--------------------------------------------
    Δ ⊢ op Γ σ (S → T) : S[σ/Γ] → T[σ/Γ]
```
`tar(Γ)` will be the last thing of type `*` mentioned, while `src(Γ)` will just be the first thing in the list. The only difference between this rule and the one for ω-categories will be how sources and targets of pasting schemes are calculated. 
We'll also need an extra one for identity morphisms;
```
Category operation formation rule
              Δ ⊢ A : *
----------------------------------
Δ ⊢ op [x : *] [A] (x → x) : A → A
```
From here, the coherences nearly the same as they are for monoids. The only difference is that the left and right sides of the equations must have the same types.
```
Category coherence formation rule
        ⊢ Γ ctx    Δ ⊢ σ sub Γ
Γ ⊢ L : A   Γ ⊢ R : A    FV(L) = FV(R) = FV(Γ)
----------------------------------------------
    Δ ⊢ coh Γ σ (L = R) : L[σ/Γ] = R[σ/Γ]
```
Using this, we can derive the unbiased versions of any ordinary conherence of a category.


Like with monoids, we can formulate the infinite many unbiased operations we might define for composing morphisms.

```
id a  = op [x : *] [a] : a → a
u ∘ v = op [x : *, y : *, f : x → y, z : *, g : y → z] [a, b, u, c, v] : a → c
(note that this is in compositional order rather than applicative order)

coh [x : *, y : *, f : x → y] [a, b, v] (f ∘ id y = f) : v ∘ id b = v
...
```

The only equations of a category are the associativitiy and identity cancelation laws. These are just the monoid laws, but with extra conditions on how compositions are allowed to be formed. In particular, the canonical representti




















Incidently, it is possible to present globular ω-categories in a biased manner. We'd need to define two infinite families of composition and identity operations for every direction in every dimension. This is done, for instance, in 
 - [Steps toward the weak higher category of weak higher categories in the globular setting](http://cgasa.sbu.ac.ir/article_11180_b13cacfd9afe5780932141c269d0add6.pdf)









