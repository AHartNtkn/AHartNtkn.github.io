{% raw %}
- [Agda Formalization](#af)
- [First Example](#fe)
- [Varying the Natural Transformation](#vnt)
- [Mutual Recursion](#mr)
- [Accumulators](#a)
- [Dynamic Programming](#dp)
- [Forgetful Functors](#ff)
- [Composing Adjunctions](#ca)
- [Further Generalization](#fg)
- [Final Thoughts](#ft)

## Introduction

This post will be a tutorial on something called the conjugate hylomorphism, described in this paper;

- ["Conjugate Hylomorphisms Or: The Mother of All Structured Recursion Schemes"](http://www.cs.ox.ac.uk/people/nicolas.wu/papers/Hylomorphisms.pdf) by Ralf Hinze, Nicolas Wu, and Jeremy Gibbons

Personally, I found the paper quite difficult to read at first, and it only became clear to me what was going on by the time I formalized much of the paper's contents. This post will explan things in a way which it hopefully easier to learn from.

I will also assume you have some basic familiarity with recursion schemes; specifically that you know what a hylomorphism is. I did write [a post](http://anthonylorenhart.com/2020-04-07-Sorting-Lists-Recursively;-The-Easy-Way/) about recursion schemes a while ago which covers everything you need to know for this post, but there are tons of tutorials on the topic which will work equaly well. I don't expect anything beyond the basics.

First, a breif review; a hylomorphism, `h : B → A`, over an algebra `a : F A → A` and a coalgebra `c : B → F B` will be a fucntion satisfying `h = a ∘ F(h) ∘ c`. Intuitively, at each recursive step `c` is building up a single layer of the type `Fix F`, `h` is then being mapped through that layer where the next recursive step will occure, and finally `a` destructs that layer to get the output. This describes an extremely general pattern for recursion which covers many algorithms, such as both merge and quicksort.

As general as hylomorphisms are, they don't seem to, at first, cover everything we want. If we have a mutually recursive function, such as;

```
even 0 = True
even n = odd (n-1)

odd 0 = False
odd n = even (n-1)
```

there isn't an obvious way to extract a coalgebra and an algebra; after all it looks like there are two separate recursions going on simultaniously.

Similarly, if we have something stateful going on, like a cache which is used to store old calculations for reuse as in dynamic programming/memoization, something beyond a hylomorphism seems to be going on.

Those and several other things lead to the creation of a family of a dosen or so different recursion schemes each of which cover some special kind of recursion. After this started getting messy, various attempts were made to unify them to varying degrees of success. The conjugate hylomorphism is essentially the final form of this line of research, unifying everything in a way that's extremely neat and conceptually elegant.

As an inital summary, the premise is that all the different recursion schemes are, in fact, ordinary hylomorphisms. However, the algebra or coalgebra is in a different category than our category of types. This other category has an adjunction with our category of types, and by composing with the components of this adjunction we can get a (co)algebra back in our category of types that we can then use in a garden variety hylomorphism. The reason why these other recursion schemes are not obviously hylomorphisms is because their (co)algebras are complicated from the view within our category of types; by decomposing them into an adjunction and a (co)algebra in a different category, things become clearer.

Hopefully that wasn't too abstract to be confusing. To make things more concrete, I'll start putting down my Agda formalization of these ideas.

<a name="af"></a>
## Agda Formalization

First, as a warm-up, let's look at regular hylomorphisms. Since they all rely on the notion of functor, we must define categories and functors first.

```agda
record Category : Set where
  field
    C : Set
    hom : C → C → Set
    𝟙 : ∀ {a} → hom a a
    ∘ : ∀ {a b c} → hom b c → hom a b → hom a c

record Functor (A B : Category) : Set where
  field
    F0 : A .C → B .C
    F1 : ∀ {a b} → A .hom a b → B .hom (F0 a) (F0 b)
```


I'm going to be eliding the equations throughout this post since they're tedious and distracting. If I wanted to do a proper, full formalization, I'd obviously include them, but they won't help a reader so they'll be missing throughout this post.

As two simple examples, we can define our category of types, the product category, and a functor between them;

```agda
Type : Category
Type = record { C = Set
              ; hom = λ x y → (x → y)
              ; 𝟙 = λ x → x
              ; ∘ = λ f g x → f (g x) }
```

```agda
data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

×-hom : {A B : Category} → A .C × B .C → A .C × B .C → Set
×-hom {A} {B} (a₁ , b₁) (a₂ , b₂) = (Category.hom A) a₁ a₂ × B .hom b₁  b₂

×-𝟙 : {A B : Category} → ∀ {a : Category.C A × Category.C B} → ×-hom {A} {B} a a
×-𝟙 {A} {B} {a , b} = Category.𝟙 A , Category.𝟙 B

×-∘ : {A B : Category} → ∀ {a b c : Category.C A × Category.C B} →
      ×-hom {A} {B} b c → ×-hom {A} {B} a b → ×-hom {A} {B} a c
×-∘ {A} {B} {_ , _} {_ , _} {_ , _} (f₁ , f₂) (g₁ , g₂) = A .∘ f₁ g₁ , B .∘ f₂ g₂

_𝕏_ : Category → Category → Category
A 𝕏 B = record { C = A .C × B .C
               ; hom = ×-hom {A} {B}
               ; 𝟙 = ×-𝟙
               ; ∘ = ×-∘ }
```

```agda
prod-F0 : Set × Set → Set
prod-F0 (x , y) = x × y

prod-F1 : ∀ {a b} → (Type 𝕏 Type).hom a b → prod-F0 a → prod-F0 b
prod-F1 {_ , _} {_ , _} (f , g) (x , y) = f x , g y

prod : Functor (Type 𝕏 Type) Type
prod = record { F0 = prod-F0 ; F1 = prod-F1 }

Δ : Functor Type (Type 𝕏 Type)
Δ = record { F0 = λ x → x , x ; F1 = λ z → z , z }
```

With those in place, we can implement our basic recursion scheme rather easily.

```agda
hylo : {T : Category} (F : Functor T T)
       {A B : T .C}
       (a : T .hom (F0 F A) A)
       (c : T .hom B (F0 F B)) →
       T .hom B A
hylo {T} F a c = T .∘ (T .∘ a (F .F1 (hylo F a c))) c
```

Unfortunately the beurocracy of record types requires us to squint a bit, but it should be clear that this is the same definition I gave before. The types highlight a bit more of what's going on; that a hylomorphism is parameterized by an arbitrary category that all our data lives within. The resulting hylomorphism is another morphism within our category.

It's worth noting that this program isn't gaurenteed to terminate. If it doesn't then the morphism doesn't neccessarily exist in our category. The original paper spends a lot of time describing termination proving. That's not something I want to dwell on since I think it distracts from building intuition. But it is important, so I'll describe it breifly.

In order to prove termination, we need to prove that the coalgebra is "recursive". This means that it forms a unique hylomorphism with any well-typed algebra. This turns out to be the same as any of these hylomorphisms terminating. To prove recursiveness we prove that a family of basic coalgebras (like the identity morphism and constant morphisms) are recursive. We then build up a family of combinators which recursiveness is closed under. We then build our desired coalgebra out of these building blocks which proves recursiveness. There's also a duel story where a hylomorphism is proved to be productive iff the algebra is corecursive, meaning it extends to a unique hylomorphism given any coalgebra. Beyond this, I don't want to spend any more time on termination.

To get to the full conjugate hylomorphism, we must formalize adjunctions.

```agda
record Adjunction {C D : Category} (L : Functor D C) (R : Functor C D) : Set where
  field
    _⌊_⌋ : ∀ {A B} → C .hom (L .F0 A) B  → D .hom A (R .F0 B)
    _⌈_⌉ : ∀ {A B} → D .hom A (R .F0 B) → C .hom (L .F0 A) B
```

we can use the functors we defined before as an example of an adjunction;

```agda
Δadj-1 : {A : Type .C} {B : (Type 𝕏 Type).C} →
         (Type 𝕏 Type) .hom (F0 Δ A) B → Type .hom A (F0 prod B)
Δadj-1 {_} {_ , _} (f , g) x = f x , g x

Δadj-2 : {A : Type .C} {B : (Type 𝕏 Type).C} →
         Type .hom A (F0 prod B) → (Type 𝕏 Type).hom (F0 Δ A) B
Δadj-2 {A} {B , C} f = (λ x → fst (f x)) , λ x → snd (f x)

Δadj : Adjunction Δ prod
Δadj = record { _⌊_⌋ = Δadj-1 ; _⌈_⌉ = Δadj-2 }
```

The main complication in constucting our function is the manner of translation between the two categories. If the (co)algebra exists in a different category from our target, its endofunctor will be over the other category. This means we need a way to translate between the two. This requires a natural transformation which turns one endofunctor into another by swapping it with the functor in our adjunction which already lands within our target category.

It's also worth pointing out that our target category can be on either side of the adjunction. Depending on where it is, we are forced to use one of two different implementations. This means we get two different hylomorphisms whose setup is identical but whose target is different.

```agda
cHyloL : {X Y : Category} (F : Functor X X) (G : Functor Y Y)
         {L : Functor Y X} {R : Functor X Y}
         (α : Adjunction L R)
         (σ : {y : Y .C} → X .hom (F0 L (F0 G y)) (F0 F (F0 L y)))
         {B : Y .C} {A : X .C}
         (a : X .hom (F0 F A) A)
         (c : Y .hom B (F0 G B)) →
         X .hom (F0 L B) A
cHyloL {X} {Y} F G {L} {R} α σ a c =
  X .∘ (X .∘ a (F .F1 (cHyloL F G α σ a c))) (Lσ c)
  where
  Lσ : {x : Y .C} → (Y .hom x (F0 G x)) → X .hom (F0 L x) (F0 F (F0 L x))
  Lσ f = α ⌈ Y .∘ (α ⌊ σ ⌋) f ⌉
```

Here, `L` will land in our target category, hence the name.

```agda 
cHyloR : {X Y : Category} (F : Functor X X) (G : Functor Y Y)
         {L : Functor Y X} {R : Functor X Y}
         (α : Adjunction L R)
         (τ : {x : X .C} → Y .hom (F0 G (F0 R x)) (F0 R (F0 F x)))
         {B : Y .C} {A : X .C}
         (a : X .hom (F0 F A) A)
         (c : Y .hom B (F0 G B)) →
         Y .hom B (F0 R A)
cHyloR {X} {Y} F G {L} {R} α τ a c =
  Y .∘ (Y .∘ (Rτ a) (G .F1 (cHyloR F G α τ a c))) c
  where
  Rτ : {x : X .C} → X .hom (F0 F x) x → Y .hom (F0 G (F0 R x)) (F0 R x)
  Rτ f = α ⌊ X .∘ f (α ⌈ τ ⌉) ⌋
```

Here, `R` will land in our target category, hence the name.

As you can see, we have `Lσ` or `Rτ` which use the adjunction and natural transformation to convert the (co)algebra in the other category into our target category. In theory, this natural transformation acts as an additional parameter, but in practice it tends to have little flexibility and its implementation is often forced on us by the type, which is why I didn't mention it in my earlier description. Sometimes it's a subtlty, though.

<a name="fe"></a>
## First Example

This is all well and good, but we really need to buid intuition to get a handle on this. The rest of this post will just be a bunch of examples.

Since Agda's a bit onerous and I want to emphasize that this is a generic programming construct I'm going to swap to Mathematica; a language without any type system at all. I'll still do some informal type-theoretical reasoning, but that's not hard in practice. That's sort of the point of these recursion schemes. They isolate the neccessary reasoning into it's simplest components.

We can implement the datastructures for categories and adjunctions as dictionaries which store the appropriate functions.

```mathematica
cat[refl_, comp_] := <|"refl" -> refl, "comp" -> comp|>

type = cat[# &, Function[{f, g}, f[g[#]] &]];

adj[lr_, rl_] := <|"lr" -> lr, "rl" -> rl|>
```

The conjugate hylomorphisms can then be implemented as special cases of the ordinary hylomorphism;

```mathematica
hylo[fmap_, a_, c_][x_] := a[fmap[hylo[fmap, a, c]][c[x]]]

cHyloL[{Ycat_, Fmap_, adj_, sig_}, a_, c_] :=
  Block[{Lsig},
    Lsig[f_] := adj[["rl"]][Ycat[["comp"]][adj[["lr"]][sig], f]];
    hylo[Fmap, a, Lsig[c]]
  ]

cHyloR[{Xcat_, Gmap_, adj_, tau_}, a_, c_] :=
  Block[{Rtau},
    Rtau[f_] := adj[["lr"]][Xcat[["comp"]][f, adj[["rl"]][tau]]];
    hylo[Gmap, Rtau[a], c]
  ]
```

Note that this is not as general a function as what I gave in Agda. In Agda, I allowed the target to be an arbitrary category. In real world programming, the target will always be the category of types, meaning we aren't going to vary the notion of compotision in the target category; it will always be regular function composition. Without it, the recursion would eagerly try unfolding the composition infinitely.

Let's give a first example which is just a regular hylomorphism. Quicksort makes use of the following functions;

```mathematica
QSFmap[f_][l] := l
QSFmap[f_][b[x_, l_, r_]] := b[x, f[l], f[r]]

QSCoalg[{}] := l
QSCoalg[{x_, l___}] := 
 b[x, Select[{l}, x > # &], Select[{l}, x <= # &]]

QSAlg[l] := {}
QSAlg[b[x_, {l___}, {r___}]] := {l, x, r}
``` 

These will map though, build up, and tear down the layers of the intermediate tree built up by quicksort.

As a conjugate hylomorphism, it's adjunction is the trival adjunction between the identity functor and itself. The two components will just be the identity functions

```
id : ∀ {A B} → (Id A → B) → (A → Id B)
id : ∀ {A B} → (A → Id B) → (Id A → B)
```

```mathematica
idid = adj[# &, # &];

QuickSort = cHyloL[{type, QSFmap, idid, # &}, QSAlg, QSCoalg]
```

```mathematica
In > QuickSort[{6, 4, 5, 2, 0, 9, 3, 8, 1, 7}]
Out> {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
```

We get the same function by using the other hylomorphism. We could have defined it as

```mathematica
QuickSort = cHyloR[{type, QSFmap, idid, # &}, QSAlg, QSCoalg]
```

<a name="vnt"></a>
## Varying the Natural Transformation

We aren't forced to set our `F` and `G` functors to be the same. If we don't then we need a natural transofrmation to mediate between them. As a simple example, we can define a natural transformation between `X ↦ 1 + X` and `X ↦ 1 + X × X` which will produce a perfectly balanced binary tree of the input height. If we use `cHyloL`, we'll be applying the natural transformation before recursing, which means we'll be mapping over `X ↦ 1 + X × X`.

```mathematica
Bsig[z] := l
Bsig[s[x_]] := b[x, x]

BFmapL[f_][l] := l
BFmapL[f_][b[l_, r_]] := b[f[l], f[r]]

BCoalg[0] := z
BCoalg[n_] := s[n - 1]

BalL = cHyloL[{type, BFmapL, idid, Bsig}, # &, BCoalg]
```

```mathematica
In > BalL[3]
Out> b[b[b[l, l], b[l, l]], b[b[l, l], b[l, l]]]
```

On the other hand, if we use `cHyloR`, we'll be applying the natural transformation last, which means we'll be mapping over `X ↦ 1 + X`.

```mathematica
Btau[z] := l
Btau[s[x_]] := b[x, x]

BFmapR[f_][z] := z
BFmapR[f_][s[x_]] := s[f[x]]

BCoalg[0] := z
BCoalg[n_] := s[n - 1]

BalR = cHyloR[{type, BFmap, idid, Btau}, # &, BCoalg]
```

```mathematica
In > BalR[3]
Out> b[b[b[l, l], b[l, l]], b[b[l, l], b[l, l]]]
```

Of course, we can vary our natural transformation from the identity function even if we use the same functor. We can use a natural transformation from `X ↦ ℕ + X × X` to itself to define a simple tree reversal function.

```mathematica
RevSig[n_Integer] := n
RevSig[b[x_, y_]] := b[y, x]

RevMap[f_][n_Integer] := n
RevMap[f_][b[x_, y_]] := b[f[x], f[y]]

Rev = cHyloL[{type, RevMap, idid, RevSig}, # &, # &];
```

```mathematica
In > Rev[b[b[1, 2], b[3, b[4, 5]]]]
Out> b[b[b[5, 4], 3], b[2, 1]]
```

<a name="mr"></a>
## Mutual Recursion

So that's all well and good, but let's look at less trivial examples. Namely, let's look at mutual recursion. Mutual recursion arises out of the adjunction between the category of types and the product category of types. Objects and morphisms in the product category of types are just pairs of types and pairs of functions, respectively.

```mathematica
prod[cat1_, cat2_] :=
 cat[
  {cat1[["refl"]], cat2[["refl"]]},
  
  Function[{fp, gp},
   Block[{fp1, fp2, gp1, gp2},
    {fp1, fp2} = fp;
    {gp1, gp2} = gp;
    {cat1[["comp"]][fp1, gp1], cat2[["comp"]][fp2, gp2]}
    ]]
  ]
```

There are two canonical adjunctions, both involving the diagonal functor which copies a type.

```
Δ X = (X, X) 
```

The other directions are the product and coproduct types.

```
(×)(X, Y) = X × Y

(+)(X, Y) = X + Y
```

The coproduct is left adjoint to `Δ`;

```
(+)X → Y
========
 X → ΔY
```

meaning that a morphism exists from `X.1 + X.2` to `Y` exactly when there is a pair of morphisms, one from `X.1` to `Y`, the other from `X.2` to `Y`.

And the product is right adjoint to `Δ`;

```
 ΔX → Y
========
X → (×)Y
```

meaning that a morphism exists from `X` to `Y.1 × Y.2` exactly when there exists a pair of morphisms, one from `X` to `Y.1`, and the other from `X` to `Y.2`.

Both adjunctions give rise to a form of mutual recursion. I'll focus on the first of these, but I'll talk about the other in a bit. The adjunction will be a pair of functions;

```agda
Δ×LR : (Δ A → B) → (A → × B)
     : ((A, A) → (B.1, B.2)) → (A → B.1×B.2)
     : (A → B.1)×(A → B.2) → (A → B.1×B.2)
Δ×LR (p1, p2) = λa . (p1 a, p2 a)

Δ×RL : (A → × B) → (Δ A → B)
     : (A → B.1×B.2) → ((A, A) → (B.1, B.2))
     : (A → B.1×B.2) → (A → B.1)×(A → B.2)
Δ×RL f = (λa . (f a).1, λa . (f a).2)
```

We can implement both of these rather easily;

```mathematica
delTimes = adj[
  Function[p, {p[[1]][#], p[[2]][#]} &],
  Function[f, {f[#][[1]] &, f[#][[2]] &}]
  ]
```

Reasoning through adjunctions can sometimes be a bit tedious, but they only need to be implemented once and they can be reused for any other function using the same paradigm from then on.

Since the right adjoint will land in our target category, we are forced to use `cHyloR`.

Setting our appropriate adjunction, the type of `cHyloR` will be;

```agda
cHyloR : (F : Functor (* × *) (* × *)) (G : Functor * *)
         (τ : {(X1, X2) : * × *} → (G (X1 × X2) → × (F (X1, X2))))
         {B : *} {(A1, A2) : * × *}
         (a : F (A1, A2) → (A1, A2)) →
         (c : B → G B) →
         B → A1 × A2
```

This kind of scheme is called a "mutumorphism". It simplements a kind of parellel mutual recursion where we have two function running simultaniously while trading information back and forth. To be honest, I don't have good intuition for these kinds of functions, but they are common as the end result of algebraic optimiztion. Let's look at a simple example; the one I gave earlier;

```haskell
even 0 = True
even n = odd (n-1)

odd 0 = False
odd n = even (n-1)
```

The recursive call strucure is exactly along the natural numbers, but twice. This suggests our `G` will simply be the regular endofunctor for the natural numbers, `X ↦ 1 + X` and our `F` will be two coppies of that, except swapped since the recursive calls swap between the functions `(X1, X2) ↦ (1 + X2, 1 + X1)`.

Filling in that information, the type of our `cHyloR` will be 

```agda
cHyloR : (τ : {(X1, X2) : * × *} → (1 + (X1 × X2) → (1 + X2) × (1 + X1)))
         {B : *} {(A1, A2) : * × *}
         (a : (1 + A2 → A1) × (1 + A1 → A2)) →
         (c : B → 1 + B) →
         B → A1 × A2
```

For our function, `A1` and `A2` will be `Bool`, while `B` will be `ℕ`. We can implement our algebra, coalgebra, and functorial map rather easily;

```mathematica
evenOddAlg = (
   evenAlg[z] := True;
   evenAlg[s[x_]] := x;
   
   oddAlg[z] := False;
   oddAlg[s[x_]] := x;
   
   {evenAlg, oddAlg}
   );

evenOddCoalg[0] := z
evenOddCoalg[n_] := s[n-1]

evenOddFmap[f_][z] := z
evenOddFmap[f_][s[x_]] := s[f[x]]
```

Our natural transformation is essentially forced by the type. 

```mathematica
evenOddTau[z] := {z, z}
evenOddTau[s[{x_, y_}]] := {s[y], s[x]}
```

We can now define our function as;

```mathematica
evenOdd = 
  cHyloR[{prod[type, type], evenOddFmap, delTimes, evenOddTau}, 
   evenOddAlg, evenOddCoalg];
```

```mathematica
In > evenOdd[3]
Out> {False, True}
```

So we've got a function which returns the results of both functions. By postcomposing with a projection operator, we get one of our two original functions.

That example was nice and simple, but not too satisfying. A slightly more complex example is this fibbonacci function;

```haskell
fib1 0 = 0
fib1 n = fib2 (n-1)

fib2 0 = 1
fib2 n = fib1 (n-1) + fib2 (n-1)
```

Arguably the most important thing to remember about using hylomorphisms is that the intermediate type should reflect the recursive call structure of the algorithm. Our output will be a pair; the left one resulting from eliminating layers shaped like `1 + X`, the two cases for `fib1`, and the right resulting from eliminating layers shaped like `1 + X × X`, the two cases for `fib2`. These considerations are resolved by our natural transformation. If we work out the type, we find it has to be

```agda
1 + (X1 × X2) → (1 + X2) × (1 + X1 × X2)
```

reflecting the fact that the `fib1` calls `fib2` and `fib2` calls both `fib1` and itself. Our implementation is essentially forced by this type to be;

```mathematica
fibTau[z] := {z, z}
fibTau[s[{x_, y_}]] := {s[y], p[x, y]}
```

Though, obviously, the names of the constructors are arbitrary. We can then implement the algebra simply as;

```mathematica
fibAlg = (
   fib1Alg[z] := 0;
   fib1Alg[s[x_]] := x;
   
   fib2Alg[z] := 1;
   fib2Alg[p[x_, y_]] := x + y;
   
   {fib1Alg, fib2Alg}
   );
```

The functorial map and coalgebra are just those over natural numbers;

```mathematica
fibCoalg[0] := z
fibCoalg[n_] := s[n - 1]

fibFmap[f_][z] := z
fibFmap[f_][s[x_]] := s[f[x]]
```

We can shove all this into `cHyloR` to get;

```mathematica
fib = 
  cHyloR[{prod[type, type], fibFmap, delTimes, fibTau},
    fibAlg, fibCoalg];
```

```mathematica
In > fib[9]
Out> {34, 55}
```

Of course, not all examples of mutual recursion are parellel like those examples. Consider the problem of counting the leaves of the following type;

```haskell
data Rose = Leaf | Branch (NonEmpty Rose)
```

The empty list is a leaf. Each branch is a list of rose trees meaning this requires recursing both on lists of trees (often called "forests") and the trees themselves.

```haskell
countTree Leaf = 1
countTree (Branch (x:xs)) = countForest (x:xs)

countForest (x:[]) = countTree x
countForest (x:xs) = countTree x + countForest xs
```

While this, at first, looks like it might have a similar call structure to the previous fibbonacci implementation, this idea is betrayed by the fact that the arguments to both functions differ. There is no sensable way to define the coalgebra or functorial map since we have two different types. What we need instead is a different kind of mutual recursion which can perform something like a relay; switching between different functions as the types of our layers changes. That's exactly what the adjunction between coproducts and the diagonal does for us.

The adjunction will be a pair of functions;

```agda
+ΔLR : (+ A → B) → (A → Δ B)
     : (A.1 + A.2 → B) → ((A.1, A.2) → (B, B))
     : (A.1 + A.2 → B) → (A.1 → B) × (A.2 → B)
+ΔLR f = (λa1 . f (inl a1), λa2 . f (inr a2))

+ΔRL : (A → Δ B) → (+ A → B)
     : (A.1 → B) × (A.2 → B) → (A.1 + A.2 → B)
+ΔRL (fl, fr) (inl a) = fl a 
+ΔRL (fl, fr) (inr a) = fr a 
```

We can implement both of these as;

```mathematica
plusDel = adj[
   Function[s, {s[{1, #}] &, s[{2, #}] &}],
   Function[p, Function[s, p[[s[[1]]]][s[[2]]]]]
   ];
```

Note that I'm marking `inl` as `{1, #}` and `inr` as `{2, #}` which allows me to save some space.

Our left functor within the adjunction, the coproduct type, lands in our target category, forcing us to use `cHyloL`. Specializing the type of that we get;

```agda
cHyloL : {* : Category} (F : Functor * *) (G : Functor (* × *) (* × *))
         (σ : {(b1, b2) : * × *} → (+ (G (b1, b2))) → F (b1 + b2))
         {(B1, B2) : * × *} {A : *}
         (a : F A → A)
         (c : (B1, B2) → G (B1, B2)) →
         (B1 + B2) → A
```

To a large extent, the work of the hylomorphism will be to fuse the layers of different type into a single uniform type. In our example we have layers of type `X = 1 + Y`, reflecting the branch layers, and layers of type `Y = X + X × Y`, reflecting the lists making up each branch. We can define a new type whose layers are of type `X = 1 + X + X + X × X`. The rose tree can be converted into this form so we nolonger have a situation where we need to swap out functions. This will be the intermediate hylomorphism type, and the algebra over that type will perform the actual counting.

If we work out the type of our natural transformation, we find it must be

```
((1 + b2) + (b1 + b1 × b2)) → F(b1 + b2)
```

where `F X = 1 + X + X + X × X`. There are, in theory, lots of ways to implement this, but only one will sort things correctly. The leaf needs to go to the unit constructor, the branch and singleton lists need to go to their appropriate unitary constructors, and the list constructor needs to go the the branch. Additionally, the mark indicating the type (`inl`/`{1, #}` for branches and `inr`/`{2,#}` for lists) needs to be correct.

```mathematica
rosePlusSig[{1, l}] := z
rosePlusSig[{1, b[x_]}] := ol[{2, x}]
rosePlusSig[{2, n[x_]}] := or[{1, x}]
rosePlusSig[{2, c[x_, y_]}] := t[{1, x}, {2, y}]
```

The counting algebra is defined as

```mathematica
rosePlusAlg[z] := 1
rosePlusAlg[ol[x_]] := x
rosePlusAlg[or[x_]] := x
rosePlusAlg[t[x_, y_]] := x + y
```

The natural transformation and algebra reflect the structure of the original mutually recursive program. The natural transformation sorts out the different types of inputs with the coproduct tag indicating which constructors corresponds to which function. The actual evaluation of the functions are then assigned to the appropriate constructors by the algebra.

The functorial map is straghtforwardly defined as;

```mathematica 
rosePlusFmap[f_][z] := z
rosePlusFmap[f_][ol[x_]] := ol[f[x]]
rosePlusFmap[f_][or[x_]] := or[f[x]]
rosePlusFmap[f_][t[x_, y_]] := t[f[x], f[y]]
```

The coalgebra is trivial since we aren't messing with the structure of the tree. This gives us;

```mathematica
rosePlus = 
  cHyloL[{prod[type, type], rosePlusFmap, plusDel, rosePlusSig}, 
   rosePlusAlg, {# &, # &}];
```

```mathematica
In > rosePlus[{1, b[c[b[c[l, n[l]]], n[b[c[l, c[l, n[l]]]]]]]}]
Out> 5
```

<a name="a"></a>
## Accumulators

Beyond the identity function, there are other adjunctions our category of types has with itself. Specifically, `X →` is right adjoint to `X ×`, for a fixed `X`. This means a function we have two functions

```agda
×→LR : (A → B) → (A → (X → B))
×→LR f a x = f(x, a)

×→RL : (A → (X → B)) → (X × A → B)
×→RL f (x, a) = f a x
```

Because both the left and right functors land in our type universe, we can use either `cHyloL` or `cHyloR`, getting two different paradigms. The adjunction is implemented as;

```mathematica
timesHom = adj[
   Function[f, Function[a, Function[x, f[{x, a}]]]],
   Function[f, Function[p, f[p[[2]]][p[[1]]]]]
   ];
```

If we specialized the types we get

```agda
cHyloL : (F : Functor * *) (G : Functor * *)
         (σ : {y : *} → (X × G y) → F (X × y))
         {B A : *}
         (a : F A → A)
         (c : B → G B) →
         X × B → A

cHyloR : (F : Functor * *) (G : Functor * *)
         (τ : {x : *} → G (X → x) → (X → F x))
         {B A : *}
         (a : F A → A)
         (c : B → G B) →
         B → (X → A)
```

Looking first at `cHyloL`, we see it has a seed `X` for some fixed type `X`. The only thing with access to `X` is the natural transformation. This indicates that this only allows us to vary the structural transformations performed at each layer. As a simple example, we can take a list of pairs and use an `X = Bool` to flip only the even pairs.

```mathematica
eFlipFmap[f_][n] := n
eFlipFmap[f_][c[{x_, y_}, l_]] := c[{x, y}, f[l]]

eFlipSig[{b_, n}] := n
eFlipSig[{False, c[{x_, y_}, l_]}] := c[{x, y}, {True, l}]
eFlipSig[{True, c[{x_, y_}, l_]}] := c[{y, x}, {False, l}]

eFlip = cHyloL[{type, eFlipFmap, timesHom, eFlipSig}, # &, # &];
```

```mathematica
In > eFlip[{False, c[{1, 2}, c[{1, 2}, c[{1, 2}, c[{1, 2}, n]]]]}]
Out> c[{1, 2}, c[{2, 1}, c[{1, 2}, c[{2, 1}, n]]]]
```

Generically, our `X` will act as an accumulator whose value will vary as one delves deeper into a structure. A more interesting application is this function which generates a list of numbers. In this case, our `G` is `1 + A`, our `F` is `1 + ℕ × A`, and our `X` will be `ℕ`.

```mathematica
rangeFmap[f_][n] := n
rangeFmap[f_][c[i_, x_]] := c[i, f[x]]

rangeCoalg[0] := z
rangeCoalg[n_] := s[n - 1]

rangeSig[{i_, z}] := n
rangeSig[{i_, s[x_]}] := c[i, {i + 1, x}]

range = 
  cHyloL[{type, rangeFmap, timesHom, rangeSig}, 
    # &, rangeCoalg];
```

```mathematica
In > range[{3, 5}]
Out> c[3, c[4, c[5, c[6, c[7, n]]]]]
```

The `cHyloR` case is a bit harder to reason about, but what's going on should be clearer if we make a similar range function to before.

```mathematica
range2Fmap[f_][z] := z
range2Fmap[f_][s[x_]] := s[f[x]]

range2Coalg[0] := z
range2Coalg[n_] := s[n - 1]

range2Sig[z] := n &
range2Sig[s[f_]] := Function[i, c[i, f[i + 1]]]

range2 = cHyloR[{type, range2Fmap, timesHom, range2Sig}, 
  # &, range2Coalg];
```
 
```mathematica
In > range2[5][3]
Out> c[3, c[4, c[5, c[6, c[7, n]]]]]
```

These functions are essentially the same. The main difference is how information is passed down the tree of the expression. In the first case, an actual value is propagated up the tree, and this value gets modified as you go. In the second, a request for a value is propagated up the tree, and this request is modified as you go. Both are sort of different ways of viewing the same functionality. I'm not sure there is a genuine difference between these two; they may simply be different formats for the same idea.

<a name="dp"></a>
## Dynamic Programming

A more substantial example of a recursion scheme arises out of the cofree comonad which is right adjoint to a forgetful functor.


...


<a name="ff"></a>
## Forgetful Functors

The histomorphism is a special case of a more generic scenario. If an algebra has a free functor, then it will be, by definition, right adjoint to the forgetful functor. This means, for any algebra with a free functor, we have the recursion scheme;

```agda
cHyloL : (F : Functor * *) (G : Functor Alg Alg)
         (σ : {y : Alg} → U (G y) → F (U y))
         {B : Alg} {A : *}
         (a : F A → A)
         (c : B → G B) →
         U B → A
```

As an example, if we take our algebra to be a monoid then...

...


<a name="ca"></a>
## Composing Adjunctions

Adjunctions can be composed in a rather obvious way. If we have two adjunctions which respectively state that;

```
(L1 a → b) = (a → R1 b)
(L2 b → c) = (b → R2 c)
```

then we can further observe that

```
(L2 (L1 a) → c)
= (L1 a → R2 c)
= (a → R1 (R2 c))
```

This means that, if `L1 ⊣ R1` and `L2 ⊣ R2`, then `L2 ∘ L1 ⊣ R1 ∘ R2`. This new adjunction will be made up of two functions;

```agda
∘LR : (L2 (L1 a) → c) → (a → R1 (R2 c))
∘RL : (a → R1 (R2 c)) → (L2 (L1 a) → c)
```

We can implement this as;

```mathematica
adjComp[adj1_, adj2_] :=
  adj[
    adj1[["lr"]][adj2[["lr"]][#]] &,
    adj2[["rl"]][adj1[["rl"]][#]] &
  ]
```

This gives a clear method for creating new paradigms of recursion by simply composing old ones together. Consider the following program for calculating the minimal number of coins (in US currency) for making change;

```python
...
```

Notice that it simultaneously makes use of a cache in the style of dynamic programming while also being mutually recursive over several different functions.


...


<a name="fg"></a>
## Further Generalization

It's worth wondering if this whole construction could be generalized even further. One which isn't covered is Mendler-style recursion schemes. These were invented for scenarios where the type families aren't functors. The hylomorphism construction requires the existence of a functional map. What would we do if we had no such thing? It's not uncommon to find type families which aren't functional, for example `X ↦ (X → X)`. We must instead formulate an algebra or a coalgebra which modifies the hylomorphism along with its usual argument. In this way, the action of the fmap is absorbed into one of them.

```agda
cmHyloL : {X Y : Category} (F : X .C → X .C) (G : Y .C → Y .C)
          {L : Functor Y X} {R : Functor X Y}
          (α : Adjunction L R)
          (σ : {x : Y .C} → X .hom (F0 L (G x)) (F (F0 L x)))
          {B : Y .C} {A : X .C}
          (a : ∀ {x} → X .hom x A → X .hom (F x) A)
          (c : Y .hom B (G B)) →
          X .hom (F0 L B) A
cmHyloL {X} {Y} F G {L} {R} α σ a c = X .∘ (a (cmHyloL F G α σ a c)) (Lσ c) where
  Lσ : {x : Y .C} → (Y .hom x (G x)) → X .hom (F0 L x) (F (F0 L x))
  Lσ f = α ⌈ Y .∘ (α ⌊ σ ⌋) f ⌉

cmHyloR : {X Y : Category} (F : X .C → X .C) (G : Y .C → Y .C)
          {L : Functor Y X} {R : Functor X Y}
          (α : Adjunction L R)
          (τ : {x : X .C} → Y .hom (G (F0 R x)) (F0 R (F x)))
          {B : Y .C} {A : X .C}
          (a : X .hom (F A) A)
          (c : ∀ {x} → Y .hom B x → Y .hom B (G x)) →
          Y .hom B (F0 R A)
cmHyloR {X} {Y} F G {L} {R} α τ a c = Y .∘ (Rτ a) (c (cmHyloR F G α τ a c)) where
  Rτ : {x : X .C} → X .hom (F x) x → Y .hom (G (F0 R x)) (F0 R x)
  Rτ f = α ⌊ X .∘ f (α ⌈ τ ⌉) ⌋
```

Look closely at the type of the algebra, `a`, in `cmHyloL` and the coalgebra, `c`, in `cmHyloR`. Notice that they've become natural transformations which claim one can penetrate through a layer of either `F`s or `G`s without either being a functor. 

<a name="ft"></a>
## Final Thoughts

There are many, many adjunctions hanging around in mathematics. Most of them have not been considered in this context. What sort of paradigms might arise from axiomatic cohesion? Or ...? Or ...? I think there's a lot of low-hanging fruit here.

{% endraw %}
