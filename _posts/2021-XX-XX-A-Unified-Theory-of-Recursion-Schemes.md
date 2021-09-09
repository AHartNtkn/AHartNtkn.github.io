{% raw %}

This post will be a tutorial on something called the conjugate hylomorphism, described in this paper;

- ["Conjugate Hylomorphisms Or: The Mother of All Structured Recursion Schemes"](http://www.cs.ox.ac.uk/people/nicolas.wu/papers/Hylomorphisms.pdf) by Ralf Hinze, Nicolas Wu, and Jeremy Gibbons

Personally, I found the paper quite difficult to read at first, and it only became clear to me what was going on by the time I formalized much of the paper's contents. This post will explan things in a way which it hopefully easier to learn from.

I will also assume you have some basic familiarity with recursion schemes; specifically that you know what a hylomorphism is. I did write [a post](http://anthonylorenhart.com/2020-04-07-Sorting-Lists-Recursively;-The-Easy-Way/) about recursion schemes a while ago which covers everything you need to know for this post, but there are tons of tutorials on the topic which will work equaliiy well. I don't expect anything beyond the basics.

First, a breif review; a hylomorphism, `h : B → A`, over an algebra `a : F A → A` and a coalgebra `c : B → F B` will be a fucntion satisfying `h = a ∘ F(h) ∘ c`. Intuitively, at each recursive step `c` is building up a single layer of the type `Fix F`, `h` is then being mapped through that layer where the next recursive step will occure, and finally `a` destructs that layer to get the output. This describes an extremely general pattern for recursion which covers many special cases, such as both merge and quicksort.

As general as hylomorphisms are, they don't seem to, at first, cover everything we want. If we have a mutually recursive function, such as;

```
even 0 = True
even n = not (ndd (n-1))

odd 0 = False
odd n = not (even (n-1))
```

there isn't an obvious way to extract a coalgebra and an algebra; after all it looks like there are two separate recursions going on simultaniously.

Similarly, if we have something stateful going on, like a cache which is used to store old calculations for reuse as in dynamic programming/memoization, something beyond a hylomorphism seems to be going on.

Those and several other things lead to the creation of a family of a dosen or so different recursion schemes each of which cover some special kind of recursion. After this started getting messy, various attempts were made to unify them to varying degrees of success. The conjugate hylomorphism is essentially the final form of this line of research, unifying everything in a way that's extremely neat and conceptually elegant.

As an inital summary, the premise is that all the different recursion schemes are, in fact, ordinary hylomorphisms. However, the algebra or coalgebra is in a different category than our category of types. This other category has an adjunction with our category of types, and by composing with the components of this adjunction we can get a (co)algebra back in our category of types that we can then use in a garden variety hylomorphism. The reason why these other recursion schemes are not obviously hylomorphisms is because their (co)algebras are complicated from the view within our category of types; by decomposing them into an adjunction and a (co)algebra in a different category, things become clearer.

Hopefully that wasn't too abstract to be confusing. To make things more concrete, I'll start putting down my Agda formalization of these ideas. First, as a warm-up, let's look at regular hylomorphisms. Since they all rely on the notion of functor, we must define categories and functors first.

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

As two simple examples, we can define our category of types, the product category, and a functor between them;

```agda
Type : Category
Type = record { C = Set
              ; hom = λ x y → (x → y)
              ; 𝟙 = λ x → x
              ; ∘ = λ f g x → f (g x) }
```

I'm going to be eliding the equations throughout this post since they're tedious and distracting. If I wanted to do a proper, full formalization, I'd obviously include them, but they won't help a reader so they'll be missing throughout this post.

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

With those in place, we can implement all three of our basic recursion schemes rather easily.

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

The main complication in constucting our function is the manner of translation between the two categories. If the (co)algebra exists in a different category from our target, its endofunctor will be over the other category. This means we need a way to translate between the two. This requires a natural transformation which turns one endofunctor into another by swapping it with one of the functors involved in the adjunction so as to keep the types consistent.

It's also worth pointing out that our target category can be on either side of the adjunction. Depending on where it is, we are forced to use one of two different implementation. This means we get two different hylomorphisms whose setup is identical but whose target is different.

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

As you can see, we have `Lσ` or `Rτ` which use the adjunction and natural transformation to convert the (co)algebra in the other category into our target. In theory, this natural transformation acts as an additional parameter, but in practice it tends to have little flexibility and its implementation is often forced on us by the type, which is why I didn't mention it in my earlier description.

This is all well and good, but we really need to buid intuition to get a handle on this. The rest of this post will just be a bunch of examples.

Since Agda's a bit onerous and I want to emphasize that this is a generic programming construct I'm going to swap to Mathematica; a language without any type system. I'll still do some informal type-theoretical reasoning, but that's not hard in practice. That's sort of the point of these recursion schemes. They isolate the neccessary reasoning into it's simplest components.

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

As a conjugate hylomorphism, it's adjunction is the trival adjunction between th eidentity functor and itself. The two components will just be the identity functions

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

On the other hand, if we use `cHyloR`, we'll be applying the natural transformation last, which means we'll be mapping over `X ↦ 1 + X` if we use it.

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

So that's all well and good, but let's look at less trivial examples. Namely, let's look at mutual recursion.





{% endraw %}
