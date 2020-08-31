I've known for a while that dependent types can be defined as initial and final dialgebras. This is a somewhat obscure fact; but an important generalization of the more commonly referenced fact that non-dependent types are initial and final (co)algebras of specific endofunctors. The only thorough source of this fact, however, is ["Dependent Inductive and Coinductive Types are Fibrational Dialgebras"](https://arxiv.org/pdf/1508.06779.pdf). That paper has been cited only once, by a followup from the same author implementing a programming language. That language itself has influenced a few more projects in dependent type theory, but the original, more theoretical, result remains obscure.

I didn't understand it the first few times I read it. I read it again, probably for the fourth time now, and I finally feel I have a grasp on what it's saying, and this post is meant to express the basic point in my own words. This note will be a bit meandering.

I won't explain what a dialgebra is. Look it up on the nlab. Did you do that? Good. Now that you've seen the definition, let's define vectors as an (F, G)-dialgebra. Essentially, F will have all the information for the input requirements and G will have the output requirements. The dialgebra itself expresses the existence of a structure-preserving transformation from F Xs to G Xs; from inputs to outputs.

Here's the first example; vectors have two constructors;

```
Vect A n
  nil  :       1            → Vect A 0
  cons : ∀ n . A × Vect A n → Vect A (n + 1)
```

So F has to say that the inputs are expecting something either trivial or a pair with an A and a vector. G will say that the outputs are expected to be a vector of length zero or a vector of length one greater than the input. Putting this together, we can say,

`Vect A n` is the initial dialgebra over the functors;
```
  F, G : (ℕ → *) → * × (ℕ → *)
  F = X ↦ (1,   λ k . A × X k)
  G = X ↦ (X 0, λ k . X (k + 1))
```

The types are important. We're defining something that's dependent on the natural numbers, so the inputs must be functions from ℕ to our type universe. The outputs of our functors are going to match the dependencies of the input and output, with one entry in the product for each constructor. The first constructor, `nil`, doesn't depend on anything, so it's just `*`. `cons`, on the other hand, depends on a natural number. This means our functors are taking functions and returning other functions. This is what "fibrational" means in the original paper. The "dialgebra" part basically asserts that we need to make a morphism between F(X) and G(X), for any X. Since the outputs are pairs, this will generally be pairs of morphisms; one for each constructor, mapping inputs to outputs.

Let's move onto a second example; finite sets. `Fin n` has two constructors;
```
Fin n
  zero :       1     → Fin (n + 1)
  succ : ∀ n . Fin n → Fin (n + 1)
``` 
And we can perform the exact same construction to get a dialgebraic characterization;

`Fin n` is the initial dialgebra over the functors
```
  F, G : (ℕ → *) → (ℕ → *) × (ℕ → *)
  F = X ↦ (λ _ . 1, X)
    = X ↦ (const 1, X)
  G = X ↦ (λ k . X (k + 1), λ k . X (k + 1))
    = X ↦ Δ (X ∘ (+1))
```

we can give a few obvious alternative definitions by altering the functors;

```
  F, G : (ℕ → *) → ℕ + ℕ → *
  F = X ↦ [λ _ . 1, X]
  G = X ↦ [λ k . X (k + 1), λ k . X (k + 1)]
```
```
  F, G : (ℕ → *) → Bool × ℕ → *
  F = X ↦ λ (b, k) . if b then 1 else X k
  G = X ↦ λ (_, k) . X (k + 1)
```

It's worth looking at a trivial case. We have the type of natural numbers
```
ℕ
  zero : 1 → ℕ
  succ : ℕ → ℕ
```

ℕ is then the initial dialgebra over the functors
```
  F, G : * → * × *
  F = X ↦ (1, X)
  G = X ↦ (X, X)
    = Δ
```

ℕ is also the initial dialgebra over the functors
```
  F, G : * → *
  F = X ↦ 1 + X
  G = X ↦ X
```

I do wonder what the generic principal relating these is. I think it may be a peculiar property of the universe of types. What I can say is that morphisms from `(1, X)` to `(X, X)` will be pairs of morphisms from `1` to `X` and from `X` to `X`. This can be written as
```
(1 → X) × (X → X)
```
which is isomorphic to `1 + X → X`. So the connection, in this case, is obvious, but it doesn't seem to me like this reasoning would generalize if our constructors required types with more exotic kinds of morphisms than functions.

Codata is essentially the same construction, but the inputs and outputs are switched, and we take the final, rather than initial, dialgebra.

ℕ∞ is ℕ equipped with a point at infinity. It has the same constructors as ℕ, but it's codata.
ℕ∞ is the final dialgebra over the functors
```
  F, G : * → * × *
  F = X ↦ (X, X)
  G = X ↦ (1, X)
```

Another example; partial streams over A of length n : ℕ∞ are defined as
```
PStr A n
  hd : PStr A (succ n) → A
  tl : PStr A (succ n) → PStr A n
```

`PStr A n` is then the final dialgebra over the endofunctors,
```
  F, G : (ℕ∞ → *) → (ℕ∞ → *) × (ℕ∞ → *)
  F = X ↦ (const A, X)
  G = X ↦ Δ (X ∘ (+1))
```

The final example I'll give is pi types. As a coinductive type, they have one constructor;
```
Π (f : I → J) (B : I → *) : J → * 
  app : Π f B (f i) → B i
```

so, `Π f B` should be the final dialgebra over
```
  F, G : (J → *) → (I → *)
  F = X ↦ B
  G = X ↦ X ∘ f
```

Now, it's not clear to me how this definition relates to a more conventional presentation of Pi types. As a simple example, if I had 
```
λ b . bool_elim b refl refl : Π (b : Bool) . b = true ∨ b = false
```
how would I write this using the above characterization? This, incidentally, has nothing to do with dialgebras, so I'll move on.

One of my personal fixations is on impredicative encodings of datatypes, and I never fully grasped how to do them for dependent types. There's a close relationship between the initial algebra of an endofunctor characterization of types and their impredicative encodings. For example, ℕ is the initial algebra over `X ↦ 1 + X`, and we can give the impredicative encoding `∀ X . (1 + X → X) → X`. In general, initial algebras over an endofunctor `F` have the encoding `∀ X . (F X → X) → X`. That's pretty cool, and we can derive lambda encodings from them.

```
zero : ∀ X . (1 + X → X) → X
     = Λ X . λ f . f (inl tt)

succ : (∀ X . (1 + X → X) → X) → ∀ X . (1 + X → X) → X
     = λ n . Λ X . λ f . f (inr (n -X f))
```

It seemed obvious to me that understanding the dialgebraic approach to dependent types should lead to a good method for getting impredicative encodings. Let's start simple, with natural numbers. To reiterate;
ℕ is the initial dialgebra over the functors
```
  F, G : * → * × *
  F = X ↦ (1, X)
  G = X ↦ (X, X)
```

We can give other first-order impredicative encodings of ℕ as;
```
ℕ ≅ ∀ X . X → (X → X) → X
  ≅ ∀ X . X × (X → X) → X
  ≅ ∀ X . (1 → X) × (X → X) → X
```

That last formulation directly relates to the dialgebraic definition. We pair up the inputs and outputs, put a → between them, and we're golden. This comes from the observation that morphisms between pairs are exactly pairs of morphisms. Using this encoding, we have the lambda encodings

```
zero : ∀ X . (1 → X) × (X → X) → X
     = Λ X . λ f . fst f tt

succ : (∀ X . (1 → X) × (X → X) → X) → ∀ X . (1 → X) × (X → X) → X
     = λ n . Λ X . λ f . snd f (n -X f)
```

Let's try with vectors;

`Vect A n` is the initial dialgebra over the functors;
```
  F, G : (ℕ → *) → * × (ℕ → *)
  F = X ↦ (1,   λ k . A × X k)
  G = X ↦ (X 0, λ k . X (k + 1))
```

We can pair morphisms again, but then we'll need morphisms between the functions too. A morphism between functions `f, g : X → Y` is going to be something of type `∀ x : X . f x → g x`, so a product of morphisms in `Y`. Since `Y` is our universe of types, these will simply be functions.

```
Vect A n ≅ ∀ X : ℕ → * . (1 → X 0) × (∀ k : ℕ . A × X k → X (k + 1)) → X n
```

we can make this a bit easier to work with;
```
Vect A n ≅ ∀ X : ℕ → * . X 0 → (∀ k : ℕ . A → X k → X (k + 1)) → X n
```

and define the constructors as lambda expressions

```
nil : Vect A 0
    = Λ X . λ n c . n

cons : ∀ k : ℕ . A → Vect A k → Vect A (k + 1)
     = Λ k . λ a v . Λ X . λ n c . c -k a (v X n c)
```

this seems like a very reasonable encoding of vectors.

As an example, to encode `['a', 'b', 'c'] : Vect Str 3`;
```
  Λ X . λ  n c . c -2 'a' (c -1 'b' (c -0 'c' n))
```

Now this whole construction seems like it should have been obvious in retrospect.

We can use this to try getting an impredicative encoding with a dependent eliminator.
```
  ι(v : Vect A n) .
    ∀ (P : ∀ k : ℕ . Vect A k) .
    P -0 nil →
    (∀ (k : ℕ) (v : Vect A k) . Π (a : A) . P -k v → P -(k + 1) (cons -k a v)) →
    P -n v
```

Not too hard to work out. A more generic construction for dialgebras in general would have to address morphisms in general. However, the assertion that dependent types are *fibrational* dialgebras would seem to imply that any dependent type `T` over some other type `I` will all have base encodings of the form
```
   T (i : I) = ∀ X : I → * . (∀ i : I . F[0] i → G[0] i) × (∀ i : I . F[1] i → G[1] i) × ... → X i
```
which seems workable.