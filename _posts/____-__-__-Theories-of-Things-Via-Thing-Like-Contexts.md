This post will be a short note on an apifiny I had with respect to the syntactical theories underlying CaTT and it's relatives. The theory CaTT is outlined in [this](https://arxiv.org/abs/1706.02866) paper, and a sibling theory in [this](http://www.lix.polytechnique.fr/~tbenjamin/monoidal.pdf) one. The basic premise of these theories is that every type in the theory is a weak ω-category. In that second paper, all types are monoidial weak ω-category. That's quite significant, and it represents one of the few working examples of something like a directed type theory; a "hott" topic nowadays. Recently, I've come to see the full apparatus of both theories, a method to make a type theory where the types are all examples of nearly any kind of abstract algebra, and I wanted to write it down somewhere. To spoil the procedure, we do the following to get a type theory of Xs;

1. Define a notion of context which is constructed using the same procedure as objects within an X, with judgmental equalities coinciding with the equations of X.
2. Define a notion of substitution into these contexts.
3. Assert a "coherence" construction allowing one to define operators out of these contexts.

And that's it. Rather than talk about full ω-categories, I'll start with something simpler. Let's make a type theory where every object is a magma. Objects within a magma can only be constructed in one way, via the multiplication operation. For step 1, we simply allow contexts to be constructed in the same way.

    ------------ for a fresh x
    ⊢ (x : *) ctx

    ⊢ Γ ctx    ⊢ Δ ctx   FV(Γ)∩FV(Δ)=∅
    -------------------------------
              ⊢ Γ ∘ Δ ctx

                      
    --------------   ------
    (x : *) ⊢ x : *    Γ ⊢ *

      Γ ⊢ A         Δ ⊢ A   
    ---------    ---------
    Γ ∘ Δ ⊢ A     Γ ∘ Δ ⊢ A

and that's the only rule for context formation. There are no equations in a magma in general, so I'll save that for other examples. In this theory, contexts are *almost* free magmas, but that additional requirement that contexts can't share free variables makes that not-quite true. Still, it's, perhapse, intuitively useful as thinking of contexts as being, in some sense, free Xs for these theories. In this case, contexts are basically just binary trees.

The next step is to define a notion of substitution from one context into another. This generally means that substitutions are built up the same way that contexts are, and a valid substitution must be in the same shape as the context being substituted into. `Δ ⊢ σ : Γ` means that the substitution `σ` runs from the context `Γ` to `Δ`. That is, given a term `A` well-formed in `Γ`, `A[σ/Γ]` should be well-fromed in `Δ`.

       Δ ⊢ t : *
    --------------
    Δ ⊢ t : (x : *)

    Δ ⊢ σ1 : Γ1      Δ ⊢ σ2 : Γ2
    --------------------------
        Δ ⊢ σ1 ∘ σ2 : Γ1 ∘ Γ2

I'm going to hand-wave some of this. I think the additional rules will be sufficiently intuitively obvious that they can be worked out by the interested reader, but it is rather tedious.

The final step is the coherences. There are two classes of rules we are to be concerned with. The first is the operation rules, and the second is the equality rules. First, the operation rules;

    Γ ⊢ps   Γ ⊢ A   FV(Γ) = FV(A)
    ---------------------------
             Γ ⊢op A

For example, `(x : *), (y : *) ⊢op *`. In this theory, all the operations have `*` on their right side. This will generally be the case for any zero-dimensional algebra (magmas, monoids, groups, etc.) More interesting things will apear when we look at the theory of categories.

The rule for equalties.

    Γ ⊢ps   Γ ⊢ A   Γ ⊢ B   FV(Γ) = FV(A)∪FV(B)
    ---------------------------------------
                Γ ⊢eq A = B

For example, we can conclude that `(x : *) ∘ (y : *) ⊢eq x = y`. This does not mean that `x = y` is true in that context, merely that it's well-formed as a statement. 

We introduce an operation, `coh`, which, given a substitution, `σ`, from a context `Γ` to a context `∆`, and a proof that `Γ ⊢eq E`, we have that `∆ ⊢ coh(σ) : E[σ]`. This allows us to derive all the generic equations and operations within a theory of magmas. We can define an internal notion of product via;

    prod x y = coh[x ∘ y]

noting that

    (x : *) ∘ (y : *) ⊢ coh[x ∘ y] : *

we can also define
 
    square x = coh[x ∘ x]

since `x ∘ x` is a valid substitution from `(x : *) ∘ (y : *)` to `(x : *)`. That is;

    (x : *) ⊢ x ∘ x : (x : *) ∘ (y : *)

We can define several different three-way products. For example;

    prod3 x y z = coh [(x ∘ y) ∘ z]

We can then derive the equality;

    ((x : *) ∘ (y : *)) ∘ (z : *) ⊢ coh [(x ∘ y) ∘ z] : prod (prod x y) z = prod3 x y z

essentially through the fact that 





prod3prod x y z : prod (prod x y) z = prod3 x y z
  = coh [[(x : *) (y : *)] (z : *)], ...


