This post consists of some notes on lambda encodings I made a while ago. I figured I'd put them here, since it's more visible and easier for me to find.

The free monoid monad is left adjoint to the forgetful functor (as are all free things). We can define the type of monoids as;

    record Monoid : * where
        Carrier : *
        _∙_ : Carrier → Carrier → Carrier
        𝟙 : Carrier

        ∙-assoc : (a b c : Carrier) → (a ∙ b) ∙ c = a ∙ (b ∙ c)
        ∙-𝟙-lcanc : (a : Carrier) → a ∙ 𝟙 = a
        ∙-𝟙-rcanc : (a : Carrier) → 𝟙 ∙ a = a

The forgetful functor is then a function that simply returns the carrier;

    ForgetMonoid : Monoid → *
    ForgetMonoid = Monoid.Carrier

which indicates that the free monoid can be encoded as;

    FreeMonoid : * → * 
    FreeMonoid A = (m : Monoid) → (A → ForgetMonoid m) → ForgetMonoid m

We can derive the various list-like properties of the free monoid, and, if we have function extentionality, we can prove they have the correct properties;

    nil : FreeMonoid A
    nil m f = 𝟙

    _∷_ : A → FreeMonoid A → FreeMonoid A
    _∷_ a xs m f = f a ∙ xs m f

    _++_ : FreeMonoid A → FreeMonoid A → FreeMonoid A
    _++_ l1 l2 m f = l1 m f ∙ l2 m f

    ++-assoc : (a b c : FreeMonoid A) → ((a ++ b) ++ c) = (a ++ (b ++ c))
    ++-assoc a b c = funext (λ m → funext (λ f → ∙-assoc m _ _ _))

    ++-nil-lcanc : (a : FreeMonoid A) → (a ++ nil) = a
    ++-nil-lcanc a = funext (λ m → funext (λ f → ∙-𝟙-lcanc m _))

    ++-nil-rcanc : (a : FreeMonoid A) → (nil ++ a) = a
    ++-nil-rcanc a = funext (λ m → funext (λ f → ∙-𝟙-rcanc m _))

This should allow one to define (weakly) free version of just about any datastrucure, including things like fields which don't traditionally have free generations.


Codata can be encoded using coalgebras. The technique was first introduced in G. C. Wraith's, "A note on categorical datatypes", who's definition of streams was reprinted [here](http://www.cs.ru.nl/~herman/PUBS/ChurchScottDataTypes.pdf).

More generically than presented there, if we have a coalgebra;

    CoAlgebra : (* → *) → * → *
    CoAlgebra f s = s → f s

then we can get the datatype that's the cofixed-point of that datatype via;

    CoFix : (* → *) → *
    CoFix f = ∀ a . (∀ s . s → CoAlgebra f s → a) → a

If you have a decent understanding of variance, then you may be able to use it to see what's going on here. When you are inside of a contravariant argument position (such as the first argument to →), then you're effectively working from the perspective of the opposite category. As a result all our left adjoints become right adjoints, and vice versa. As an example, the product's adjointness property now looks like

    X → A ⨯ B ≃ (X → B) → A

Which tells us that `(→ B) ⊣ (⨯ B)`, the reverse from before. In general, our fixpoint definition (from the first paper I mentioned) is;

    ∀ a . Algebra f a → a

But in the opposite category, that becomes,

    ∀ s . s → CoAlgebra f s

Feeding from our example, we can see an alternative method for defining the product. Consider that, in the opposite category, products become coproducts, and vice versa. If we take the coproduct definition, reverse the direction of all the arrows, we can use that to define the ordinary product;

    _×_ : * → * → *
    A × B = ∀ X . (∀ S . S → (S → A) → (S → B) → X) → X

    π₁ : A × B → A
    π₁ p = p (λ seed pa pb . pa seed)

    π₂ : A × B → B
    π₂ p = p (λ seed pa pb . pb seed)

There are also four distinct ways of making pairs, but it's pretty obvious that they're all extensionally equivalent.

    _,₁_ : A → B → A × B
    a ,₁ b = λ z . z a (λ _ . a) (λ _ . b)

    _,₂_ : A → B → A × B
    a ,₂ b = λ z . z b (λ _ . a) (λ _ . b)

    _,₃_ : A → B → A × B
    a ,₃ b = λ z . z a (λ y . y) (λ _ . b)

    _,₄_ : A → B → A × B
    a ,₄ b = λ z . z b (λ _ . a) (λ y . y)

Continuing to a more conventional example, streams can be defined as;

    StreamCoAlg : * → * → *
    StreamCoAlg A L = A × L

    Stream : * → *
    Stream A = CoFix (StreamCoAlg A)

    head : Stream A → A
    head st = st (λ seed step . π₁ (step seed))

    tail : Stream A → Stream A
    tail st = λ s . st (λ seed step . s (π₂ (step seed)) step)

And, as a final example, based on [this](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=9A564F2172717230E15D3F8EC5253423?doi=10.1.1.47.5204&rep=rep1&type=pdf), the interval of reals `[0,1)` can be encoded as;

    RealCoAlg : * → *
    RealCoAlg L = ℕ × L

    Reals : *
    Reals = CoFix RealCoAlg

Though, I haven't found a nice way of actually programming with it.

This basic technique should be able to be combined with the encodings used in [Cedille](http://firsov.ee/impred-ind/impred-ind.pdf) to get coinduction. It took a while for me to see a reasonable way it could be done, but the following might work. It's important to note that codata are principally defined by their destructors. In the case of streams, we can define it using a simple unfold;

    unfold : S → (S → X × S) → Stream X
    unfold seed gen = λ f . f {S} seed gen

If we want to define a coinduction principle, there's only one case that needs to be addressed; the single unfold destructor. 

    streamCoInd = (s : Stream X) → ∀ (P : Stream X → *) . (∀ S . (seed : S) → (gen : S → X × S) → P (unfold seed gen)) → P s

From here, we can take the dependent intersection of the original stream type with this coinduction principle to get a notion of stream with dependent elimination.

    StreamD X = ι(s : Stream X) . streamCoInd s
