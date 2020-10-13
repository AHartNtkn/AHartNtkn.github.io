
...

I think something like the following should work; split up the (monoidal) canonical context into a contravariant and coveriant part. When calculating the source and target, the target becomes the source and vice versa on the contravariant part. If the contravariant part is denoted with `{`s, we might have;

```
[a, b] =
  op [{x : *}, [y : *]]
     [{a}, [b]]
     *

unit-law =
  coh [[x : *]]
      [...]
      [1, x] → x

bij-1 =
  coh [{x : *}, {y : *}, [z : *]]
      [...]
      [x ⨂ y, z] → [x, [y, z]]
...

hom-func-1 =
  op [{x : *}, [y : *, z : *, f : y → z]]
     [...]
     [x, y] → [x, z]

hom-func-2 =
  op [{x : *, y : *, f : x → y}, [z : *]]
     [...]
     [y, z] → [x, z]
...
```

however, this isn't able to capture the "wiring" which makes internal homs genuinely useful. How would one witness, for instance, the canonical `1 → [x, x]` for any `x`? This would suggest implementing some computational principal which cancels out the contra and covariant instances of repeated free variables in the type, but the calculation would have to be more sophisticated than the simple variable counting method presented thus far. We should not have a canonical map `[x ⨂ y, z] → [y, [x, z]]` since our tensor product isn't symmetric, though we can make it so by making our context a finite set instead of a list. As a consequence, variables shouldn't cancel if they're not in the correct position to do so. The above construction basically just defines a product that's contravariant in one argument. Such a thing might be useful when defining opposite categories, but it doesn't address the "hom" part of "internal hom". 

We may also extend the context with an extranatural component just to explicitly state this wiring. Using `(` to denote extranatural input-output pairs, we might be able to do;

```
j(a) =
  coh [(x : *)]
      [a]
      1 → [x-, x+]

L(x, b, c) =
  coh [(x : *), {y : *}, [z : *]]
      [b, c]
      [y, z] → [[x+, y], [x-, z]]

comm-1 =
  coh [(x : *), (y : *)]
      []
      j(y) ∘ L(x, y-, y+) → j([x, y])

i(a, b) =
  coh [{x : *}, [y : *]]
      [a, b]
      [x, y] → [1, [x, y]]

j1(a) =
  coh [(x : *), [z : *]]
      [a, _]
      [[x+, x-], z] → [1, z]

comm-2 =
  coh [(x : *), [y : *]]
      [...]
      i(x-, y) → L(x, x-, y) ∘ j1(x) ???
...
```
This may suggest a full rehauling for how variance is tracked in the first place. We might count, not variables, but occurrences of variables under each variance.  `FV` would then make sure the sources and targets just have the same counts for co and contra variance.

Another alteration we could do is change how the source and target of types are defined. We may count the source to include all contravariant arguments in the target and the target to include all the contravariant arguments in the source. We'd simply delete extranatural arguments when calculating free variables since they'd be wired to themselves and wouldn't be present in any kind of normal form. We might be able to simply have;

```
j(a) =
  coh []
      []
      1 → [a, a]

L(x, b, c) =
  coh [{y : *}, [z : *]]
      [b, c]
      [y, z] → [[x, y], [x, z]]

comm-1(x, y) =
  coh []
      []
      j(y) ∘ L(x, y, y) → j([x, y])
(Everything is extranatural here, but how would it know those center two ys should connect?)

i(a, b) =
  coh [{x : *}, [y : *]]
      [a, b]
      [x, y] → [1, [x, y]]

j1(a) =
  coh [[z : *]]
      []
      [[a, a], z] → [1, z]

comm-2 =
  coh [{x : *}, [y : *]]
      [...]
      i(x, y) → L(x, x, y) ∘ j1(x)
...
```

though, handling extranatural functoriality and actually writing down `FV` is something I don't know how to do.

