{% raw %}
This post will be a relatively short note on a simple self-interpreter for the lambda calculus I recently made. I've been thinking of making an extensive work, perhaps a book, on programming in the lambda calculus. As part of that, I was looking into self-interpretation. If one wants to do meta-programming in the lambda calculus, one has to model it within itself. There are lots of ways of doing this, but the conceptually simplest is to use higher-order abstract syntax. This essentially re-uses the already existing binding and just puts a wrapper around it to mark the location of lambdas. This allows us to vary our evaluation scheme without needing to reinvent substitution.

Before I get started; here's a bare-bones implementation of a lazy evaluator for the de Bruijn-indexed lambda calculus in Mathematica;

```mathematica
quote[n_, x_Integer] := x + 1 /; x >= n;
quote[n_, λ[x_]] := λ[quote[n + 1, x]];
quote[n_, y_[x_]] := quote[n, y][quote[n, x]];
quote[n_, l_] := l;

sub[e_, n_, x_Integer] := If[x >= n, If[x == n, e, x - 1], x];
sub[e_, n_, λ[x_]] := λ[sub[quote[0, e], n + 1, x]];
sub[e_, n_, y_[x_]] := sub[e, n, y][sub[e, n, x]];
sub[e_, n_, l_] := l;

eval[a_] := spine[a, {}]

spine[λ[k_], {e1_, x___}] := spine[sub[e1, 0, k], {x}]
spine[a_[b_], {x___}] := spine[a, {b, x}]
spine[λ[l_], {}] := λ@eval@l
spine[l_, e_] := fold[l, e]

fold[x_, {y___, b_}] := fold[x, {y}][eval@b]
fold[x_, {}] := x
```

I'll use this implementation to test my programs written in the lambda calculus proper. As an example, here's an evaluation involving the s and k combinators.

```mathematica
k = λ[λ[1]];
s = λ[λ[λ[2[0][1[0]]]]];
```

```mathematica
In > eval[s[k][k][x]]

Out> x
```

Encoding lambda expressions within the lambda calculus isn't hard. From;

- ["A self-interpreter of lambda calculus having a normal form"](http://people.dm.unipi.it/berardu/Art/1992Self-interpreter/efficient.pdf) by Berarducci and Bohm
- ["Efficient Self—Interpretation in Lambda Calculus"](https://www.researchgate.net/publication/2673832_Efficient_Self-Interpretation_in_Lambda_Calculus) by Torben Mogensen

we can define a simple HOAS encoding of lambda terms into the lambda calculus that I'll implement in a bit. Through this encoding, we can directly access the syntax of a program. We can also define an evaluator/unquote function but this function will return a bottom-expression; not another HOAS-encoded expression. This means we lose the ability to reason about the syntax of the expression after unquoting. So we need a self-interpreter that will evaluate the expression without unquoting it.

Typically, an encoding like the one we want would be terms of type

```
∀X. (X → X → X) → ((X → X) → X) → X
```

This essentially defines HOAS terms in terms of their folds. This works perfectly well, but it's not too convenient as it has linear-time destructors. Unlike other types, perhaps, we will be frequently interlacing constructors and destructors. As a consequence, it's better to look at our encoding as an iterated polynomial functor;

```
μX. (X × X) + (X → X)
```

Application is the first component of the coproduct; a product encoded as;

```
app = λx y. λa l. a x y
```

Lambda expressions are the second component, encoded as

```
lam = λf. λa l. l f
```

We can implement them as

```mathematica
app = λ[λ[λ[λ[1[3][2]]]]];
lam = λ[λ[λ[0[2]]]];
```

As examples, here's what the HOAS encoded s and k combinators look like.

```mathematica
ek = lam[λ[lam[λ[1]]]];
es = lam[λ[lam[λ[lam[λ[app[app[2][0]][app[1][0]]]]]]]];
```

We can use this encoding to define a simple lazy interpreter using a typical spine-stack evaluator;

```
eval (app x y) l   = eval x (y : l)
eval (lam f) (x:l) = eval (f x) l
eval (lam f) nil   = lam f
```   

If you've seen lazy evaluators before (such as the one I gave near the beginning of this post), you'd expect this to be mutually recursive with another function that refolds the spine of the expression, but the reuse of the lambda binder makes that redundant. The only situation in which the fold would be called is one where the list of spine elements is empty and hence there's nothing to fold.

We can consolidate the cases by eliminating lambda-encoded lists;

```
eval (app x y) l = eval x (y : l)
eval (lam f) l   = l (lam f) (λx l. eval (f x) l)
```

And we can further consolidate by eliminating on lambda-encoded lambdas.

```
eval a l =  
   a (λx y. eval x (y : l))
     (λf. l (lam f) (λx l. eval (f x) l))
```

If we define

```
F = λe. λa. λl. 
      a (λx y. e x (y : l))
        (λf. l (lam f) (λx l. e (f x) l))
```

Then we can define

```
eval = F eval
```

This makes `eval` a simple instance of the y combinator; specifically, we can define `eval` to be

```
eval = Y F
```

We can implement the y combinator as

```mathematica
Y = λ[λ[1[0[0]]][λ[1[0[0]]]]]
```

Since we are doing simple list manipulation, we need to define the basic list constructors.

```mathematica
nil  = λ[λ[1]];
cons = λ[λ[λ[λ[0[3][2]]]]];
```

We can then, with some care, define F from earlier

```mathematica
F = λ[λ[λ[
      1[λ[λ[4[1][cons[0][2]]]]]
       [λ[1[lam[0]][λ[4[1[0]]]]]]
    ]]]
```

We can clean up F a bit using some beta and eta-equivalences. We also need to feed it into a Y and feed that an empty list, finally getting our evaluator as;

```mathematica
ev = λ[Y[λ[λ[λ[
       1[λ[λ[4[1][λ[λ[0[2][4]]]]]]]
        [λ[1[λ[λ[0[2]]]][λ[4[1[0]]]]]]]
     ]]][0][nil]]
```

We can test this on a few simple examples. Here, we encode k and s combinators in our HOAS encoding and evaluate `s k k k` to get `k`.

```mathematica
In[1] > ev[app[app[app[es][ek]][ek]][ek]] // eval
In[2] > ek // eval

Out[1]> λ[λ[0[λ[λ[λ[0[λ[3]]]]]]]]
Out[2]> λ[λ[0[λ[λ[λ[0[λ[3]]]]]]]]
```

Here, we encode k and s combinators in our HOAS encoding and evaluate `s k k s` to get `s`.

```mathematica
In[1] > ev[app[app[app[es][ek]][ek]][es]] // eval
In[2] > es // eval

Out[1]> λ[λ[0[λ[λ[λ[0[λ[λ[λ[
          0[λ[λ[λ[1[λ[λ[1[10][4]]]][λ[λ[1[7][4]
        ]]]]]]]]]]]]]]]]]
Out[2]> λ[λ[0[λ[λ[λ[0[λ[λ[λ[
          0[λ[λ[λ[1[λ[λ[1[10][4]]]][λ[λ[1[7][4]
        ]]]]]]]]]]]]]]]]]
```

The simplest self-interpreter I'm aware of before this was described here;

- ["John's Lambda Calculus and Combinatory Logic Playground"](https://tromp.github.io/cl/cl.html) by John Tromp.

The one there uses a de Bruijn encoding and is 206 bits when put in binary lambda calculus. We can define that a simple recursive function;

```mathematica
blc[λ[M_]]     := "00" <> blc[M]
blc[M_[N_]]    := "01" <> blc[M] <> blc[N]
blc[i_Integer] := StringJoin@Table["1", {i}] <> "0"
```

Encoding my self interpreter we find that it's only 115 bits.

```mathematica
In[1] > blc@ev
In[2] > % // StringLength

Out[1]> 0001010100010001100100000110010000000001011000000101111101
        000000101011011110000101100000010110000111110011000000010
Out[2]> 115
```

Of course, there's a problem with the interpreter I just gave. It only evaluates things into weak head normal form. To get a full normal form, we need to have a marker for variable positions to act as a base case. Something like

```haskell 
spine (app a b) l   = spine a (b:l)
spine (lam f) (a:l) = spine (f a) l
spine (lam f) []    = lam (λ x . spine (f (var x)) [])
spine (var x) l     = folda x l

folda x (b:l) = folda (app x (spine b [])) l
folda x [] = x
```

Note that the encodings for particular programs aren't really changing. `var` should ONLY be added by the evaluator; the programmer should never place them deliberately within an expression. The evaluator artificially adds `var`s as tags when recursing on an abstraction so it knows where to stop. The nature of bound variables is their black-box nature, meaning a program can't tell if it's looking at a variable if it isn't tagged as such. Since we now have three cases, the constructor encodings must change accordingly;

```mathematica
app = λ[λ[λ[λ[λ[2[4][3]]]]]];
lam = λ[λ[λ[λ[1[3]]]]];
var = λ[λ[λ[λ[0[3]]]]];
```

The evaluator can be turned from a mutually recursive function to an ordinary recursive one by noting that `folda` is only ever called on `var`.

```haskell
spine (app a b) l   = spine a (b:l)
spine (lam f) (a:l) = spine (f a) l
spine (lam f) []    = lam (λ x . spine (f (var x)) [])
spine (var x) (a:l) = spine (var (app x (spine a []))) l
spine (var x) []    = x
```

By performing similar transformations as before, we can redefine `spine` as

```haskell
spine = (λs e l.
            e (λa b. s a (b:l)) 
              (λf. l (lam (λx. s (f (var x)) [])) 
                     (λa l. s (f a) l)) 
              (λx. l x (λa l. s (var (app x (s a []))) l))
        ) spine
```

by shoving this into a y combinator and making similar modifications as before, we get a full normal form evaluator.

```mathematica
evn = 
 λ[Y[λ[λ[λ[
       1[λ[λ[4[1][λ[λ[0[2][4]]]]]]]
        [λ[1[λ[λ[λ[1[λ[7[4[λ[λ[λ[0[3]]]]]][nil]]]]]]]
            [λ[4[1[0]]]]]]
        [λ[1[0][λ[4[λ[λ[λ[0[λ[λ[λ[2[7][10[6][nil]]]]]]]]]]]]]]
       ]
  ]]][0][nil]]
```

```mathematica
In[1] > evn[lam[λ[app[app[app[es][ek]][ek]][ek]]]] // eval
In[2] > lam[λ[ek]] // eval

Out[1]> λ[λ[λ[1[λ[λ[λ[λ[1[λ[λ[λ[λ[1[λ[4]]]]]]]]]]]]]]]
Out[2]> λ[λ[λ[1[λ[λ[λ[λ[1[λ[λ[λ[λ[1[λ[4]]]]]]]]]]]]]]]
```

Note that `es` and `ek` are different from before since `app`, etc. have different definitions; though both have the same definition otherwise.

This new evaluator isn't quite as terse being 233 bits in BLC, but it's still pretty small. More importantly, unlike most other interpreters, this should work quite well in languages like interaction combinators where the method of binding is completely different from any standard lambda calculus.

The reason I wanted to write a self interpreter was to write programs that can reason effectively about the syntax of lambda expressions. This is essential for type-checking and program synthesis. As part of the project I mentioned at the beginning of this post would be a section on implementing proof and program search a la MiniKanren but in the pure lambda calculus.

{% endraw %}
