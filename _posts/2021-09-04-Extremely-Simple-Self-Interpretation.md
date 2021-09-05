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

If you've seen lazy evaluators before (such as the one I gave near the beginning of this post), you'd expect this to be mutually recursive with another function that refolds the spine of the expression, but the reusage of the lambda binder makes that redundant. The only situation which the fold would be called is one where the list of spine elements is empty and hence there's nothing to fold.

We can consolidate the cases by eliminating lambda-encoded lists;

```
eval (app x y) l = eval x (y : l)
eval (lam f) l   = l (lam f) (λx l. eval (f x) l)
```

And we can further consolidate by eliminating on lambda-encoded lambdas.

```
eval a = 
   a (λx y. λl. eval x (y : l))
     (λf. λl. l (lam f) (λx l. eval (f x) l))
```

If we define

```
F = λe. λa. 
      a (λx y. λl. e x (y : l))
        (λf. λl. l (lam f) (λx l. e (f x) l))
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
F = λ[λ[
     0[λ[λ[λ[4[2][cons[1][0]]]]]]
      [λ[λ[0[lam[1]][λ[λ[5[3[1]][0]]]]]]]
    ]]
```

We can clean up F a bit using some beta and eta-equivalences. We also need to feed it into a Y and feed that an empty list, finally getting our evaluator as;

```mathematica
ev = λ[Y[λ[λ[
            0[λ[λ[λ[4[2][λ[λ[0[3][2]]]]]]]]
             [λ[λ[0[λ[λ[0[3]]]][λ[4[2[0]]]]]]]
         ]]
        ][0][nil]]
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

The simplest self-interpreter I'm aware of prior to this was described here;

- ["John's Lambda Calculus and Combinatory Logic Playground"](https://tromp.github.io/cl/cl.html) by John Tromp.

The one there uses a de Bruijin encoding and is 206 bits when put in binary lambda calculus. We can define that a simple recursive function;

```mathematica
blc[λ[M_]]     := "00" <> blc[M]
blc[M_[N_]]    := "01" <> blc[M] <> blc[N]
blc[i_Integer] := StringJoin@Table["1", {i}] <> "0"
```

Encoding my self interpreter we find that it's only 117 bits.

```mathematica
In[1] > blc@ev
In[2] > % // StringLength

Out[1]> 00010101000100011001000001100100000001010000000010111110110
        0000010101110110000001010000001011100001111100111000000010
Out[2]> 117
```

The reason I wanted to write a self interpreter was to write programs that can reason effectively about the syntax of lambda expressions. This is essential for type-checking and program synthesis. As part of the project I mentioned at the beginning of this post would be a section on implementing proof and program search a la MiniKanren but in the pure lambda calculus. I'm not sure if it's possible using this encoding since equality between HOAS expressions isn't decidable, but there may be a trick around that.

{% endraw %}
