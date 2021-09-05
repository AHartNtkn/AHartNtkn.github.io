My viewpoint on calculus, specifically differentiation, has changed rather radically recently. I learned about differential forms and how to manipulate them algebraically. They have the following properties;

```
𝕕c = 0 where c is a constant
𝕕(x + y) = 𝕕x + 𝕕y
𝕕(x y) = 𝕕x y + x 𝕕y
```

So, basically the product rule and linearity. To give a simple example;

```
if z = x² + 2xy + y
then 
𝕕z = 𝕕(x² + 2xy + y)
   = 𝕕(x²) + 𝕕(2xy) + 𝕕y
   = 𝕕(x x) + 𝕕(2xy) + 𝕕y
   = 𝕕x x + x 𝕕x + 𝕕(2xy) + 𝕕y
   = 2x 𝕕x + 𝕕(2xy) + 𝕕y
   = 2x 𝕕x + 2 (𝕕x y + x 𝕕y) + 𝕕y
   = 2x 𝕕x + 2y 𝕕x + 2x 𝕕y + 𝕕y
   = (2x + 2y) 𝕕x + (2x + 1) 𝕕y
```

The two coefficients of `𝕕x` and `𝕕y` are the partial derivatives; specifically;

```
∂x z = 2x + 2y
∂y z = 2x + 1
```

If we fix a particular `x` and `y` and treat `𝕕x` and `𝕕y` as variables, then `𝕕z` is a formula for a plane; the linear approximation of `z` at our chosen `x` and `y`.

That `𝕕` will produce a linear approximation is a straightforward inductive argument.

Assume we have an expression, `e`, which is made up solely of additions, multiplications, constants, and variables `x1, x2, ...`, but nothing of the form `𝕕xn`. There are four cases;

1. If `e` is a variable `xn` then `𝕕e` will be `𝕕xn`, which, taken as a function of `𝕕xn` is a linear approximation of `xn` with coefficient/derivative 1.
2. If `e` is a constant then `𝕕e` will be 0; a linear approximation of a constant.
3. If `e` is of the form `e1 + e2` then `𝕕e` will be `𝕕e1 + 𝕕e2`. `𝕕e1` and `𝕕e2` will be a linear combination of `𝕕xn`s by the inductive hypothesis and this will remain true when both are added together.
4. If `e` is of the form `e1 e2` then `𝕕e` will be `𝕕e1 e2 + e1 𝕕e2`. `𝕕e1` and `𝕕e2` will be a linear combination of `𝕕xn`s by the inductive hypothesis. We also assumed that `e2` and `e1` don't contain any `𝕕xn`s, so `𝕕e1 e2` and `e1 𝕕e2` are also linear combinations of `𝕕xn`s, as is their sum.

The coefficient of `𝕕xn` within `𝕕e` is the partial derivative with respect to `xn` of `e`.

That's quite remarkable. The usual story about derivatives involves limits. For a while, there have been efforts to bring calculus back to its roots and use infinitesimals instead as they are easier to manipulate algebraically. However, this derivation illustrates something important; we don't need either! Taking a derivative has nothing to do, in principle, with anything related to continuity; it's a side effect of the simplest nontrivial method of algebraically defining a linearization operation. There is absolutely no reason to interpret `𝕕x` as an "infinitesimal change in `x`"; it's completely superfluous. So all the arguments about philosophical whatever completely dissolve! This is also less complicated than accounts involving infinitesimals; perhaps not as much of a simplification as removing limits, but still an improvement.

This is also much more general as this can be done over any rig; anything with an addition and multiplication which distribute over each other. This includes completely discrete domains; we don't need a domain where limits or infinitesimals need to be defined. 

Integration, in so far as it's merely an approximate inverse of differentiation, is also perfectly well defined in discrete domains. Or, at least it's as well defined as it usually is; up to a constant and whatnot. Though many specific integrals are undefined in finite domains. Take the finite field with two elements, for instance. In that domain, we have that `x² = x` for all `x`. Up to eta-equivalence, there are only four unary functions;

```
f x = 1
f x = 0
f x = x
f x = x + 1
```

Their derivatives are `0`, `0`, `1`, and `1`, respectively. As a consequence, `f x = x` and `f x = x + 1` don't have integrals in this domain since nothing differentiates to them. But we know this because the concept of an integral is perfectly sensable even if it's not always defined. We can even perform definite integrals using the first fundamental theorem of calculus... sometimes. We need a notion of negation to do that, at least, so it can't be done in a general rig. There might be some other requirements, like some notion of order.

Anyway, this post is about lambda expressions. I've been trying to understand the differentiation of lambda expressions as part of a vague idea of general-purpose differential programming. I don't have a whole lot to write about that since there are a dozen different somewhat incompatible approaches, each with their own pluses. The usual narrative about these is to think not about lambda expressions themselves but an expanded calculus that includes addition and multiplication. Exactly what those mean will vary, but the most common interpretation is to think of `a + b` as a program that non-deterministically chooses between `a` and `b` with equal probability. The coefficients are then probabilistic weights, and the derivatives are, in some sense, the tangents of the higher-order probability distributions represented by the program.

That's all well and good, but I've not found much of that too convincing. I don't see where it comes from, fundamentally. Programs, even probabilistic ones, seem fundamentally discrete, and asserting continuity always seemed unnatural. This is a problem I have with probability theory in general. Probability distributions are often construed as functions from ℝ to ℝ satisfying some properties. But doing that makes no sense computationally; many computable distributions do not have computable probabilities (the universal distribution, for example), and lambda expressions with a choice operator and no coefficients are already Turing complete for all computable distributions. We can, and should, think of probabilities as programs that sample things randomly, and that doesn't necessitate continuity.

That's where I was a few months ago. Nowadays I don't think of derivatives as having anything fundamentally to do with continuity, so it's changed my perspective here too. Of course, the fundamental thing is no longer derivatives at all. Instead, it's the differential form. So, what do differential forms for general lambda expressions look like?

Let's look at the simple example;

```
f(x) = x²
```

We will have that

```
𝕕f(x, 𝕕x) = x 𝕕x
```

This highlights the important fact that the differential of a function is a function of both its original argument and the differential form of that argument. We can note that;

```
f = λx. x²
```

and that

```
𝕕f = λx. λ𝕕x. x 𝕕x
```

So we inevitably conclude that

```
𝕕(λx. f) = λx. λ𝕕x. 𝕕f
```

The other operator in the lambda calculus is application, but we literally already defined the differential of application as;

```
𝕕(f(x)) = 𝕕f(x, 𝕕x)
```

Using it, we can note that

```
𝕕(f(g(x))) = 𝕕f(g(x), 𝕕(g(x))) = 𝕕f(g(x), 𝕕g(x, 𝕕x))
```

A version of the chain rule.

And that's everything we need to calculate differential forms of lambda expressions! We don't even need to extend the calculus with addition and multiplication. As some examples;

```
𝕕(λx. x) = λx. λ𝕕x. 𝕕x
```

```
𝕕(λx. λy. x) = λx. λ𝕕x. 𝕕(λy. x) 
             = λx. λ𝕕x. λy. λ𝕕y. 𝕕x
```

```
𝕕(λf. λx. f(x))
= λf. λ𝕕f. 𝕕(λx. f(x))
= λf. λ𝕕f. λx. λ𝕕x. 𝕕(f(x))
= λf. λ𝕕f. λx. λ𝕕x. 𝕕f(x, 𝕕x)
= λf. λ𝕕f. λx. 𝕕f(x) by eta-reduction
= λf. λ𝕕f. 𝕕f by eta-reduction
```

which is the same derivative as `λx. x`. Appropriate, since they are eta-equivalent.

```
𝕕(λf. λg. λx. f(x, g(x)))
= λf. λ𝕕f. 𝕕(λg. λx. f(x, g(x)))
= λf. λ𝕕f. λg. λ𝕕g. 𝕕(λx. f(x, g(x)))
= λf. λ𝕕f. λg. λ𝕕g. λx. λ𝕕x. 𝕕(f(x, g(x)))
= λf. λ𝕕f. λg. λ𝕕g. λx. λ𝕕x. 𝕕((f(x))(g(x)))
= λf. λ𝕕f. λg. λ𝕕g. λx. λ𝕕x. 𝕕(f(x))(g(x), 𝕕(g(x)))
= λf. λ𝕕f. λg. λ𝕕g. λx. λ𝕕x. 𝕕f(x, 𝕕x)(g(x), 𝕕g(x, 𝕕x))
= λf. λ𝕕f. λg. λ𝕕g. λx. λ𝕕x. 𝕕f(x, 𝕕x, g(x), 𝕕g(x, 𝕕x))
```

Neat. But, it's worth noting that we don't really get a derivative out of this. Remember, the derivative is a coefficient of a differential form as part of a linear combination. We don't get linear combinations here since we don't even have addition. Well, that's not entirely true. In the `𝕕(λx. λy. x)` case, for example, we can interpret `𝕕x` as `1 𝕕x + 0 𝕕y`, and read, entirely correct, derivatives from that. But that's pretty loosey-goosey and we definitely can't do that in more realistic scenarios.

The main utility of differential programming is the ability to use gradient descent to do a form of program synthesis. This gives us a differential form in computationally generic domains and a derivative in some discrete domains. However, this doesn't give any obvious discrete form of gradient descent. I suspect such a thing does exist, but this doesn't make such a thing clear. I may be wrong though and we might need to seek out a special case of what I've presented here where continuity persists. Something like quasi-Borel spaces which have already been used for higher-order probabilistic programming.

But that's all I have for now. Bye!
