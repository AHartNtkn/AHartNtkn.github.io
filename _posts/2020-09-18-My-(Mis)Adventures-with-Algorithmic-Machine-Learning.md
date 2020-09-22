- [Introduction](#heading1)
- [Why Not Use Ordinary Compression?](#heading1p5)
- [Methods for Approximating Kolmogorov Complexity](#heading2)
- [Approximating Conditional Kolmogorov Complexity using CTM](#heading2p5)
- [Block Decomposition](#heading3)
- [Spit-balling ways to improve DBM and CTM](#heading4)
- [Algorithmic Loss](#heading5)
- [Algorithmic Optimization](#heading6)
- [Final Thoughts](#heading7)

<a name="heading1"></a>
## Introduction

I've been researching, for quite some time, the prospect of machine learning on a wider variety of data types than normally considered; things other than tables of numbers and categories. In particular, I want to do ML for program and proof synthesis which requires, at the very least, learning the structures of trees or graphs which don't come from a differentiable domain. Normal ML algorithms can't handle these; though some recent methods, such as graph neural networks and transformers, can be adapted to this domain with some promising results. However, these methods still rely on differentiation. Is this really required? Are we forever doomed to map all our data onto a differentiable domain if we want to learn with it?

An alternative approach that has been bandied about for a while is the utilization of compression. It's not hard to find articles and [talks](https://www.lesswrong.com/posts/hAvGi9YAPZAnnjZNY/prediction-compression-transcript-1) about the relationship between compression and prediction. If you have a good predictor, then you can compress a sequence into a seed for that predictor and decompress by running said predictor. Going the other way is harder, but, broadly speaking, if you have a sequence that you want to make a prediction on and a good compressor, then whichever addition increases the compressed size the least should be considered the likeliest prediction. This approach is quite broad, applying to any information which can be represented on a computer and not requiring any assumptions whatsoever about the structure of our data beyond that. We could use this idea to, for example, fill in gaps in graphs, trees, sets of input-output pairs, etc.

It's important to understand what's actually required here. We don't actually need to compress our training data; we only need a way to estimate the change in minimal-compression-size as we add a prediction. This minimal-compression-size is called the Kolmogorov Complexity, denoted `K(X)`. The minimal-compression-size of a program which outputs `X` on an input `Y` is called the Conditional Kolmogorov Complexity, denoted `K(X|Y)`. The topic of Kolmogorov Complexity is quite broad, and I won't explain all its complexities here. A standard introduction is the textbook [An Introduction to Kolmogorov Complexity and Its Applications](https://www.springer.com/gp/book/9781489984456) by Li, Ming, Vitányi, and Paul. If we have a good method for calculating `K`, then we don't need to actually make use of compression.

Making this practical is quite hard and under-researched, and there aren't many papers on the topic. But there is this;
  [Algorithmic Probability-guided Supervised Machine Learning on Non-differentiable Spaces](https://arxiv.org/abs/1910.02758)
which reproduces some standard ML applications using this approach. I want to understand how doing ML this way works, and this post will basically be a collection of the notes I made while reading the paper. If I refer to "the paper", "this paper", etc. in this post, this is what I'm referring to. These notes will digress quite often and I'm also quite critical of some aspects of the paper. This post was also written somewhat like a stream of consciousness, so I'll often say something which I correct later on. This post isn't intended to merely summarize the paper, but to describe what I learned and thought as I read it. Hopefully, you'll learn stuff too.

<a name="heading1p5"></a>
## Why Not Use Ordinary Compression?

One of the most common suggestions for approximating `K(X)` is to simply use an already existing compression algorithm. The problem is that most "optimal" compression algorithms such as arithmetic encoding are only optimal up to the Shannon Entropy of the data. That is if we assume the data is sampled randomly from a distribution, the best we can do is estimate the shape of this distribution and give shorter encodings appropriately to more likely symbols. This is, asymptotically, about the same as counting substring occurrences to reduce redundancy. If our data is actually just randomly sampled, then this is great! But the real world isn't like this. Most real-world data can be construed as an essentially deterministic process with some added noise. Most compression potential comes from modeling this underlying process, not the noise.

Consider the sequence;
```
1, 2, 3, 4, ..., 1000
```
This is, obviously, very compressible. An optimal (truly optimal, not Shannon entropy-optimal) compressor would be able to compress this into a program producing this output. Maybe `Range@1000`, or something even smaller, depending on what language it's using. But statistical compression will just try to find repetitive substrings. Even if we represent this list in binary and compress, statistical methods won't be able to compress this much better than a truly random string since there are few repetitious patterns.

There are lots of natural examples of this. Compressing the digits of π, compressing the coordinates of regular geometric figures, compressing a list of positions for a simple physical system simulation. It's obvious that these can have small algorithmic complexity, that they should be compressible into small programs that generate them, and yet statistical compression methods won't be able to take advantage of this.

As a consequence, we must use compression methods that do something more sophisticated than statistical compression. Unfortunately, essentially all general-purpose compression methods are like this. There are some ML-based methods that probably aren't. A lot of the text-compression algorithms which participated in the [Large Text Compression Benchmark](http://mattmahoney.net/dc/text.html) use RNNs which are definitely doing something other than statistical compression.

Much of the paper is dedicated to explaining one method for approximating Kolmogorov Complexity. Kolmogorov Complexity isn't computable, and getting a good approximation is very hard. Some clever methods have been devised, but we can't get a good grasp of it as easily as we can perform statistical compression.

<a name="heading2"></a>
## Methods for Approximating Kolmogorov Complexity

We, ultimately, need a way to approximate Kolmogorov complexity. The learning methods themselves should be largely independent of this, but choosing a method is essential for real-world applications. Here are a few methods I've found in the literature;

---

### CTM - Coding Theorem Method

This is the method the paper endorses so I'll talk about it in more detail later on.

The idea is to enumerate all strings of a given length and run them as programs for some chosen model. We collect all the input-output pairs to a database. We then use the coding theorem to justify using this to estimate `K`. In particular, if we add together `2^-l(p)`, where `l(p)` is the length of `p`, for all programs `p` which output `X`, that will get us an estimate for `K(X)`. This is basically what the coding theorem says, hence the name of the method.

#### BDM - Block Decomposition Method

This utilizes an existing CTM database to estimate Kolmogorov Complexity. It first tries finding algorithmically compressible substrings using CTM and then uses that information in conjunction with a Shannon entropy like calculation to estimate the complexity of the whole string. For small strings, BDM is close to the performance of CTM, for large strings its average-case performance is close to statistical compression. Many large strings in practice, however, tend to be compressed better than with statistical methods.

See:
  - [Numerical Evaluation of Algorithmic Complexity for Short Strings](https://arxiv.org/abs/1101.4795)
  - [A Decomposition Method for Global Evaluation of Shannon Entropy and Local Estimations of Algorithmic Complexity](https://arxiv.org/abs/1609.00110)

--- 

### List Approximation

List Approximation is based on optimizing a simple observation. While generating the smallest program generating `X` is not computable, generating a list guaranteed to contain the smallest program is. In particular, we can return a list enumerating all strings below and containing `X`. This will definitely have the smallest program generating `X`, but it will be exponentially large in the length of `X`. How small can this list be?

[Short lists with short programs in short time](https://arxiv.org/abs/1301.1547) (also an improved version in [Short lists for shortest descriptions in short time](https://arxiv.org/abs/1212.6104)) show that this list can be made quadratically large (and asymptotically no smaller) in the length of the input while guaranteedly containing the smallest program. This makes searching for `K(X)` much more practical as we only need to run a number of programs quadratic in the size of `X`.

If we are willing to accept only approximating `K` with a list, we can ensure an `O(log(|X|))` penalty to our smallest generating program and make the list linear in the size of `X`, as shown in [Linear list-approximation for short programs](https://arxiv.org/abs/1311.7278).

These methods seem promising, but the algorithms themselves are quite abstruse and some require exponential space, making them impractical. However, improvements may be possible.

It's unclear how much labor is actually saved when using the approximation lists. It may be the case that both the smallest possible representations of programs and everything else in the list requires an absurd amount of work to normalize. It may remove those programs which were already easy to dismiss when using brute-force while exclusively keeping the ones that are hard to assess anyway. The lists may also only have the smallest program which is hard to assess. If there's no second-best approximation to `K`, then we're stuck having to find the actual smallest value with no backup if that's impractical to know. Without any practical demonstrations, it's hard to know if these are genuine problems.

---

### Universal Almost Optimal Compression

This method is based on a generic property of compression-decompression pairs. As it turns out, we can, while incurring polylogarithmic overhead in the size of the compressed string, replace a (potentially non-computable) compression algorithm and its decompressor with a pair consisting of an efficient compressor and a potentially inefficient decompressor. By fixing our compressor-decompressor pair to be `K` and `E` (the function that simply evaluates a program), we can get a new compression-decompression pair that will compress inputs to a length which differs, at most, polylogarithmically from `K`. This compressor would not get us smaller, equivalent programs, but, if our goal is to simply approximate the size of a hypothetical Kolmogorov-compressed program, this should work fine.

The basic idea is the following; rather than trying to find the actual smallest compressed string, instead only generate a small "fingerprint" which would allow you to identify what the smallest compressed string might be. The decompressor then just generates a list of candidates, perhaps exhaustively, and uses the fingerprint to find and run it by brute force.

Depending on how much work we're willing to put into making this fingerprint, we can get it down to a pretty small size. According to the paper, it can be made within `K(X) + O(log(X))` with only polynomial effort in the size of the string.

I don't fully understand how this technique works. It's tied up in a lot of the theory that List Approximation uses as well. It's concepts come from the theory of pseudorandomness; something I'll have to become more familiar with.
See
  - [Universal almost optimal compression and Slepian-Wolf coding in probabilistic polynomial time](https://arxiv.org/abs/1911.04268)

---

### Incremental compression

Instead of calculating `K(X)` all at once, it can usually be done piecemeal. The idea is that, given some input `X`, we want to find a pair of functions `F`, `D`, such that `F(D(X)) = X` and `|F| + |D(X)| < |X|`. Specifically, we want to find the smallest `F` meeting this requirement. The idea is that `D(X)` reduces the size of `X`, deleting whatever information is in `F` from `X`. `F` is then that information, isolated from `X`. By repeating this over and over again, we can decompose `X` into a series `F1(F2(F3(...(R))))`, where `R` is the residual which wasn't compressed. In the limit, `R` should basically consist of all the random information present in `X`, while the `F`s correspond to algorithmic "features" which can be isolated from `X`. So long as the `F`s are always as small as possible, this construction will approach the actual Kolmogorov complexity.

I think this line of work hints towards a rich theory of "atomic" algorithmic information, but it's not ready for practical applications as of yet. The incremental compression is not computable, but it should be much quicker to approximate, on average, than `K(X)` while still approaching `K(X)`.

See:
  - [A theory of incremental compression](https://arxiv.org/abs/1908.03781)

---

### Higher-order compression

This is a method of compressing lambda expressions by observing a connection between grammar-based compression and lambda binding.

The procedure is very simple. Start with a lambda expression.
* Compress the expression using a tree-grammar (using re-pair, for instance). Convert this tree grammar back into a lambda expression.

* Run a "simplification procedure" which performs
  * eta-reduction
  * beta-reduction on linear lambda bindings
  * beta-reduction on applications to bound variables

* Repeat until the expression stops shrinking.

I honestly have a hard time believing this works. I'll have to think about it more carefully. To me, this doesn't seem like it should perform better than statistical compression, but, according to the paper [Functional Programs as Compressed Data](http://www-kb.is.s.u-tokyo.ac.jp/~koba/papers/hosc-fpcd.pdf);

>our representation of compressed data in the form of λ-terms is optimal with respect to Kolmogorov complexity, up to an additive constant.

I don't buy the argument given in the paper, though, which just seems to argue that optimal compression should be possible in theory; it doesn't even mention the specifics of the algorithm they present. None the less, I want to include this here since it makes a specific and relevant claim. Some followup work seems to be doing something more computationally interesting, such as [Compaction of Church Numerals for Higher-Order Compression](https://arxiv.org/abs/1706.10061), so a future version of this might be better suited for the task at hand.

Similar grammar-based methods should work for other structured models of computation. For example, using re-pair for graphs as presented in [Grammar-Based Graph Compression](https://arxiv.org/abs/1704.05254), a version should be possible for interaction nets.

<a name="heading2p5"></a>
## Approximating Conditional Kolmogorov Complexity using CTM

While those methods allow approximating `K(X)`, we usually actually want to approximate `K(X|Y)`, the amount of information stored in `X` but not in `Y`; the amount of information `X` tells us if we already know `Y`. The paper tries giving a method for doing this, but the method seems very questionable. It says to do the following;

- Fix a "computable relation" `M : x → y`.
  - I don't know what the paper means by this, and the phrase "computable relation" is never clarified. I would assume that it means that a list of output `ys` can be enumerated on any input `x`, but I don't know. `M` is also described as being a "Turing complete space", in the typical case (e.g. when using CTM). `M` cannot be both a space and a relation, so clearly space is meant in some loose sense, but it's unclear what a "Turing complete space" is supposed to be. I interpret this as meaning that `M` is supposed to be a relation from programs to outputs in the typical case, which is a function, not a relation. But this framing implies that `M` could be broader. Perhaps `M` may be a relation in the case of a nondeterministic computation model, but this is not expanded upon in the paper.
- Fix a finite set `P`, such that `(y, x) ∈ P` iff `y ∈ M(x)`
  - In the ordinary case, `P` would be the database in CTM of output-input pairs.
- The paper then states that we can then approximate `K(X|Y)` by taking the `log₂` of the sum, for all `(Y, X) ∈ P`, of `1/|P|`.

The problems start when we realize that, since `P` is a set, any pair occurs in `P` at most once, meaning that this value is `log₂ (1/|P|)` if `(Y, X)` is in the database or `-∞` if it isn't. This is obviously not what's intended, but I also can't glean from the context what is. Furthermore, the full expression given in the paper is;
```
CTM(X|Y) = log₂ Σ{(Y, X) ∈ P} 1/|P|
```
Both `X` and `Y` are bound twice, once in defining CTM and once by the sum itself. It seems like the second bind is trying to reference the first, but that makes no sense, syntactically. Alternatively, if we interpret the binders as entirely separate, then CTM does nothing with its arguments, and just returns `0` on all inputs (since `log₂ |P|/|P| = 0`), which is obviously wrong.

The simplest fix is to simply make `P` a multiset which may contain multiple copies of any given pair. The calculation should then be;
```
CTM(x|y) = log₂( |[ p ∈ P : p == (x, y) ]| / |P| )
         = log₂ Σ{p ∈ P} if p == (x, y) then 1/|P| else 0
```
This may be off, but this is the closest thing to the original paper I could think of. It just calculates the log-likelihood of a random program covered by `P` outputing `x` on input `y`. Except, there are a few problems with this. Firstly, this quantity will always be negative since `log(X) < 0` when `0 < X < 1`. `K` can never be negative. Even if we fix this, there's still a bigger issue. This CTM definition assumes that all programs are uniformly random, but they aren't. Think of the procedure we'd go through when generating a random program. Assuming our choices are uniformly distributed, the programs we generate won't be. If we assume our programs are binary strings, then we will, at each point, make one of three choices; add a `0`, add a `1`, or end the string. If each choice is uniformly sampled, then there will be a one third chance of generating the empty string, a one in nine chance of generating `1`, and about a one in 60,000 chance of generating `001101001`. The chance of generating a program decays exponentially with the length of the program. This observation is built into algorithmic probability, and it's weird that the CTM measure, as described in this paper, ignores that.

Digressing a bit, I feel like the authors may have some over-familiarity with one specific model of computation. One approach to define `M` used by the Complexity Calculator project is to use an enumeration for Turing machines which, to my knowledge, was originally devised for investigating busy beaver numbers. I believe that the authors are imagining that `M` as a function that enumerates all Turing machines, runs `x` on them all, and outputs a stream of all the `ys` that each Turing machine outputs. This will certainly be a function rather than some generic relation, though.

Let's think of what this measure means for other models of computation. If we were using, say, lambda expressions instead, `M` should enumerate all lambda expressions, take another lambda expression as input, and output the normal forms of the input applied to all possible lambda expressions. This does seem like it makes sense for any model of computation, but I'm not sure it makes sense as a measure of algorithmic similarity.

The justification for this procedure is supposed to come from the coding theorem, which states that `K(X) + O(1) = -log₂(m(X))`, where 
```
m(X) = Σ{p | p ↓ X} 2 ^ -l(p)
```
where `p ↓ X` means `p` normalizes to `X` and `l(p)` is the length of `p`. There's that exponential decay I was talking about.

See: 
- [Scholarpedia: Algorithmic probability](http://www.scholarpedia.org/article/Algorithmic_probability)
- Theorem 4.3.3 of "An Introduction to Kolmogorov Complexity and Its Applications"

Modifying this for lambda expressions,
```
m(X) = Σ{l | l ↓ X} 2 ^ -I(l)
```
where `l ↓ X` means `l` normalizes to `X` and `I(l)` measures the bit information of `l`, essentially the number of binary decisions made when constructing `l`. `I(l)` would be calculated
```
I(l) := I(l, 0)
I(λ x . y, b) := log₂(2 + b) + I(y, b + 1)
I(x y,     b) := log₂(2 + b) + I(x, b) + I(y, b)
I(x,       b) := log₂(2 + b)
```
Incidentally, the length of a binary string doesn't actually give the information content of that string. If a string's length isn't fixed beforehand, then each additional digit incurs one trit of information since at each stage of the construction we are choosing between one of three options; `0`, `1`, or stop constructing the string. From this, we can conclude that `l(s) = log₃(2 ^ -I(s)) - 1`; that is, the length of a string is one less than the number of trits in that string. If the string's length is fixed beforehand, if we cannot choose to end the construction of a string at our leisure, then each choice is actually binary and `l(s) = I(s)`.

I think that using `I(s)` to calculate the information rather than the length is more theoretically correct than the usual expression in terms of length. It doesn't seem to matter too much in the case of strings because the sum over all `2 ^ (-l(s)-1)` = the sum over all `2 ^ -I(s)` = `1`, so both are valid ways of making a probability distribution over all programs with a similar exponential decay. That `-l(p)-1` is there so that the empty string isn't given 100% of the distribution. The real problem is generalizability; the length calculation generally fails to make a coherent distribution if our computation model no longer accepts binary strings as inputs. The information, however, can always be adapted even if our computational model expects programs to be something esoteric, like graphs, such is the case with interaction nets.

As a side note, despite length being theoretically incorrect, it's been used in some papers for measuring the information of a lambda expression. See [Computable Variants of AIXI which are More Powerful than AIXItl](https://arxiv.org/abs/1805.08592) for instance. But it seems like the theoretically wrong thing to do, especially since the actual information is so easy to calculate. I think many authors in this field don't think too carefully about the information content of the things they write about, which is quite ironic.

The conditional coding theorem states that;
```
K(X|Y) + O(1) = -log₂(m(X|Y))
```
where
```
m(X|Y) = Σ{p | p(Y) ↓ X} 2 ^ -l(p)
```

See: 
- Theorem 4.3.4 and Definition 4.3.7 in "An Introduction to Kolmogorov Complexity and Its Applications"

This is definitely not what that CTM measure is approximating. Because the original in the paper is so obviously wrong, and the nature of `M` is so poorly explained, it's hard to patch it up to whatever the author's intended. In fact, I'm not sure this is actually possible. The conditional coding theorem relies on the length of the program, `p`, which `Y` is being fed into. This would require us to incorporate the complexity of the Turing machine itself, but `P` doesn't store that information.

Let me try to offer a more sensible formulation of the CTM idea. Assume a computing function `M : x → y` which simply evaluates an input program into an output using a fixed computational model. Let `P` be a finite subset of output-input pairs `(y, x)`. The input type should satisfy the smn theorem, so we can format programs like `f(x)`; have functions which can have variables substituted into them. For some Turing machines, application is often just done as list concatenation, though, this becomes squirley if we want to represent functions which take multiple arguments, nested function application, etc. For a more well-structured model of computation, such as the lambda calculus, application may be a fundamental operation. Regardless, we can then define our metric as;

```
CTM(x|y) = - log₂( Σ{ p | (x, p(y)) ∈ P } 2 ^ -I(p) )
```

This would require us to be able to pattern match so as to detect `p(y)`. If application is just concatenation, then this is as simple as looking for the suffix `y`, which is pretty trivial.

This doesn't much resemble what's in the paper, but it makes much more sense.

The paper mentions that `CTM(x)`, which approximates non-conditional Kolmogorov complexity, can be defined as `CTM(x|"")`, `x` conditioned on the empty string. Well, actually it says that `CTM(""|x)` should do this, but that doesn't make any sense. It's unclear enough in the original, it should definitely be `CTM(x|"")` in my modified version since it would just be summing for every program `p = p ++ ""` which outputs `x`; hence it's eminently compatible with concatenation-as-application. In general, a separate measure would need to be made for other models of computation since application-as-concatenation doesn't even make sense in general for Turing machines (do you really think application should be associative?), much less other models of computation. More generically, we'd define;

```
CTM(x) = - log₂( Σ{ p | (x, p) ∈ P } 2 ^ -I(p) )
```

<a name="heading3"></a>
## Block Decomposition

In the section on BDM (Block Decomposition Method), the authors keep calling things "tensors", but it never explains what a tensor is. It definitely isn't in the ordinary mathematical sense since the only things described as tensors are just binary strings. The [original paper on BDM](https://arxiv.org/abs/1609.00110) talks about compressing tensors and vectors. However, neither of those two things are actually compressed in that paper. Instead, it seems like the authors think that any list is a vector and any list-of-lists is a tensor, which is what they actually compress. It's annoying when authors abuse terminology like this; it's just confusing. From that, I think "tensor" just means a 2-dimensional array of bits. Just call them arrays if that's what they are! This is a CS paper, after all.

I have a suspicion that the segment on block decomposition was copy-pasted from somewhere else without any copyediting. Tensors aren't mentioned outside the section on BDM.

Setting that aside, we're trying to approximate `K(X|Y)` using `BDM(X|Y)`. Assume a fixed "partitioning strategy". The paper never explains what this is, but I read other sources (which I'll talk about later on) which informed me that a "partitioning strategy" is simply a method of splitting up a string into (possibly overlapping) substrings which are already in our CTM database. What BDM tries to do is then devise a "pairing strategy" which minimizes a quantity. The paper doesn't state what a "pairing strategy" is either, and no other source I read clarifies. It only says the following;

A pairing strategy generates a set `P`
  - consisting of pairs of pairs `((rx, nx), (ry, ny))`
    - where `rx` and `ry` are partitions of `X` and `Y` made by our partitioning strategy
      - where each `rx` occurring in `P` must only occur once, though there is no similar restriction on `ry`.
        This just means that `P`, treated as a relation, is (non-totally) functional.
    - where  `nx` and `ny` are the occurrence counts of `rx` and `ry` within the partitionings of `X` and `Y`, respectively.

That's all it says on pairing strategies. As far as I can tell from this, a pairing strategy that pairs nothing and is just empty is valid, but I'm pretty sure it's not supposed to be.

Assuming we have an understanding of what additional constraints a pairing strategy should have, we want to find the pairing strategy which minimizes the following quantity;
```
Σ{((rx, nx), (ry, ny)) ∈ P} CTM(rx|ry) + if nx == ny then 0 else log(nx)
```
The minimal value for this quantity will be `BDM(X|Y)`.

This quantity will always be nonnegative and we can always minimize it to zero by making `P` empty. This is obviously not intended. It also doesn't make much sense to me that we're taking the log of `nx` if `nx` is just the count of `rx`s rather than something involving the length of `rx`. And shouldn't that log term scale with the difference between `nx` and `ny` in some way? The paper offers no real intuition.

Maybe looking at the [original BDM paper](https://arxiv.org/abs/1609.00110) can offer clarification. It gives a nice example which I'll reproduce here. Let's say we're applying BDM to the string 
```
010101010101010101  
```

We have a choice of partitioning strategy, but it gives the example of splitting the string into substrings of length 12 which may overlap by, at most, 11 digits. When we do this, we get 3 `101010101010`s and 4 `010101010101`s. According to CTM, both strings have a complexity of 26.99 bits (assuming we're using only 2-state binary Turing machines). This would indicate that the smallest program generating the 12 digit string is, at most, 26 digits long.  Also, there are no Turing complete 2-state binary Turing machines, so this choice seems doubly weird. We then calculate the BDM value as
```
26.99 + log(3) + 26.99 + log(4) ≈ 57.565
```
... Okay, but shouldn't it be way smaller? The original string wasn't even twice as long as its partitions, and it should be almost as easy to generate as the partitions. I thought this might be an idiosyncrasy of the specific kind of Turing machine which the paper uses, but the [complexity calculator website](https://www.complexitycalculator.com/) says almost the same thing, giving the "BDM algorithmic complexity estimation" as 57.5664 bits when we select a block size of 12 with an overlap of 11.

Let's digress a bit and think of `K` in the lambda calculus. Firstly, we need a way to represent binary strings. We'll just encode these as lists of bits. The type of bits will be defined as
```
2 = ∀ X . X → X → X = {0, 1}
```

where

```
0 = λ f . λ t . f
1 = λ f . λ t . t
```

Strings should have an appropriate elimination rule stating that, for any type family or predicate `P` over binary strings,

```
∀ S : BinString . (∀ s : BinString . (b : 2) → P s → P (b :: s)) → P "" → P S
```
This is essentially the induction rule for binary strings. One form of it, anyway. We can replace that predicate with a polymorphic variable to get our representation.
```
BinString = ∀ X . (2 → X → X) → X → X
```
Compare
```
∀ S : BinString . ∀ P . (∀ s . (b : 2) → P s → P (b :: s)) → P "" → P S
                  ∀ X . (           2  → X   → X         ) → X    → X
```
For any particular string, `S`, we can realize the induction principal using 
```
λ c : ∀ s . (b : Bits) → P s → P (b :: s) . λ n : P "" . S c n
```
See
- [Generic Derivation of Induction for Impredicative Encodings in Cedille](https://homepage.divms.uiowa.edu/~astump/papers/cpp-2018.pdf), I guess, since I don't know of a better source on this topic.

I'll talk about alternate representations later on, but I think this is among the most natural representations given the mathematical structure of the data type. "Most natural" doesn't necessarily mean "best", though. Unary natural numbers are more natural than binary representations since pretty much every simple representation of the universal property of ℕ suggests a unary representation. However, they're horribly inefficient.

Using this representation, the original string would be encoded as

```
λc . λn . 
  c 0 (c 1 (c 0 (c 1 (c 0 (c 1
  (c 0 (c 1 (c 0 (c 1 (c 0 (c 1
  (c 0 (c 1 (c 0 (c 1 (c 0 (c 1 n))))))
  )))))))))))
```

This representation has about 192.747 bits of information. That information count might seem like a lot, but the lambda calculus represents trees of any kind as efficiently as it can represent lists. In this case, the string is a list of bits, which is about as small as can be expected. 

However, it can be compressed to;

```
λc . λn . 
  (λ f . λ x . f (f x))
  (λ f . λ x . f (f (f x)))
  (λ x . c 0 (c 1 x))
  n
```

which has about 83.93 bits of info. One of the 12 character sub-partitions can be compressed into

```
λc . λn . 
  (λ f . λ x . f (f x))
  ((λ f . λ x . f (f (f x))) (λ x . c 0 (c 1 x)))
  n
```
For the other, just swap `0` and `1`. This has about 83.93 bits; the exact same, in fact, as the full string. The reason why these are the same is that there are 9 repetitions of `01` in the full string and `9 = 3 ^ 2`. In the substrings, there are 6 repetitions of `01` or `10`, and `6 = 2 * 3`. The information in multiplication is the same as the information in exponentiation, so the representations end up having the same amount of info. Of course, I don't know if these are actually the smallest possible representations; these are just the smallest I could come up with. They do illustrate my point, however. The two strings should have about the same information; maybe the original should have a little more. It seems extremely suspicious to me that the two strings have such dramatically different bit-counts according to BDM.

This isn't the only way to represent binary strings. We can write the original string instead as;

```
λ0 . λ1 . 
  0 (1 (0 (1 (0 (1
  (0 (1 (0 (1 (0 (1
  (0 (1 (0 (1 (0 (1 (λ x . x)))))))
  )))))))))))
```

To justify this representation we need to prove that its type is isomorphic to something satisfying the universal property of binary strings. Here, `1` and `0` are expected to take a function `X → X` and return another function of the same type. This means our new representation has type;
```
∀ X . (X → X) → (X → X) → (X → X)
```
we can rewrite this using a bit of type algebra;
```
  ∀ X . (X → X) → (X → X) → (X → X)
≅ ∀ X . (X → X) × (X → X) × X → X
≅ ∀ X . (X → X) × (X → X) × (1 → X) → X
≅ ∀ X . (X + X + 1 → X) → X
≅ ∀ X . (2 × X + 1 → X) → X
```
Note that any type of the form `∀ X . (F(X) → X) → X` is the (weakly) initial algebra over the endofunctor `F`. Binary strings are the initial algebra over the endofunctor `X ↦ 2 × X + 1`, a special case of initiality for lists in general. Continuing this calculation;
```
  ∀ X . (2 × X + 1 → X) → X
≅ ∀ X . (2 × X → X) → (1 → X) → X
≅ ∀ X . (2 → X → X) → X → X
```
Which is the representation we started with. This justifies that the new representation is isomorphic to the old one and we can comfortably use it interchangeably. See also;
- [Recursive types for free!](https://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt)
- [The Algebra of Algebraic Data Types (Youtube)](https://www.youtube.com/watch?v=YScIPA8RbVE)

Our new representation has about 78.9 bits of information, while the sub-strings have about 54.9 bits. We can compress our larger string to
```
λ0 . λ1 . 
  (λ f . λ x . f (f x))
  (λ f . λ x . f (f (f x)))
  (λ x . 0 (1 x))
  (λ x . x)
```
which has about 57.27 bits of information, less even than what BDM states the Turing machine representation should have. And the lambda calculus has to represent every tree-like datatype! What's the Turing machine representation doing with all that space below 57 bits if it can't even fit 9 repetitions of `01`? As far as I can tell, the two 12 digit substrings can't be compressed any further, but my point from before still stands; both strings have similar amounts of algorithmic information. It's suspicious that BDM would say otherwise.

...

Okay, I think I figured it out. I was confused about the partitioning strategy. It's up to us to find a good selection of a block-size and overlap. Going back to the complexity calculator, if I set the block-size to 2 and the overlap to 1, it estimates the complexity to be 12.82 bits. Doing the calculation myself, we have 9 repetitions of `01` and 8 repetitions of `10`. The calculation would then be;
```
3.3274 + log(9) + 3.3274 + log(8) ≈ 12.8247
```
Going through all the options in the complexity calculator, the minimal is a block-size of 2 with an overlap of 0. This partition only has 9 `01`s, and nothing else. This gives the calculation as;
```
3.3274 + log(9) ≈ 6.4973
```

The BDM paper states that 

> [...] if `|Adj(X)|` is close to `1`, then `BDM(X) ≈ K(X)`.

where `Adj(X)` is the set of pairs of substrings with their occurrence counts. The block-size of 2 with an overlap of 0 gives an `|Adj(X)|` of exactly 1 since it only has one partition; `Adj(X) = {(01, 9)}`. This should, presumably, be as close to the actual Kolmogorov complexity as BDM can get. I suppose that means we're trying to find the partition strategy which minimizes the number of substrings that are covered by the CTM database.

The appendix of the paper has a section on "The Impact of the Partition Strategy". It says the following;

> BDM better approximates the universal measure `K(X)` as the number of elements resulting from applying the partition strategy to `X`.

as the number of elements... what? Was this paper not copyedited!

> `BDM(X|Y)` is a good approximation to `K(X|Y)` when the `Adj(X)` and `Adj(Y)` share a high number of base tensors.

I assume that this doesn't actually rely on our data being tensors (or 2-dimensional arrays).

> We conjecture that there is no general strategy for finding a best partition strategy [...] Thus the partition strategy can be considered an hyperparameter that can be empirically optimized from the available data.

Hmm... this seems like a big gap in the whole approach.

So this clears up some things about the partitioning strategy, at any rate. But I wasn't worried about the partitioning strategy anyway; I wanted to know what a "pairing strategy" is! The original paper on BDM isn't any help since it doesn't describe conditional BDM at all.

Going back to the topic paper of this post, it does describe a "coarse conditional BDM of `X` with respect to the tensor `Y`". Again, tensors are not explained at all in the paper, and it's unclear if `Y` actually needs to be a tensor in any mathematical sense. As I stated before, I think the authors just mean a 2-dimensional array when they say "tensor", and it seems obvious that the construction doesn't rely on dimensionality at all. It defines `BDM(X|Y)` as
```
BDM(X|Y) = (Σ{(rx, nx) ∈ Adj(X) && rx ∉ Adj(Y)} CTM(rx) + log(nx))
         + (Σ{(rx, nx) ∈ Adj(X) ∩ Adj(Y)} log(nx))
```
This definition isolates the unique information in `X` while issuing additional penalties if the information shared between `X` and `Y` appears more or less often in `X` than in `Y`. I'm not sure if this makes sense. The paper says;

> [the second sum] is important in cases where such multiplicity dominates the complexity of the objects

but, intuitively, it seems to me like the sum should only add a penalty if `nx > ny`; because, otherwise, we're penalizing the conditional complexity of `X` for content that's in `Y` but not in `X`. I'll have to think about this a bit more.
 
The "coarse" BDM is, I guess, less accurate than the "strong" BDM that I first looked at; but, at least, it makes sense. It's weaker since it doesn't utilize conditional CTM. But without additional clarification on what a "pairing strategy" is, I just can't understand how the strong version works.

I've thought a lot about it and, while I'm not confident, I think I've figured out the two most reasonable fixes.
- If the pairing strategies must cover `X` then that solves the specific problem I pointed out.

- If P is supposed to be totally functional over the partitions of `X`.

Neither of these conditions is hinted at in the paper, but it's the best I've got. The paper does say;

> prior knowledge of the algorithmic structure of the objects can be used to facilitate the computation by reducing the number of possible pairings to be explored

So, at the very least, the pairing strategy is supposed to be determined by some algorithm that isn't described in any detail. I'm frustrated by this whole thing.

<a name="heading4"></a>
## Spit-balling ways to improve DBM and CTM

One of the thoughts I had was that there may be program patterns in the computational model that always uniformly evaluate. For example, it may always be the case that `100101X10101Y0`, for any strings `X` and `Y`, evaluate to `1100101`, or maybe `0Y1010`. In either case, we can make the replacement to get a smaller program without running the program. Or maybe it reduces to `01Y0Y01`. Depending on the length of `Y` this may or may not be a reduction in size. And there may be variations on this involving variable substrings being riffled and combined in nontrivial ways.

The CTM database only keeps track of string reductions, but it may be possible to search for patterns and perform block replacement via unification/pattern matching instead. This description was given in terms of first-order unification, but there may be a close link with higher-order unification; unification modulo computation.

This also seems similar to finding smaller extensionally equivalent expressions to certain programs. That is, finding programs that don't normalize to the same term but do normalize to the same term when given the same inputs. Programs that are not the same but are behaviourally indistinguishable. I wrote a program a while ago in Mathematica which enumerated SKI expressions, applied 30 or so variable arguments to them, and collected them into pairs which normalized to the same expression after application. In this way, I built up a database which I used to make expressions smaller by replacing expressions with smaller extensionally equivalent ones. In practice, this ran into some issues. Namely, two extensionally equivalent expressions are not guaranteed to have the same behavior since one may start evaluating after two arguments while the other will evaluate after three. For example, the following two expressions are extensionally equivalent, but they only normalize to the same term after two arguments are applied despite the first fully normalizing after only one application.
```
λ f . f
λ f . λ x . f x
```
This only ceases to be a problem if you have some sort of typing discipline which can allow you to infer the number of arguments an expression expects. You can then assess extensional equivalence up to that number of arguments while also guaranteeing the preservation of expected behavior up to the type of the full expression you're compressing. This, of course, doesn't work on extensional equivalences which require some elimination principle to justify; e.g. the equivalence of merge sort with quicksort. This may be particularly relevant to incremental compression.

---

Generating programs with specific, even simple, types is highly nontrivial. It's just the proof synthesis problem where you want to enumerate all proofs (encoding programs) of a given theorem (encoding a type). Restricting to certain typing disciplines, such as simple types without intersection types, certain polymorphic typing disciplines, some refinement type disciplines, some disciplines heavily leaning on algebraic data types, and some others, can be searched fairly efficiently, however. The following papers seem particularly relevant;

- [Type-and-Example-Directed Program Synthesis](https://dl.acm.org/doi/pdf/10.1145/2813885.2738007)
- [Example-Directed Synthesis: A Type-Theoretic Interpretation](http://www.jfrankle.com/refinements-popl-16.pdf)
- [Program Synthesis from Polymorphic Refinement Types](https://dl.acm.org/doi/pdf/10.1145/2980983.2908093)
- [Generating Constrained Test Data using Datatype Generic Programming](https://dspace.library.uu.nl/bitstream/handle/1874/383386/thesis.pdf?sequence=2)

This may be leverageable for some applications. In fact, I'd guess most applications could leverage this.

It's also worth noting that this whole problem is just the inductive programming problem + a desire to minimize the induced program's size. Inductive programming is a whole field unto itself. There are algorithms which can exhaustively produce programs which have a particular output sequence;

See;
  - [Google Scholar: "exhaustive program generation inductive programming"](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C3&q=exhaustive+program+generation+inductive+programming&btnG=)

These approaches can be used as compression methods. They're not guaranteed to approach the Kolmogorov complexity, but they should generally do a much, much better job than statistical compression methods while being much more efficient than methods attempting to approximate `K` directly.

---

Consider the possibility of an algorithm, `C(X)`, and a Shannon optimal compressor, `S(X)`, such that the sum over all `X` of `C(X) - S(X)` is negatively infinite. That is to say, `C(X)` tends to perform better infinitely much as a Shannon optimal compressor while being no slower. BDM already does this, but is there an algorithm that can do this without keeping track of a large database which takes exponential effort beforehand to calculate?

It's actually fairly easy to outperform Shannon optimal compressors by infinitely much. Have a compressor do the following;
If it detects a string encoding `0, 1, 2, 3, 4, 5, ...,` replace it with a flag indicating such and a number indicating how far the sequence counts.
For all other strings, replace them with the output of a Shannon optimal compressor.
Such a scheme would generally perform no worse than a Shannon optimal compressor while performing better by infinitely much; though the benefits clearly only apply to a small number of patterns overall, even if there are infinitely many such patterns. This means that CTM will generally be able to improve by infinitely much by adding entries to its database, but expanding this database takes exponential effort. Is there a way to do better? Is there a way to characterize how far this can go without exponential effort? Even if it doesn't cover as much of program space asymptotically, is there a way to grow the database forever using only, say, linear effort? Or quadratic? Or logarithmic? And could such things be efficiently sewed together so that we discover the easy things first and foremost, for even very large strings, and the hard things later?
```
1, 2, 3, ... 999999999, 1000000000
```
Is easy to compress algorithmically, but I wouldn't expect BDM to do much better than statistical compression. I do believe that such things should be efficiently discoverable anyway, but not by CTM or BDM, as it stands.

---

I think we may need to start thinking about the amount of effort we'd be willing to expend during compression. `K` and its alternatives seem somewhat backward in their definition. `K` is the minimal size of a program that can become our target if we're willing to expend an arbitrarily large effort *during decompression*. Levin complexity is just `K` plus the log of the runtime of the smallest program. Essentially, Levin complexity is the minimal size of a program that can become our target if we're willing to expend only an exponential amount of effort on decompression. But, shouldn't it be the other way around? Shouldn't we care more about the amount of effort we want to put *during compression*?

For the purposes of ML, we don't care about decompression at all. What would be better to know is how small a program can be if we're only willing to spend exponential, quadratic, linear, etc. effort with respect to the length of the string we're compressing. Are these problems solvable? I have a suspicion that feedforward NNs are essentially solving a subset of the linear effort case.

This is an area which has been explored quite a bit; by the paper on [Universal almost optimal compression](https://arxiv.org/pdf/1911.04268.pdf) I mentioned earlier, for instance. I would like to explore this area in the future, and I believe it will be of tremendous importance for making compression practical for machine learning.

---

Here's another idea. This one is original to me, but I wouldn't be surprised if someone came up with something similar. Some models of computation can be run backward, nondeterministically. Specifically, models where every state can be reached in one step by only a finite number of transitions. This *can't* be done effectively with the lambda calculus. If we were in a state `λ x . λ f . f x x`, that could have been reached in one step by
```
λx . (λ y . λ f . f y y) x
(λx . x) (λ x . λ f . f x x)
λ x . λ f . (λx . x) (f x x)
(λ d . λ x . λ f . f x x) (λx . x)
(λ d . λ x . λ f . f x x) (λx . x x)
(λ d . λ x . λ f . f x x) (λx . x x x)
...
```
and infinitely many other things. This means that running a lambda expression backward implies enumerating infinite possibilities at each step. That doesn't mean running expressions backward is impossible, but it limits the utility of such an approach since we'd basically be enumerating every lambda expression an infinite number of times at each backward step. The same applies to combinator logic.

Many models of computation, however, don't have this property. Anything where a fixed amount of work is done at each step don't; that includes Turing machines, interaction nets, the linear lambda calculus, and most abstract machines. These can all be run backward, as a result. We can then enumerate all the programs which normalize to a particular output by doing the following, assuming we're using an appropriate Turing machine;

- Start with our output string.
- Enumerate every end-state involving this machine. That is, every case where the head of the machine is at every position while in the halting state. 
  - For each of these, generate an infinitely tall rose tree by recursively running the program backward for each time step. We can collapse these trees into a stream by doing a breadth-first-search and we can collapse these searches together by riffling the streams. 
  - Every time we reach a point where the machine's head is at the beginning of the string in the starting state, we've logged a program which normalizes to our output.

This procedure will look for and find only those programs which normalize to our desired output, ordered by running time. We can keep this going for as long as we want, remembering only the smallest program found so far. The longer we go, the closer our approximation is to the actual shortest program and therefore the actual Kolmogorov complexity. There are also probably heuristics we could apply to prune the search of paths which won't get us anything smaller than what we already have. I don't know how efficient this could be made, but it seems to me that it would do better than BDM on large strings.

For parallel models of computation, such as interaction nets, we can optimize this further by treating the (backward) transformations of different segments of the program independently and only combine the timelines when they start interacting.

A further optimization, which would make finding small programs easier but would make finding values verifiably close to `K` harder, is to iteratively compress programs. We run a program backward only until it shrinks. We then abandon all other search branches and start over with the new, smaller program. Doing this over and over may allow one to effectively find and run recursive programs that generate an output backward much more efficiently than an unpruned search.

---

Here's another idea. In CTM, we're enumerating all programs and seeing what they output. It may, instead, be better to enumerate programs and then filter them for randomness. Essentially, we'd build up a database by looking at each program in algorithmic order. We'd try building that program from the programs already in the database. If we can't build it using the existing elements in the database in a way that's smaller than what we're trying to build then the thing we're looking at is algorithmically random and we add it to the database as a random "atom". This should significantly cut down on the combinatorial explosion, though, I'm pretty sure it would still be exponential.

---

Ultimately, I think this whole problem should eat itself. If we're using a universal learning algorithm, then, certainly, it can learn to be better, somehow. Bootstrapping should be possible in this domain.

<a name="heading5"></a>
## Algorithmic Loss

Hey, wasn't this post supposed to be about machine learning? Oh ya! Let's finally talk about that!

For any standard ML technique, we need a definition of Loss which we want to minimize. The exact way this will be defined will depend on what our task is. Generally, we'll use some form of conditional Kolmogorov complexity directly as our loss. Similar measures are already used in some applications. Cross-entropy is the most directly related loss, and using algorithmic complexity can be thought of as a more robust form of entropy.

Generally, our loss will try to capture how much of the data *isn't* captured by our model. We want our loss to answer the question, "given our model, how much of the data still needs to be explained?" To that end, our loss will generally be a function of the training data *conditioned on our predictions*. Given a data point `y` and a prediction `Y`, our loss on that point will be `K(y|Y)`. To get a total loss we can just add all these measures together.

But the paper suggests adding the *squared* losses together. Why should we do this? Why do we do this for normal ML? Well, we can't normally just add together the losses since they can often be negative. `K` can never give negative values, so that's not a problem here. Why would we use the mean squared error rather than the mean absolute error? There are two explanations I've seen. Firstly, I've seen some say MSE is easier to differentiate than MAE. This isn't true outside of very simple models where you want to find the optimum in a single step, such as linear regression, and we aren't going to be differentiating `K` anyway, so this doesn't matter. The other reason comes from an assumption that we're modeling things sampled from a gaussian distribution.

This has always irked me. No matter how many statisticians say "it's reasonable to assume everything is being sampled from a gaussian", that doesn't make it true. If you do any bayesian ML, you'll find that a significant effort spent on standard techniques is in distribution engineering. If what you're modeling can't be negative, is multi-modal, follows a power-law, or a litany of other things then you're not looking at data sampled from a gaussian and you'll have to make a different distribution instead.

Anyway, let's do the derivation;

Firstly, what we always really want to do is maximize the likelihood. Our model is going to make predictions about the probability of various data. The likelihood of our model is just the product of all the probabilities of each training point as predicted by our model.
```
L(m) = Π(i) m_prob(yi)
```
In practice, this will usually end up multiplying a bunch of numbers below 1 together, getting a vanishingly small likelihood for models training on a lot of data. Because of this, we usually use the negative log-likelihood, which is
```
NLL(m) = - Σ(i) log(m_prob(yi))
```
This makes all our too-small numbers large without losing any information, so this is usually what real algorithms try to minimize. On a historical note, this trick was often used to make carrying out tough calculations easier. Logorithm tables were a hot commodity back before calculators became commonplace. Anyway, we often also divide by the total number of data points to turn this log-likelihood into a mean log-likelihood, that way our loss doesn't become huge just because we're working with a lot of data points.

The equation of a gaussian is;
```
e^(- (x-μ)² / 2 σ²) / σ √(2 π)
```
The "prediction" made by a Gaussian model will be the mean, `μ`, and the likelihood of a particular piece of data `x` will be that data fed into the PDF of a gaussian with the predicted mean. Substituting with those changes into our negative log-likelihood, this becomes
```
- Σ(i) log(e ^ - (yi - y_pred)² / 2 σ²)  / σ √(2 π)
  = (1 / σ³ √(8 π)) Σ(i) (yi - yi_pred)²
```
which is exactly the squared error, modulo some constant we don't care about. And getting the average by dividing by the number of data points will get us the mean squared error, MSE. This should also illustrate that if you don't think it's reasonable to assume your data were randomly sampled from a gaussian distribution, then you should also not think it's reasonable to use the squared error without a similar derivation.

Okay, so, what's the justification for squaring `K`? Let's think about this, what are the probabilities in our likelihood? Well, they'll be the algorithmic probabilities; the probability that a random program will output the datapoint when given our model's prediction as an input. The coding theorem says exactly that (within an additive constant) the Kolmogorov complexity of a program is the negative logarithm of the algorithmic probability, meaning the appropriate negative log-likelihood is exactly the sum of `K`s.

But, wait, I was looking for justification for squaring `K`. That's what the paper does. Does it say why?

> [...] in order to remain congruent with the most widely used cost functions, we will, for the purpose of illustration, use the sum of the squared algorithmic differences.

Oh, so there is no reason. To be clear, there is a clearly right thing to do here; use the sum of `K`s, not squared `K`s. We may also want to divide by our number of data points to get the mean `K` error rather than just the total error. I don't think the author's thought very hard about what the loss should be. For much of the paper this odd, clearly wrong loss function will be used.

The paper goes on to talk about categorical loss. The obvious thing, to me, is to do basically the same thing, and that's what the paper recommends. Assume our model is outputting some object which is being used to predict the class. In classical ML, this would be like the class probabilities before assigning a class. The loss will be `K(Y|M(X))`, where `Y` is the actual class and `X` is our input data. This signifies how much information is in the real class but not in the prediction of our model. If we were using our model for prediction, then the class `C` which minimizes `K(C|M(X))` would be our prediction. 

The paper relates this to clustering by algorithmic distance. I don't see the connection, but it does reference another generic version of a standard ML technique; specifically, it points out the paper [Clustering by Compression](https://homepages.cwi.nl/~paulv/papers/cluster.pdf).


<a name="heading6"></a>
## Algorithmic Optimization

Great! So, we know how to assess a model, how do we actually do optimization? Prepare to be disappointed, because the answer offered in the paper is "brute search"! Well, the paper phrases it in a more, umm, appealing way. The optimization algorithm is defined as follows;

- Keep track of the most recent minimal cost in a variable called `minCost` which is initially set to infinity.
- Keep track of the best set of parameters in a variable called `param`, which is, I guess, initially set to `null`, or something.
- Create a stream of all possible parameters of our model in algorithmic order (that is, ordered by their `K`).
- For each set of parameters, calculate the loss of the model with those parameters. If the loss is less than the current `minCost`, then set `minCost` to this value and set `param` to the parameters being used.
- Keep going until you're bored or satisfied or no reason at all; the paper doesn't care.

That's it. However, there are some complications that make me not entirely sure if this is right. It defines the cost function (the sum of squared `K`s) to be `J_a(ˆX, M)`, where `ˆX` is our dataset, and `M` is the model we're assessing. However, the actual description of the algorithm tells us to use `J_a(ˆM, σ_i)`, where `σ_i` is the ith parameter in our parameter stream and `ˆM` is never defined. Presumedly, `ˆM` has something to do with our model, but it can't possibly be a replacement for `ˆX` since `ˆX` is just a collection of input-output pairs and our model is, obviously, not. Either there's an entirely separate loss function over models and parameters which is never defined, or there was a typo and the algorithm should have said `J_a(ˆX, M{σ_i})`, or something like that. The version I wrote seems pretty intuitive (if overly simplistic), so I'm leaning toward the latter.

The paper states that this algorithm "can ['minimize `K(M)` and minimize the cost function'] in an efficient amount of time". But, uh, no it doesn't. It does as bad as a brute force search because it is a brute force search. It goes on to say 

> the algorithmic parameter optimization always finds the lowest algorithmically complex parameters that fit the data `ˆX` within the halting condition [...] algorithmic parameter optimization will naturally be a poor performer when inferring models of high algorithmic complexity.

Ya think? I'm not confident that this would work on anything but toy problems, but, who knows, maybe I'm wrong and this actually works surprisingly well on real-world data, but I doubt it. The algorithm doesn't even try taking advantage of what it knows about good parameters.

As an aside, the paper mentions that

> In the context of artificial evolution and genetic algorithms, it has been previously shown that, by using an algorithmic probability distribution, the exponential random search can be sped up to quadratic

giving a few citations. This seems more reasonable to me, as such methods aren't just brute-force searching.

This will be a bit of a digression, but if you read this far you probably don't care about that. The first example it uses is a regression problem on two variables. It says the following on the ability to enumerate the parameter space;


> For instance, in order to fit the output of the function `f` (Eq. 2) by means of the model `M`, we must optimize over two continuous parameters `s1` and `s2`. Therefore the space of parameters is composed of the pairs of real numbers `σ_i = [σ_i1, σ_i2]`. However, a computer cannot fully represent a real number, using instead an approximation by means of a fixed number of bits. Since this second space is finite, so is the parameter space and the search space which is composed of pairs of binary strings of finite size [...]

This entire paragraph is rather head-scratching. Computers certainly can fully represent a real number. We can figure out how by following the same basic procedure I used before to figure out how to encode binary strings. You just state a universal property of the mathematical object you want to represent and derive a type of realizers. This is a bit squirrely with the real numbers as the exact universal properties diverge in constructive settings. Dedekind Reals and Cauchy Reals aren't isomorphic anymore, for instance. There are also practical questions about how to make calculating with them as easy as possible. That being said, the simplest universal property for any kind of real number I'm aware of is the following; the (nonnegative) real numbers are the final coalgebra of the endofunctor `X ↦ ℕ × X`. There are a few places that say this in various guises. The most direct is [On coalgebra of real numbers](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=9A564F2172717230E15D3F8EC5253423?doi=10.1.1.47.5204&rep=rep1&type=pdf) which is all about this observation. See [this](https://ncatlab.org/nlab/show/continued+fraction#half-open) as well.  This basically says that real numbers are an infinite stream of natural numbers. There are a few ways of viewing what this represents, and that will largely determine whether you see each number as representing a nonnegative real or something isomorphic, like something in `[0, 1)`. For the latter, we can read each natural number as describing how many 1s we encounter before encountering a `0` in the binary expansion of a number. For example;
```
0 = [0, 0, 0, ...]
0.1 = [0, 0, 2, 0, 2, 0, 2, 0, 2, ...]
0.2 = [0, 2, 0, 2, 0, 2, 0, 2, 0, ...]
1/3 = [1, 1, 1, 1, 1, 1, 1, ...]
√2 - 1 = [2, 1, 1, 0, 0, 0, 0, 1, 0, 4, ...]
π - 3 = [0, 1, 0, 1, 0, 0, 0, 6, 2, 1, 1, ... ]
```
Of course, we can map this back on to the non-negative reals by interpreting each number `x` as `1/(1-x) - 1` instead. Then we'd have
```
0 = [0, 0, 0, ...]
0.1 = [0, 0, 0, 1, 3, 1, 0, 0, 1, 3, 1, 0,...]
0.2 = [0, 1, 1, 1, 1, 1, 1, 1 ...]
√2 = [1, 0, 1, 1, 5, 2, 0, 0, ...]
3 = [2, 0, 0, 0, 0, 0 ...]
π = [2, 0, 0, 0, 1, 0, 0, 2, 0, 0 ... ]
e = [1, 3, 2, 0, 1, 0, 2, 1, ... ]
```
Alternatively, we can imagine each entry in a sequence `[a0, a1, a2, ...]` as representing a number as a simple continued fraction of the form
```
a0 - 1/ (a1 - 1/(a2 - 1/ ...))
```
This can represent any nonnegative real number.
```
0 = [0, 0, 0, ...]
1 = [1, 0, 0, 0, ...]
0.1 = [0, 10, 0, 0, 0, ...]
0.2 = [0, 5, 0, 0, 0, ...]
1/3 = [0, 3, 0, 0, 0, ...]
√2 = [1, 2, 2, 2, 2, 2, ...]
π = [3, 7, 15, 1, 292, 1, 1, ... ]
```
whichever interpretation we use will determine how we define, for instance, addition, multiplication, etc. Note that there are some complications with representing real numbers as continued fractions in this way. Notably, that some numbers don't have unique representations. While I understand these caveats, I don't understand how to solve them, though I've been told that such solutions are "various".

Following [Recursive types for free!](https://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt), the (weakly) final coalgebra of `X ↦ ℕ × X` can simply be defined as
```
∃ X . (X → ℕ × X) × X
```
We can construct a real number by fixing a type, `X`, giving an `X` as a seed, and then defining a method of generating new digits and new seeds from an old seed. For example, we can construct an infinite stream of zeros by setting `X` to be `⊤`, the unit type, giving `•`, the only inhabitant of `⊤`, as our seed, and defining our generator as `λ x . (0, •)`. In full, we'd have
```
0 : ℝ := λ x . (0, •), •
```
or, if we want to be explicit about all our encodings;
```
0 := λ p . p (λ x . λ p . p (λ z . λ s . z) (λ x . x)) (λ x . x)
```
This means that the real number zero has, at most, about 32.511 bits of complexity; surprisingly small for something which is supposedly infinitely large.

The usual reason we'd want to use floating-point numbers over exact real numbers is efficiency; floating-point numbers are much faster to compute with since our computers have hardware dedicated to computing with them. But this approach is representing the parameters as raw outputs of some virtual computer anyway, so that doesn't apply here. To use floating points, we'd have to convert them to some encoding of floats in our computational model. We get no efficiency in using floats here.

We can make our representation a bit more efficient. Following the [Coinductive function spaces page](http://www.dcs.ed.ac.uk/home/pgh/coit.html), we can use a few isomorphisms to change this representation. Notably, it's generally the case that
```
∃ X . (X → A × X) × X ≅ (∀ X . (1 + X → X) → X) → A
```
for any `A`. Since `ℕ` is the initial algebra over the endofunctor `X ↦ 1 + X`, the above can be rewritten as;
```
∃ X . (X → A × X) × X ≅ ℕ → A
```
So we can redefine the nonnegative reals as just functions from `ℕ → ℕ`. Neat! Following this, we can define zero instead as;
```
0 := λ n . λ z . λ s . z
```
as the constant function which just returns `0` for any input. This has only about 6.9 bits! Not bad for representing infinite many digits. It would actually be MORE complex if we truncated it to finitely many digits. We may even notice that the least complex real number will simply be encoded by the identity function. This will be the number who's continued fraction is `[0, 1, 2, 3, 4, ...]`. As it turns out, this number is
```
I₁(2)/I₀(2) ≈ 0.697775
```
where the `I`s are [Bessel I](https://en.wikipedia.org/wiki/Bessel_function#Modified_Bessel_functions:_I%CE%B1,_K%CE%B1) functions. This is called the "Continued Fraction Constant". Or, if we were interpreting the number as representing the binary expansion, this would be
```
2 - ϑ₂(1 / √2) / 2 ^ (7/8) ≈ 0.358367
```
Where ϑ is an [elliptic theta](https://mathworld.wolfram.com/JacobiThetaFunctions.html) function. I don't know if this constant has a name, but I couldn't find it anywhere. When using our previous map to turn this into the full positive reals, this becomes `≈ 0.558524`.

Anyway, my whole point with this exercise was to show we can represent real numbers, and many other mathematical structures besides, just fine on computers. We don't, and, in fact, shouldn't use floating-point numbers if we're going to take algorithmic complexity seriously. The original BDM paper mentions π a few times, saying, for instance,

> the digits of π have been shown to be [...] only algorithmic in the way they are produced from any of the many known generating formulas

so the authors know that π (and presumably other real numbers), in all of its infinite digits, can be represented by an algorithm. But in this paper, they insist on using floating-point numbers. Why? The paper just says that the parameter space becomes enumerable, but we can effectively enumerate the (constructive) reals by enumerating all the inhabitants of the type
```
ℝ := (∀ X . X → (X → X) → X) → (∀ Y . Y → (Y → Y) → Y)
```
the output of such a procedure, if it were done in some breadth-first manner, would output encodings for real numbers in essentially algorithmic order.

I think the authors need a crash course in [higher-type computability](https://www.springer.com/gp/book/9783662479919). There's a whole wide world that you're missing out on if you really believe computers can only represent discrete data types.

<a name="heading7"></a>
## Final Thoughts

The rest of the paper just goes through some usage examples. I don't feel the need to summarize them, but you may find them interesting to look at. The cellular automata classifier was a particularly good illustration. In the "hybrid machine learning" section, an interesting suggestion is to use `K` as a regularization method on top of another loss function. The same section also suggests giving training weights to simpler samples from a given space so that a model prioritizes the most plausible samples. I don't know how effective these would be, but they're interesting suggestions which may be useful as additional tools in the ML toolbox.

The conclusion section refers to the whole framework as a "symbolic inference engine" being integrated into traditional ML. I... wouldn't phrase it that way. There's not much inference and even less symbology. That being said, a type-sensitive version of these ideas might be better.

I'm not satisfied with the methods presented in this paper. Nothing in it connected back to that idea of "whichever addition increases the compressed size the least should be considered the likeliest prediction" I mentioned at the beginning of this post. All the paper said, really, was "when tuning parameters, just look through them in order from least to most complex. Also, use Kolmogorov complexity, not Shannon entropy, to measure complexity." I'll keep that in mind, but I think I'd want to look at other methods. I think something more carefully designed will need to be made for practical ML. I'll take a closer look at those evolutionary methods I mentioned. I have a strong suspicion that there is a connection between algorithmic genetic programming and [stochastic superoptimization](https://cs.stanford.edu/people/eschkufz/docs/asplos_13.pdf).

I also suspect that some variation of a bandit algorithm may benefit from these ideas. The paper [Information-gain computation](https://arxiv.org/abs/1707.01550) and its [followups](https://scholar.google.com/scholar?cites=4074844082910455437&as_sdt=805&sciodt=0,3&hl=en&scioq=Information-gain+computation+in+the+Fifth+system) described a hypothetical system which uses a contextual bandit for exploring a logic-program like search space. The expert that the bandit uses simply calculates the Kullback–Leibler divergence, a statistical entropy metric, of the history for each branch to give recommendations. The idea is that desirable histories should maximize the information gained about our goal as we go down it. A better system might use a `K` approximation rather than an entropy measure. Though, the paper also suggests using an RNN to do the history compression, which I'd expect to give a lower value than the entropy since it would be able to exploit the temporal structure of the history for compression in a way that statistical compression wouldn't be able to do.

...

THE END




