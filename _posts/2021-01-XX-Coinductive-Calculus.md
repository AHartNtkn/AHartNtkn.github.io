{% raw %}
Part 1: What, exactly, do you mean by "real"?

A topic I've been thourouly obsessed with over the past few months is the idea of differential programming. There's a lot to say on the subject, but, in my opinion, the topic's in a rather sorry state. Most of what you'd read on the topic is about neural networks and similar black-box tequniques. I don't find this very satisfying since I like to understand what I'm doing. My ideal is that I have a specification for what a program is meant to do, and only after the fact do I derive the algorithm from this specification. How are you supposed to do that with a neural network?

Not every technique from that domain is like that; some things seem closer to comprehensibility; lots of stuff from statistical machine learning, Bayesian methods, and arguably neural differential equations can be given something like a proper specification. There is also a lot of work on logical aspects of differential programming, namely differential linear logic and its associated methods. While a lot of that work seems promising, a lot of it is missing something important. Looking at some of the nicer work in this area like

- [The simple essence of automatic differentiation](https://arxiv.org/abs/1804.00746)

they tend to give, at best, a very nice description of the syntactic transformations which one might do during symbolic differentiation. But there's always an assumed background. There's the reals, or a comperable category and an assumed set of functions, like sin, etc. But there's a huge question which such works never answer. What, exactly, is a real number, and what are those functions?

To be clear, I'm not asking what a mathematician thinks they are. I know what a dedikind cut is, but if an algorithm is going to reference them, then what are they referencing? I understand how to represent natural numbers as an inductive datatype. What, concretely, should a real number be? And what are functions like sin, cos, etc. What specification does sigmoid satisfy? These seem like essential questions, and they never seem to get asked in the context of differential programming. Of course, they have been asked, and answered, but I only recently learned about the answers; and that's what this post is about.

I guess the first question is how we represent real numbers. I've mentioned in the past that we can represent reals exactly as streams of natural numbers. The basic observation is as follows;

- Notice that `[0, 1)` is isomorphic to `[0, ∞)` through the functions

```
         1
f(x) = ----- - 1
       1 - x

         x
g(x) = -----
       x + 1
```

- Notice to that every number in `[0, 1)` has a canonical binary representation as a series of finite sequences of 1s and potentially infinite sequences of 0s. 

For example 0 is 0.000..., 1/2 is 0.1000...., etc. 1 isn't representable since 0.111... isn't in the interval. By preventing the representation of infinite sequences on 1s, we can force a unique representation. From here, we can count how many 1s we encounter before encountering a 0 in the binary expansion of a number to get a unique list of natural numbers. For example;
```
0 = 0.000...
  = [0, 0, 0, ...]
0.1 = 0.00011001100110011001101
    = [0, 0, 2, 0, 2, 0, 2, 0, 2, ...]
0.2 = 0.0011001100110011001101
    = [0, 2, 0, 2, 0, 2, 0, 2, 0, ...]
1/3 = 0.0101010101010101010
    = [1, 1, 1, 1, 1, 1, 1, ...]
√2 - 1 = 0.011010100000100111101
       = [2, 1, 1, 0, 0, 0, 0, 1, 0, 4, ...]
π - 3 = 0.0010010000111111011011
      = [0, 1, 0, 1, 0, 0, 0, 6, 2, 1, 1, ... ]
```

This is quite an elegant solution. However, it doesn't come without its problems. As far as I can tell, there doesn't seem to be any way to define basic operations. What would addition be, for instance? The big issued come when we try adding numbers like

```
  0.011010100000100111101...
+ 0.000101011111011000010...
```

This seems like it will probably be `0.100...`, but, without looking at an infinite number of digits, it's impossible to know. There might be some way to define, not addition, but addition done on numbers interpreted as an element of `[0, ∞)`, but I couldn't find any example of this and I suspect it's not possible. If it isn't, then this approach is doomed despite its simplicity.

Another representation using sequences of natural numbers is continued fractions. Given a sequenc of numbers `[a, b, c...]`, we can calculate the number it represents as

```
1 + a + 1
        ---------
        1 + b + 1
                ---------
                1 + c + 1
                        ---
                        ...
```

This is a bit different from an ordinary continued fraction. We add 1 at each step so that `0`s contribute. We also have to deal with the fact that some numbers can be represented as multiple different fractions. For example, `2` can be represented both as

```
[2] = 2
```

and 

```
[1,1] = 1 + 1 = 2
            -
            1
```

To remove these redundancies, we can simply force a 1 to be added to the last element if it's ever reached and subtract 1 from the final result. So, with all of our modifications, the above lists would actually be

```
[2] = 1 + 2 + 1 - 1 = 3
```

and 

```
[1,1] = 1 + 1 + 1         - 1 = 4
                ---------       -
                1 + 1 + 1       3
```

we can then represent 2 as

```
[1] = 1 + 1 + 1 - 1 = 2
```

and the would-be redundancy becomes

```
[0, 0] = 1 + 0 + 1         - 1 = 1
                 ---------       -
                 1 + 0 + 1       2
```

if I explained this a bit confusedly, here's some code which implements the idea

```mathematica
NatListToRatP[{}] := 1
NatListToRatP[{a_, as___}] := 1 + a + 1/NatListToRatP[{as}]
NatListToRat[l_] := NatListToRatP[l] - 1

RatToNatListP[r_] :=
 Block[{a = Numerator@r, b = Denominator@r, div, rem},
  {div, rem} = QuotientRemainder[a, b];
  If[rem == 0,
   {div - 1},
   Flatten[{div, RatToNatListP[b/rem]}]
   ]]
RatToNatList[0] := {}
RatToNatList[r_] /; r > 0 := RatToNatListP[r + 1] - 1
```

by composing this with the functions in my last post, this gives a nice bijection between rational numbers and natural numbers

```mathematica
NatToRat := NatListToRat@*NatToNatList
RatToNat := NatListToNat@*RatToNatList
```

```mathematica
In[0]  := Array[NatToRat, {10}, 0]
In[1]  := RatToNat /@ %
```

```mathematica
Out[0] := {0, 1, 2, 1/2, 3, 1/3, 3/2, 2/3, 4, 1/4}
Out[1] := {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
```

however, if we start trying to seriously use this representation, we run into problems. Sticking to the case of finite lists, basic operations like addition are possible, but quite involved. The algorithms for such were worked out in the by Bill Gosper in

- [Continued Fraction Arithmetic](https://perl.plover.com/classes/cftalk/INFO/gosper.txt)

however, despite what that post says, the algoithms are very complicated. One shouldn't have to calculate a determinant in order to add two numbers. To get a unique representation, we'd then have any irrational number be represented by an infinite continued fraction, and no rational would be represented by such. When we come to this realization, it becomes immediate that basic operations are now impossible. Consider the irrational number √2 which would be represented by an infinite sequence. If we multiplied √2 by itself, it should become 2, represented by a finite list. But it would be impossible to know this without first seeing all infinite entries in √2. Since 2 can't be represented by an infinite fraction, we are, once again, doomed.

Both of these methods are described in some detail in

- [The continuum as a final coalgebra](http://it.mmcs.sfedu.ru/_files/ct_seminar/articles/The%20continuum%20as%20a%20final%20coalgebra.pdf)
- [On coalgebra of real numbers](https://pdf.sciencedirectassets.com/272990/1-s2.0-S1571066100X02757/1-s2.0-S1571066105802725/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEA0aCXVzLWVhc3QtMSJHMEUCIQCsO8jPY04ZaVm4nRU8BR%2BwzEf4YwEE98c%2B5GR0YsQoMQIgXNlY9EQ7UXVF6wGrtKTNigqCXB55xdN5pH1PP2Qd%2FZYqvQMI5v%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FARADGgwwNTkwMDM1NDY4NjUiDCEhJMV1wQ4D0KppoiqRAxkwzbXvKkY2qrRjLMnVbMSGlds0hNNwr1eUnN76Mdnuq8chB23LH5fAvKBAihPhcgtkt27szVIWRn%2BjpOY5vlNt3wsf0LomXSnaHyEJ1Iqlcio1ucFAkQG%2BNEnVf2bVNgXOOhNa7GKyT9cL0zE%2BLEKuhqtz6MLT3C5smeSxWGA1FY3CRCKqyXOnDreEnKvcunr4vxPS7x2ezJHDYqk7g4cqJ4DNJPMSyaGeraPuqCwXL2gw3dNmBVRsw3mF3BMqsf6ogTR3ti33kqtJ24mQRor2hME6oeT1zpWbxPPmbcXncAjeCShYMSDLLIWShC%2FfuXdyHgW1rVQh%2FmZ3D%2FMZjB9T5hK8x4EpoepUizRRsvFMBdgCEANGjb0%2B8Ic5PGMbYg870MTIDxqlAvNFd%2Fkl0rrCMU6qwKzhvsfF3LPyi4qJ2EHxmZKEL1a%2BUdFupHfPT3sCz7RPTK%2BfDUwmI8rpFT2HNhGDBdz6J2ztnlm8h1IMfQCrjAtFFfWBHBwvpDdAIXh7No7YoflxUR5Gg62ZpTuhMOXts4AGOusBOizUu1Tg9U4q9ub4faeV9O62cIbLR6zffXzK5qAzTytZBNO0bYHjOYryHFrw6zPBd1y%2FBx7C49JHfHvaMynfVu5swyPrlW3iJ77Zre1%2FnlXiFlKGh7acbu2VFTC7NTpaQ2qncUDS7zBizBiXzQTWcobSRTF4kg%2BrXGip2C2P8%2F48FQqm0BR9hF1FVsRmH6Fv3oiUh%2FxblCj61wrJw8nm1VtIvNtOP1URgCfSWZVrEl%2BUZXMGcPgYRJpBG6aKUiuEtyizdqHVm07tv7H9sQqmvINTqD3FQpgOJlSINIpQzJCUtNrXwr6v4I4o1Q%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20210124T055216Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYXWQZWIHU%2F20210124%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=a4c862bdc01087a45b6f8d1c0bcf11cf06da3add289b63c1eb68b0a9b87fbf41&hash=d2792a7b87372717e392ddb8012f912008b007c009bb8a4b3b532c7c24e24ec7&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S1571066105802725&tid=spdf-7992097f-7302-4755-9ca5-f332aa6ab8e7&sid=d06e5c1114dab04a9e4b8212ceab586a6121gxrqa&type=client)

So, where do we go from here? While I hold out hope that a perfect representation of the reals exists, I haven't found one that's usable in the literiture. Instead, we can use notations with redundancy. There are actually a lot of options. The most common is the signed bit representation, described in

- [Coinductive Definitions and Real Numbers](https://www.doc.ic.ac.uk/teaching/distinguished-projects/2009/m.herrmann.pdf)

It represents the closed interval `[-1, 1]` as a list of 1s, 0s, and -1s. If we look at a sequence, we hold in memory an interval with a lower and upper bound. These bounds are always rational numbers. If we see a 0, we focus on the middle half of the interval. If we see a -1, we focus on the left half. If we see a 1 we focus on the right half. For example, if we saw
```
[1,1,0,-1,1,0...]
```

We'd start with `[-1, 1]`. Since our first digit is a 1, we focus on the second half of the interval, obtaining `[0, 1]`. The next is a 1, so we focus on the second half again getting `[1/2, 1]`. We next have a 0. In general, when we have an interval `[a, b]`, we can define focusing as

```
f([a, b], 0)  := [(3 a + b)/4, (a + 3 b)/4]
f([a, b], -1] := [a, (a + b)/2]
f([a, b], 1]  := [(a + b)/2, b]
```

following that, we'd have `[5/8, 7/8]` as our next interval. We then have -1, instructing us to focus on our first half, getting `[5/8, 3/4]`. We then have a 1 for `[11/16, 3/4]`, and finally a 0 for `[45/64, 47/64]`. At each stage, we get progressively closer and closer to our answer.

This new representation helps us due to the overlaps. Consider the following scenario where we need to decide which signed bit to use when representing an interval. Up to now, we've narrowed our inteval to between a and b.

```
        0
a   |-------|   b
|-------|-------|
   -1      1
  (-----------)
```

the parentheses represent the interval we're classifying. At this point, it could be in any one of the three. We can make an observation about the number which will shring the bounds. If the left keeps shrinking on each observation, it will eventually cross the 0 threshold. If it keeps going, it will cross the other, making 1 the only possibility. If th right does this, then -1 will be the only possibility. If both sides shrink, then the number will eventually be between the two thresholds of 0, making that our next digit.

In a perfect representation, if our number was exactly 0 then our number bounds would shrink forever, never crossing into either 1 or -1. By having the overlap, we gaurantee that we will always will eventually be able to output a digit at the cost of numbers having non-unique representations.

This representation does have a few nice properties. If a number is negative, then it will have a unique representation as a sequence made purely of -1 and 0; similar is true of positive numbers with 1 and 0. We may observe that `[-1, 1]` is isomorphic to `[-∞, ∞]` via the following;

```mathematica
expand[0] := 0
expand[1] := ∞
expand[x_] /; x > 0 := 1/(1 - x) - 1
expand[-1] := -∞
expand[x_] /; x < 0 := 1 - 1/(1 + x)

contract[0] := 0
contract[∞] := 1
contract[x_] /; x > 0 := x/(x + 1)
contract[-∞] := -1
contract[x_] /; x < 0 := x/(1 - x)
```

Under this representation, some operations become extremely trivial. For example, given a positive number represented as a sequence of 1s and 0s, we can invert the number by swapping the 1s and 0s.

```
contract[22] = 22/23
  = [1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1...]
contract[1/22] = 1/23
  = [0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0...]
```

Similarly, a negative number represented as a sequence of -1s and 0s can be inverted by swapping the 0s and -1s.

```
contract[-14/57] = -14/71
  = [0, 0, -1, -1, 0, 0, -1, 0, 0, -1, -1, -1, -1, 0, -1, 0, -1, 0, 0, -1...]
contract[-57/14] = -57/71
  = [-1, -1, 0, 0, -1, -1, 0, -1, -1, 0, 0, 0, 0, -1, 0, -1, 0, -1, -1, 0...]
```

That's pretty cool! However, this is not the representation I'll be using for the rest of this post. Instead, I want to use something simpler. A modification to the digned bit representation is to just use two overlapping intervals and just use bits. Something like

```
        1
a   |-------|
|-------|   b
    0
```

This would, seemingly, have the same properties while being slightly simpler than the signed bit represenations. In all my reading, I never came across this. The type itself is well-known. Infinite lists of binary digits are the cantor space; but using them like this is something I haven't been able to find. We can define a new focusing function

```
f([a, b], 0) := [a, (a + 3 b)/4]
f([a, b], 1] := [(3 a + b)/4, b]
```

We do have some additional options. As long as there's an overlap at all, we're gauranteed to eventually decide on a bit given enough observations. We could narrow this overlap acording to the formula

```
f([a, b], 0) := [a, ((2^n + 1) b + (2^n - 1) a)/2^(n+1)]
f([a, b], 1] := [((2^n + 1) a + (2^n - 1) b)/2^(n+1), b]
```

The larger we set n, the smaller the overlap. If n is set to 1, the overlap will be between -1/2 and 1/2. If n is set to 2, the overlap will be between -1/4 and 1/4. For general n, the overlap will be between -1/2^n and 1/2^n. This will have the effect of increasing the rate of convergence; the intervals will get smaller, faster the more digits we see. Though, there are diminishing returns and one should be careful not to set n so high that basic operations become extremely inefficient. I'll use n = 1 since I want to keep things simple for this post.

This representation does have the nice property that finding the negative of a number corresponds to swapping the 1s and 0s, but there doesn't seem to be a trivial way to ivert a number, so I'll have to think of something more clever, though it honestly isn't difficult in general.

I'll start actually implementing things. Since we're using infinite data structures, I'll use Haskell which is designed for exactly this sort of usage. We'll start by defining real numbers as streams of booleans;

```haskell
type ℝ = [Bool]
```

To reiterate a point from earlier; this isn't theoretically correct. What this actually defines is the Cantor space, but I'll be treating it like the reals.

And yes, in case you were wondering, Haskell doesn't complain about the unicode.

An important function for manipulating streams will be `ana`, short for "anamorphism". This is just an unfold operation without a base-case since it's going to be used to define infinite data structures.

```haskell
ana :: (a -> (b, a)) -> a -> [b]
ana f a =
  case f a of
    (b, a') -> b : ana f a'
```

using this, we can define a function which converts a `Rational`, Haskell's built-in representation of fractions, into a `ℝ`. Before that, let's define our focusing function. I've hard-coded a power of 1, but you can feel-free to vary it.

```haskell
type Interval = (Rational, Rational)

power = 1
focus :: Interval -> Bool -> Interval
focus (fl, fr) False =
  (fl, ((2^power + 1) * fr + (2^power - 1) * fl) / 2^(power + 1))
focus (fl, fr) True = 
  (((2^power + 1) * fl + (2^power - 1) * fr) / 2^(power + 1), fr)
```

I will also need the expand and contract functions since we'll be going back and forth between `[-1, 1]` and `[-∞, ∞]`. Since Haskell's `Rational` type doesn't have point's at infinity. I'll devise a new type with points at infinity along with the somewhat tedius `Num` and `Fractional` instances so I can use the standard numerical operations. The only thing interesting about is is how the infinities are handled. They are defined to make the most conservative conclusions when combining intervals which might have infinities on either side. 

```haskell
data RationalInf
  = Rat Rational
  | Inf
  | MInf
  deriving (Eq, Show)

instance Num RationalInf where
  Rat a + Rat b = Rat (a + b)
  a + Rat b = a
  Rat a + b = b
  Inf + MInf = Rat 0
  MInf + Inf = Rat 0
  a + b = a

  Rat a * Rat b = Rat (a * b)
  Inf * Rat b | b >= 0 = Inf
              | True = MInf
  MInf * Rat b | b >= 0 = MInf
               | True = Inf
  Rat b * Inf | b >= 0 = Inf
              | True = MInf
  Rat b * MInf | b >= 0 = MInf
               | True = Inf
  Inf * MInf = MInf
  MInf * Inf = MInf
  MInf * MInf = Inf
  Inf * Inf = Inf

  abs (Rat r) = Rat (abs r)
  abs a = Inf

  signum (Rat r) = Rat (signum r)
  signum Inf = Rat 1
  signum MInf = Rat (-1)

  fromInteger i = Rat (fromInteger i)

  negate (Rat a) = Rat (negate a)
  negate Inf = MInf
  negate MInf = Inf

instance Fractional RationalInf where
  fromRational r = Rat r

  recip Inf = Rat 0
  recip MInf = Rat 0
  recip (Rat r) = Rat (recip r)
```

The expand and contract functions are then fairly simple.

```haskell
expand :: Rational -> RationalInf
expand r | r == -1 = MInf
         | r == 1  = Inf
         | r < 0   = Rat $ 1 - 1/(1 + r)
         | r == 0  = Rat 0
         | r > 0   = Rat $ 1/(1 - r) - 1

contract :: RationalInf -> Rational
contract MInf = -1
contract Inf  = 1
contract (Rat r) | r < 0  = r/(1 - r)
                 | r == 0 = 0
                 | r > 0  = r/(r + 1)
```

Using these, we can define the coalgebra which generates the appropriate real given a fraction. There are a couple of different ways of implementing it. I decided to initially try focusing to the left. If this focus is too small, then we know the input must be in the other interval and we focus to the right instead. If our left focus isn't too small then we can use it, though we may not *have* to use it if the input is actually in both intervals. Depending on if we test the left or right focus, we can generate different sequences which represent the same number. The larger our `power`, the less of a difference this makes.

```haskell
ratToRealCoalg (f, n) = 
  let fl = focus f False
  in if n > snd fl
     then (True, (focus f True, n))
     else (False, (fl, n))

ratToReal :: Rational -> ℝ
ratToReal r = ana ratToRealCoalg ((-1, 1), contract r)
```

Going the other direction, we can just fold to convert a real back into a rational number. We can't get the rational number exactly, we can only get an interval. I decided that I'll return the upper approximation. We also can't fold forever, so I decided to cut off after 100 bits which should be more than enough precision.

```haskell
realToFloat :: ℝ -> Float
realToFloat =
  fromRational . (\(Rat r) -> r) . expand .
  snd . foldl focus (-1, 1) . take 100
```

```haskell
> realToFloat $ ratToReal $ 1/2
0.5
> realToFloat $ ratToReal $ 2
2.0
> realToFloat $ ratToReal $ -33
-33.0
> realToFloat $ ratToReal $ 0
3.3331495e-14
```

That seems to have worked out well. We're now in a position to write our first actual function over reals, as simple as it might be. I mentioned before that if we swap bits in our representation we get the negative.

```haskell
realNegate :: ℝ -> ℝ
realNegate = map not
```

```haskell
> realToFloat $ realNegate $ ratToReal $ -43
43.0
> realToFloat $ realNegate $ ratToReal $ 20
-20.0
> realToFloat $ realNegate $ ratToReal $ 1/3
-0.33333334
> realToFloat $ realNegate $ ratToReal $ 0
6.0810894e-13
```

Part 2: Let's be rational about this

From here we can start generating some basic functions. Where possible, the easiest functions to implement are those which are already defined over the rationals. To accomplish this, we can create an intermediate representation of a real in terms of a sequence of nested intervals. This representation itself could be used as a definition for the reals as detailed in 

- [PCF extended with real numbers](https://www.cs.bham.ac.uk/~mhe/papers/realpcf.pdf)

Getting a list of nested intervals out of a real is very simple; we just scan with `focus`.

```haskell
realToInts :: ℝ -> [Interval]
realToInts = scanl focus (-1,1)
```

Let's start with addition. Using an operation which adds two intervals, we can simply calculate streams of successive interval approximations of the addition from interval approximations of the input numbers.

```haskell
intAdd :: Interval -> Interval -> Interval
intAdd (a1, a2) (b1, b2) =
  let m a b = contract (expand a + expand b)
  in (m a1 b1, m a2 b2)
```

the actual stream of intervals produced by addition arises out of a straightforward zipping.

```haskell
addInts :: [Interval] -> [Interval] -> [Interval]
addInts = zipWith intAdd
```

getting from a stream of intervals to a real is a more complicated business. It's another coalgebraic construction, but we need to reason a bit more carefully so as to ensure any focus contains our number up to whatever interval approximation we're looking at. If the largest our number can be, `big`, is smaller than the top of our left subinterval, `topL`, then we can use 0/`False` as our next bit. If the smallest our number can be, `low`, is larger than the bottom of our right subinterval, `botR`, then we can use 1/`True` as our next digit. If either of these conditions is not met, then we look at the next step in our interval stream to see if the next one is more specific.

```haskell
intsToRealCoalg :: (Interval, [Interval]) -> (Bool, (Interval, [Interval]))
intsToRealCoalg (f, is) = 
  let (botL, topL) = focus f False
      (botR, topR) = focus f True
      iss = dropWhile (\(low, big) -> big > topL && low < botR) is
  in if fst (head iss) > botR
     then (True,  ((botR, topR), iss))
     else (False, ((botL, topL), iss))

intsToReal :: [Interval] -> ℝ
intsToReal i = ana intsToRealCoalg ((-1, 1), i)
```

we can combine our operations to define real addition

```haskell
realSum :: ℝ -> ℝ -> ℝ
realSum r1 r2 = intsToReal $ addInts (realToInts r1) (realToInts r2)
```

```haskell
> realToFloat $ realSum (ratToReal 11) (ratToReal 9)
20.0
> realToFloat $ realSum (ratToReal (1/3)) (ratToReal (2/3))
1.0
> realToFloat $ realSum (ratToReal (-8)) (ratToReal 4)
-4.0
> realToFloat $ realSum (ratToReal (-26)) (ratToReal (-9))
-35.0
> realToFloat $ realSum (ratToReal (-8)) (ratToReal 8)
1.8856775e-13
```

now that we have `intsToReal`, any operation which is defined on `Rational` can be straightforwardly extended to the full reals. Multiplication seems like an obvious choice. The only real complication is multiplying intervals. Depending on the values and the signs of the two ends of the intervals, the lower and upper bounds could vary quite a bit. We must calculate all four possibilities and pull out the smallest and largest.

```haskell
intTimes :: Interval -> Interval -> Interval
intTimes (a1, a2) (b1, b2) =
  let m a b = contract (expand a * expand b)
      l = [m a1 b1, m a1 b2, m a2 b1, m a2 b2]
  in (minimum l, maximum l)

intsProds :: [Interval] -> [Interval] -> [Interval]
intsProds = zipWith intTimes

realProd :: ℝ -> ℝ -> ℝ
realProd r1 r2 =
  intsToReal $ intsProds (realToInts r1) (realToInts r2)
```

```haskell
> realToFloat $ realProd (ratToReal (3/2)) (ratToReal (2/3))
1.0
> realToFloat $ realProd (ratToReal (1/2)) (ratToReal 100)
50.0
> realToFloat $ realProd (ratToReal 400) (ratToReal 0)
5.246011e-14
> realToFloat $ realProd (ratToReal 15) (ratToReal (-3))
-45.0
> realToFloat $ realProd (ratToReal (-2/5)) (ratToReal (-35))
14.0
```

What about the reciprocal? If we try to reproduce the procedure we just did for addition and multiplication we run into a serious issue. If we just reciprocate both sides of the interval, we get a sequence which begins with `(0, 0)`, since 0 is the reciprocal of infinity. The intervals then grow for a bit before eventually settling down. This means we need to drop an initial segment of our interval. As far as I can tell, dropping the first 3 entries guarantees correctness, but I'm honestly not sure why. Beyond that, the obvious implementation going through expanding and contracting doesn't work for some reason. If we go through the algebra to simplify that triple composition, we find out that it's the same as `1-x` when `x` is positive and `-1-x` when `x` is negative. A similar calculation can be done for addition and multiplication, but the results are much more complicated. In this case, we get a simpler expression, and this does work. Unfortunately, some of this is a bit of a hack job. Hopefully, I'll be able to justify something better in the future.

```haskell
intRecip :: Interval -> Interval
intRecip (a, b) =
  let ra = signum a - a
      rb = signum b - b
  in if ra < rb
     then (ra, rb)
     else (rb, ra)

intsRecip :: [Interval] -> [Interval]
intsRecip = drop 3 . map intRecip

realRecip :: ℝ -> ℝ
realRecip = intsToReal . intsRecip . realToInts
```

```haskell
> realToFloat $ realRecip $ ratToReal 8
0.125
> realToFloat $ realRecip $ ratToReal 3
0.33333334
> realToFloat $ realRecip $ ratToReal 2
0.5
> realToFloat $ realRecip $ ratToReal (1/2)
2.0
> realToFloat $ realRecip $ ratToReal (1/3)
3.0
> realToFloat $ realRecip $ ratToReal (2/3)
1.5
> realToFloat $ realRecip $ ratToReal (3/2)
0.6666667
> realToFloat $ realRecip $ ratToReal 1
1.0
> realToFloat $ realRecip $ ratToReal $ -1
-1.0
> realToFloat $ realRecip $ ratToReal $ -1/2
-2.0
> realToFloat $ realRecip $ ratToReal $ -2
-0.5
> realToFloat $ realRecip $ ratToReal $ -8
-0.125
```

We almost have all the neccessary components for instances of `Num` and `Fractional`. All we need is a definition of `signum` and `abs`. To make these, I'll define a function which tests for positivity over intervals.

```haskell
positiveInt :: [Interval] -> Bool
positiveInt ((l, r) : is) | l > 0 = True
                          | r < 0 = False
                          | True  = positiveInt is

positiveReal :: ℝ -> Bool
positiveReal = positiveInt . realToInts

realAbs :: ℝ -> ℝ
realAbs r = if positiveReal r then r else realNegate r

realSignum :: ℝ -> ℝ
realSignum r = if positiveReal r then ratToReal 1  else ratToReal (-1)
```

And we can make our instances so we can use the nice interfaces.

```haskell
instance Num ℝ where
  (+) = realSum
  (*) = realProd
  abs = realAbs
  signum = realSignum
  fromInteger = ratToReal . fromInteger
  negate = realNegate

instance Fractional ℝ where
  fromRational = ratToReal
  recip = realRecip
```

These calculations are now actually using the functions I've implemented to do their calculations.

```haskell
> realToFloat $ signum $ 22
1.0
> realToFloat $ signum $ -22
-1.0
> realToFloat $ signum $ 0.01
1.0
> realToFloat $ signum $ -0.01
-1.0
> realToFloat $ abs $ -0.01
1.0e-2
> realToFloat $ abs $ -1
1.0
> realToFloat $ abs $ -22
22.0
> realToFloat $ abs $ 22
22.0
> realToFloat $ abs (22 - 45)
23.0
> realToFloat $ 4 - abs (22/45)
3.511111
```

Part 3: Linear Thinking Can Take You Only So Far



{% endraw %}
