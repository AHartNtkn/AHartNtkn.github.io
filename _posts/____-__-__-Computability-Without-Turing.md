A feature is just a decompression function, while a descriptive map is a compression function.

I recently read a paper about compression that I thought was interesting; ["A theory of incremental compression"](https://arxiv.org/abs/1908.03781). I find its contents very interesting, but it maintains one of my biggest petpeves with respect to the theory of computability; its presumption of Turing machines and binary encodings throughout. To explain my problem with this, I want to ask what Turing machines have to do with computation. The most common response would probably be something like;

> Turing machines are THE model of computation, the first, and its universal. Turing machines can run any program, so they're equivalent to any model of computation; anything you say about them will apply just as well to anything else.

I imagine this will be the most common answer, but it's wrong on many levels. Firstly, Turing machines are not, historically, the first model of universal computation. The first was the SKI calculus made by Moses Schönfinkel in 1924. He didn't indent it to be a universal model of computation; that was an accident, but still. The first deliberate attempt at making a universal model of computation was done by Alonzo Church, with the lambda calculus in 1932. The Turing Machine came later in 1936. Church and Turing were both coligues, familiar with eachothers works. Turing, reaching toward universality, was able to show that the lamdba calculus and his machines were equivalent in a precise sense; evey Turing machine could be simulated by an appropriate lambda expression, and every lamdba expression could be simulated by a universal Turing machine. This led to the Church-Turing Thesis (notice that Church's name is first), the hypothesis that, beyond a certain point of expressiveness, all models of computation can simulate eachother. This does NOT mean that all such models are interchangeable. Some are harder to implement, some have different complexity properties, some are harder or easier to reason about. Turing machines are very inefficient as a model of computation, which is why real computers don't much resemble them. By presuming a specific model, you're assuming peculiarities which don't necessarily make sense in general.

Here's a point; if what you're saying is about computation in general, then anything you say should also be sayable about Cholachi Machines. Now, Cholachi Machines aren't a real model of computation, but what you say should still hold for them under the presumption that they were. By fixating on the peculiarities of any specific model, you are pretending like such peculiarities are generalities; and worse, you risk saying something that doesn't actually generalize while saying that it does.

To then end, I want to go over many of the ideas within the mentioned paper, trying to translate all the akward statements it makes about Turing machines into ones in general about computation. I'll also talk about how these ideas manifest in other, less pedestrian, models of computation.

The paper's not very long, and much of it is just going over well-tred ground, so this shouldn't take that long. The beginning is about defining a formalized notion of Kolmogorov complexity. First, we have some data, `x`, we want to compress. We have a descriptive map, `f'`, such that `f'(x) = r`, where `r` is the compressed representation of `x`. Furthermore, we have a "feature" function, `f`, such that `f(r) = x`, which decompresses the representation into `x`.

One thing to understand about this is that `x`, `f`, `f'`, and `r` are all programs within our model of computation. The paper presumes these are programs for Turing machines, and therefore are binary strings. What does it mean, exactly, to apply a function, which is a Turing machine program, to data, which is also a Turing machine program? There's no canonical answer to this questions since Turing machine programs don't act much like functions as they are normally thought about, but one standard answer, the one used by the paper, is to simply concatenate `x` with `f` to get `f(x)`, the program that computes the result of the application. This implies quite a few sophisticated things about how we have to structure our functions and data in order for this to actually work out. These functions would change dramatically if we used a different standard. Hell, they change dramatically from machine to machine. Some other models of computation, such as the lambda calculus, combinator calculi, and interaction combinators do have canonical answers; just apply the function, just apply the function, and use a function application combinator, respectively. Thinking about function application in terms of manipulating Turing machine programs is awkward and, more importantly, unnecessary for almost any purpose.

The solution offered by the paper is contained in what is, perhapse, the most anurism-enducing sentence I've ever read in a white paper.

> We will use the shortcut y(z) := U(⟨z, y⟩) defining how strings, interpreted as Turing machines (partial recursive functions), can execute other strings

Turing machines are NOT partial recusive functions; those are two totally different models of computation. If anything can be pointed to as utterly, unequivically wrong in this paper, it's this sentence right here. Partial recursive functions don't even accept binary strings as inputs, they are totally different models of computation. And partial recursive functions aren't even mentioned elsewhere in the paper, so why are the mentioned here? 

In order to define Kolmogorov complexity, the paper presumes that all data are binary strings, that

> Since there is a bijective map B → N of finite strings onto natural numbers, strings and natural numbers are used interchangeably

and that there exists a length meta-function `l` that tells us the length of our program. Note that `l` is not a program itself.

Using these, Kolmogorov complexity of `x` relative to `r` is then defined as

    K(x | r) := min_f {l(f) : U(r ++ f) ≡ f(r) = x}

Where `U` is a Turing machine of some sort. So, intuitively, the Kolmogorov complexity of `x` relative to a representation `r` is the length of the shortest function `f` which gives `x` as a feature of `r`. It's mentioned in the paper that

Firstly, that bijection, it really has nothing to do with strings. We have some model of computation, and we are assuming that there's a (bijective) Gödel numbering, `G`, over our programs such that `l(x) < l(y) → G(x) < G(y)`. This allows us to count through our programs by just counting the numbers that encode them. We don't need our programs to be strings to do this, there are [really nice encodings](https://pdfs.semanticscholar.org/1116/6d556081b34497f05ae43eddbe1997354568.pdf) for nearly arbitrary data structures nowadays. There isn't a practical benefit to having programs be strings.

On this note, there's an odd hypocrisy here. Programs are represented as strings and computation can be done in general using Turing machines, so the authors decide to talk about *all* programs as if they are strings, and *all* computation as if it were about Turing machines. But, because all programs can be represented as numbers, they will *sometimes* be talked about as one or the other.

Back on topic, that length function, what's it doin'? As in, for data in general, for arbitrary models of computation, including those that don't accept strings as programs, what is `l` supposed to mean?

For starters, what even is the length of a natural number? What's the length of 0? Well, since we're using binary, one might assume it would be 1, but no. The empty string gets mapped to 0, so the length of 0 is 0. Meanwhile, 1 has a length of 1, and 10 (2) also has a length of 1. However, 10 (the program, not the number) has a length of 2. This is so stupid. Why are these idiosyncrasies introduced in the first place? What purpose do they serve? None!

Okay, so what `l` is trying to get at, really, is the information within the data expressed as number and variety of choices made during the construction of that data. The more decisions, the longer the length. Let's consider binary strings. At each construction step, we have three choices,


1. End the string.
2. Prepend a '0' to the string.
3. Prepend a '1' to the string.


In order to construct `"0"` or `"1"`, we had to make 2 choices, while `""` only required 1. We can just count up the number of choices to get a measure of length, and it works just fine for our purposes. But let's consider a different data structure,

```haskell
data Bool
  = 0
  | 1

data Trool
  = 0
  | 1
  | 2

data BoolOrTrool
  = B Bool
  | T Trool
```

There are 5 different data of type `BoolOrTrool`,

- `B 0`
- `B 1`
- `T 0`
- `T 1`
- `T 2`

When constructing data of type `BoolOrTrool`, we have to make two decisions. If all we were measuring length using choices alone, then all terms of `BoolOrTrool` should have the same choices. But consider, if our first choice was `T`, then we have three choices, while `B` would only give us `T`. This means `T 1` carries with it more information than `B 0`.

Think about what generating randome `BoolOrTrool`s would look like. First it makes a 50/50 choice between `B` and `T`. If it picks `B`, it makes another 50/50 choice between `0` and `1`. If it picked `T`, it would then make a 33/33/33 choice between `0`, `1`, and `2`. The likelihood of `T 1` is 1/6, while the likelyhood of `B 1` is 1/4. The inverse of this is nearly an information measure, but not quite. If we wanted a measure in bits, we need to take the negative log base 2 of this. Consider, if we made 10 binary choices, that should encode 10 bits of information. The likelihood of one outcome, say "0110010100" is 1/(2^10). Taking the negative log base 2 of this we get 10, the information measured in bits. The information within `B 0` would be 2 bits, while `T 1` contains about 2.58 bits of information.

With our binary string program, the empty string contains about 1.58 bits of information while `"0"` contains about 3.17 bits of information. `l` is really a proxy for the of information the program.



With this intuition, it's not hard to calculate the information in a lambda expression. It's a bit different from before since our number of choices change over time. At any given point, one may construct a lambda expression by chosing among a new lambda binding, an application, or one of the already bound variables. Initially, there are only two choices, either bind or apply. If we have one binding, then we have three choices, bind, apply, or use the bound variable. A simple algorithm for calculating information within lambda expressions can be defined;

    C(b, λx . e) = C(b + 1, e) * (2 + b)
    C(b, x(y)) = C(b, x) * C(b, y) * (2 + b)
    C(b, v) = 2 + b; where v is a bound variable

    I(e) = log(2, C(0, e))

We can calculate that `I(λx . x)` is about 2.6 bits of information, for instance.

If we were constructing an interaction net starting at some root, we have four choices, abstraction, application, duplication, or deletion. If we abstracted, then we suddenly have the choice to merge the two dangling nodes. This means that, unlike with the lambda calculus, binding isn't local. The number of actions available to us goes up and down as we make various choices in constructing a graph, and I honestly don't know how I'd go about calculating the information within a graph in general. But I can at least say that, in the special case of the identity function, it has about 4.6 bits of information.

Reformulating, we can get a generic notion of Kolmogrov complexity be simply replacing the length with information.

    K(x | r) := min_f {I(f) : f(r) = x}

From here, the generic Kolmogrov complexity, not relative to `r`, is defined as

    K(x) = K(x | ε)

Where ε is the empty string. What, exactly, is this supposed to be? Generically, we might assume that ε represents the program with the least information. Except, that doesn't make sense in general. If we had a model of computation that accepted only non-empty binary strings as programs, then there's no distinction between `"0"` and `"1"` in this regard. Furthermore, whatever such a program does is somewhat random. In turing machines, the action of `ε` could be anything. In lambda expressions, the lowest information program is the identity function, in interaction nets its a program that deleted its input. Perhapse, `ε` is supposed to be any program `x` such that `G(x) = 0`. I don't think we can get rid of arbitraryness, but at least it can be well defined given our prior assumptions. This brings me to a little rant; data are not strings; not in general.

> But data is countable creating a bijection between strings and any form of data, so we can talk about data simply by talking about string (and presumably, coercing our predicated across the bijection at some later date).

I hear you say. Well, yes, but I dare anyone to convincingly talk about SQL databases as if they were strings. Just because you CAN talk about something by talking about something equivalent, doesn't mean you should. Firstly because it is, in general, highly non-trivial to establish equivalences between datatypes. It is a fact, though an obscure one, that there is a bijection between [trees and septuples of trees](https://arxiv.org/pdf/math/0212377.pdf) (but not pairs of trees). No one would then argue that it's perfectly fine to talk about and reason about trees as if they were septuples of trees. I shouldn't have to argue this. In practice, unless you're actually working with strings, it's pretty much universally accepted that you shouldn't represent your data as strings. Representing things like number or dates as strings rather than dedicated datatypes is often called "unclean date" in practice. Why so many computer scientists persist in the delusion that one can simply think of data as strings with no more structure is baffling to me. It limits the imagination and leads to weird, arbitrary descisions that mascurade as signifigant through the conincidenced of string formatting. The length function is not something that makes sense in general. What's worse is that it doesn't really matter. They could have said all the same things without giving pointless details about the particulars of Turing machines.

Anyway, yhe paper goes on to define the information about y in x to be;

   I(x : y) = K(y) - K(y | x)

This clashes a bit with the termonology I established earlier. This will still be a measure of bits, but modulo some notion of compression (since this is given in terms of minimal information something-or-others). But this isn't really a problem.

The paper then goes on to discuss descriptive maps; important to the underpinning ideas of the paper. Remember, descriptive maps are inverse to features. 

> We have a descriptive map, `f'`, such that `f'(x) = r`, where `r` is the compressed representation of `x`. Furthermore, we have a "feature" function, `f`, such that `f(r) = x`, which decompresses the representation into `x`.

The set of descriptive maps of `x`, given a feature `f`, is

    D_f(x) = { f' : f (f' (x)) = x ∧ l(f) + l(f'(x)) < l(x) }

Of course, I prefer to use information. This will actually prevent a problems down the line.

    D_f(x) = { f' : f (f' (x)) = x ∧ I(f) + I(f'(x)) < I(x) }

This clearifies the "compressed representation" mentioned earlier. Remember that the compressed representation `r = f'(x)`. To say that `I(f) + I(r) < I(x)` is to say that `x` is only truely compressed when both the new representation, `r`, and the method for decompression, `f`, are, in combination, smaller than `x` itself.

If `D_f(x)` is empty, if `x` isn't compressible relative to `f`, then `f` is called a "feature" of `x`. Considering how "feature" was used earlier, this is very confusing.

This is another place where the assumption of turing machines makes a technical statement in the paper out-and-out wrong.

> The so-called compression condition l(f) + l(r) < l(x) is required to avoid the case of f and f' being the identity functions, leading to the useless transformation r = id(x) = x.

Except this souldn't be a problem anyway. In no universal model of computation will `id` carry zero information. `I(id) + I(x) < I(x)` will not typically. Clearly, the authors expect the empty string to act as the identity function. This entire problem only occures with a peculiarity, not just of turing machines, but of their particular choice of function-application. In most models of computation, for most choices of function application, `I(f(x)) > I(f) + I(x)`, but they picked (possibly empty) strings, and concatonation, which means `I(f(x)) < I(f) + I(x)`. And, despite they themselves designing this problem, they act like this is just something that has to be dealt with when talking about these sorts of subjects.

K(x) + I(ε) < I(x)

There is information inherent in application. I_ap is the, increase or decrease, in the information of a pair of programs when they are applied to eachother.

I_ap = I(f(x)) - I(f) - I(x)

It's possible that I_ap is negative. For example if our programs come in the form of binary strings and application is defined as concatination, then I(f ++ x) = I(f) + I(x) - I(ε), meaning that application subtracts roungly 1.58 bits of information. This, however, is unusual. In the lambda calculus, application at the top level corresponds to a binary choice between lambda abstraction and applicaiton, meaning each additional application induces 1 bit of additional information, which is more typical.



f is a decompression function for x if there does exists a compression function f' such that f(f'(x)) = x and I(f) + I(f'(x)) + I_ap < I(x).



f* is the smallest decompression function for x. Note that f* is not the best decompression function, it's mearely the f such that I(f) is minimized, so it makes up the smallest possible proportion of I(f) + I(f'(x)).




f is a universal feature if there is a constant C, such that K(x) + I(ε) + C = I(x) implies that D_f(x) is empty, that there is no compression function for x which will then decompress back to x by f.

f is a universal decompression function if there is a constant C, such that K(x) + I(ε) + C = I(x) that there is a compression function, f', for x which will then decompress back to x by f. That is, there is f' such that f(f'(x)) = x.

f* is the smallest complression algorithm for x.






