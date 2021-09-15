{% raw %}

- [Axiomatic Cohesion](#ac)

[...]

<a name="ac"></a>
## Axiomatic Cohesion

If one looks around mathematics they can find an innumerable menagerie of adjunctions. Anything which is an adjunction involving the category of sets can be used here. An interesting example typically seen outside the perview of programming is that of axiomatic cohesion. If we have a space-like category (say, topological spaces, for instance), then there is an adjoint quadruple of functors between it and the category of sets.

```
Π ⊣ D ⊣ U ⊣ C
```

`C` is the codiscrete functor; it takes a set and generates a space in the shape of a ball where all points are connected together in a maximal way. `U` is the "underlying set" functor; something like a forgetful functor. The adjunction means that continuous functions from some space `S` to a codiscrete space `C T` is the same as a conventional function from `U S` to `T`. In other words, all functions are continuous when the target is codiscrete.

`D` is the discrete functor. It takes a set and generates a discrete space where each set element is a distinct point completely separated from every other element. The adjunction means that a continuous function from the discrete space `D S` to another space `T` is the same thing as a regular function from `S` to `U T`. In other words, all functions are continuous when their source is discrete.

`Π` is a more complicated story. Over sets, it simply denotes the set of connected components. The adjunction indicates that a function from the set of connected components of a space, `Π S`, to a set `T` is the same as a continous function from `S` to the discrete space `D T`. The complication comes from the qualification "category of sets". Throughout this post, and many of my other posts, I talk about a "category of types". Most people seem to assume, if they assume anything at all, that this is a queer way of saying the category of sets. It isn't; the category of types is something like a free topos. The free topos and category of sets share nearly everything interesting in common, so thinking of one instead of the other doesn't tend to hurt. However, this is a case where it matters. If we use a different category, say the category of ∞-groupoids instead, then the adjoint quadruple would still be there. However, `Π` would not represent something far more sophisticated than the set of connected components; namely the fundamental ∞-groupoid of the space. This is not something I want to dwell on, though, as it's quite a technical distraction.

I mentioned that this adjudication holds in any "space-like category". There are lots of examples of such, many of which we wouldn't usually associate with spaces. An example which is closer to usual programming is the category of (undirected) graphs. All the above functors exist just fine there, and I can use it as a practical example.

...

[...]

It's worth wondering if this whole construction could be generalized even further. There are a few kinds of recursion schemes I can think of which don't seem to be covered by the previous construction.

The first are dependently typed folds. I talked about them near the end of my post on [godel encodings](http://anthonylorenhart.com/2021-01-08-Basic-Bijective-Godel-Encodings/). Since then I've found them in a few other places, such as the paper

- !!@!#$!@$#@ Algebra of Programming, relational ornements paper #$@%$#%^

Adding an index to the algebra, coalgebra, etc. is not hard. The fibrational form of the conjugate hylomorphism is then a simple modification of the original where the fiber is appropriately routed around. This allows the function to carry proofs and whatnot. The type of the fiber might be allowed to change through the natural transformation.

As an example of how this might be useful, we can implement quicksort in such a way that the "bag" of elements is carried as part of the fiber. During decomposition, we can construct a tree where, at each branch, a proof that all elements to the left are less than the head and a proof that all elements to the right are greater than the head could be stored at the fiber. The algebra could then combine these into and output type describing a sorted list, carrying a proof of sorting as part of the fiber as well as a bag of elements. The end type could be a function from a list with a particular bag of elements to a sorted list with that same bag of elements; thus verifying that (if the program terminates) it does, in fact, sort lists.

I'm not entirely sure if this generalization is necessary. It seems like this might be accomplished by what we already have, but it's not clear to me how. Maybe explicating adjunctions with slice categories is enough, but this seems like a function between two different slice categories, so I don't know.

[...Mendler-Style...]

Obviously we can combine the previous two generalizations rather easily, if we wanted.


{% endraw %}
