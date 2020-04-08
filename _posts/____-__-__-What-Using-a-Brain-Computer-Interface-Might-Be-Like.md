I was reading [this](https://www.businessinsider.com/why-elon-musks-plan-to-give-us-superhuman-brains-is-doomed-to-fail-2018-11) article recently, and it gave me sone thoughts. Don't read the article; it's short, hardly tries to support its thesis, and makes factually inacurate claims on top of that. But one quote in particular from it caught my attention, this one by Philipp Heiler.

"You have to ask yourself what the advantage is over other interfaces like touchscreens or language assistants like Alexa. If an Alexa user steps out the front door, they've probably already asked the assistant to unlock the car or to carry out some other such task, so electrodes aren't really needed in the brain for this kind of thing."

It seems to me that Heiler is suffering from an extreme lack of imagination. This quote sturred up quite a few thoughts pertaining to brain-computer interfaces and how I think they ought to work in practice. Usually, discussion around BCIs focusses on hardware rather than software. This isn't unreasonable since there's a lot that needs to be done in that respect, and hardware is needed before software can work. However, if one consideres how software on a BCI should work, it will be remarkably different from what exists on smart phones or personal assistants.

This is largely written with what is likely an unrealistically optimistic outlook on the potential compatibility between the brain and various technologies, so this is a bit on the sci-fi side, but my goal is to illustrate the potential of the technology, and that always requires some level of speculation.

I think the most important technology for BCI software is something called distributional semantics. It's a technical realization of the Firth addage "You shall know a word by the company it keeps". The way it works is fairly straightforward. You take a representative collection of 50-300 words (usually verbs and adjectives like "run" or "green"), then count co-occurrences between the representatives and various other words you want to know the meaning of (usually nouns like "dog" and "queen"). After doing this, the meaning of the word is identified with that list of words, treated as a vector in a very-high dimentional vector space. The semantic difference between words is then calculated using the cosine-distance.

This may seem very basic, but it passes some nice sanity tests. For example, in [this](http://colah.github.io/posts/2014-07-NLP-RNNs-Representations/) post, an example is given where the distance between "man" and "woman", "aunt" and "uncle", and "king" and "queen" roughly coenside. It also allows one to characterize the typicality of certain attributes; for example that dogs are often brown but almost almost never green, or that cat's purr but very little else does. These sorts of things will naturally apear in the structure of the corpus used to generate the word-vectors.

This isn't the full story. Such a model doesn't adequately convey the meaning of full sentenses, for example, much less gramatical words like "at". A separate collection of tequniques called compositional semantics was used instead. The familiar [X-bar](https://en.wikipedia.org/wiki/X-bar_theory). However, far more successful in this domain are [pregroup grammars](https://en.wikipedia.org/wiki/Pregroup_grammar). I won't explain the technicalities (read the linked article if you care), but I will say it identifies the basic gramatical parsing rules of any language with a very simple algebraic structure called a pregroup. This allows for a characterazation for the gramatical meaning of a sentence, but can't tell the meaning of an individual word.

There's an important symmetry between pregroups and vector spaces; they are both special cases of a more general algebraic structure called a compact closed category. This was spelled out in [2010](https://arxiv.org/pdf/1003.4394v1.pdf), and there it's noted that the [product](https://ncatlab.org/nlab/show/cartesian+product#InAGeneraCategory) of the two structures forms a unified structure which, if inellegantly, incorperates both semantic models. Since then, better, more ellegant structures have been identified which can serve the same purpose; specifically something called a [convex space](https://golem.ph.utexas.edu/category/2018/03/cognition_convexity_and_catego.html), which offers a more holistic description of words in terms of relations of a sort (which encode the gramatical semantics) ornemented with generalized probabilistic information encoding the rest of the word's meaning.

Interestingly, this approach has recently reached into the domain of semantic databases with [this](https://arxiv.org/pdf/1811.03277.pdf) paper. While it's mostly a sketch, it hints at a future where large domains of discourse are converted into a relational database where everything carries additional semantic structure. This will be more important later.

