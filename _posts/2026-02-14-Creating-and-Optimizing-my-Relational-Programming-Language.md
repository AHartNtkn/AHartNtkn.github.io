About a year ago, I had an idea for a relational programming language. I created a simple prototype for it back then, but it was too inefficient for interesting work. Recently, I finished a version of it which is several hundred times faster and more memory efficient. In this post, I'll describe the language, give a few demonstrations, then describe the methods I used to make it more efficient by automating optimization research using Claude's experimental (at the time of this writing) agent feature. You can find the current implementation [here](https://github.com/AHartNtkn/rwLog-Rust/).

# The Core Language

The original concept for the language was to do relational programming via rewriting. This was mainly inspired by the [Relational Machine Calculus](https://arxiv.org/abs/2405.10801), itself a simple system for relational programming. But I wasn't satisfied with the RMC; it was more complicated than it needed to be, and wasn't completely pure in terms of what I imagine when I think of pure, mathematical relations.

The ultimate idea was to create a system with a few basic relations, and rewrite them algebraically to produce a normal form. This normal form is a (potentially infinite) union of relations I call "pattern spans"; in the language they'd be written as something like

```
(f (g $x) (h $y $x)) -> (r $y (t $y) $z)
```

as an example. The `$x` terms are variables which can be matched with anything. This relation relates anything matching the first pattern with anything matching the second, such that shared variables map to the same thing on both sides. This can be thought of as a relational generalization of first-order lambda expressions, and is somewhat reminiscent of binders in the [rho/rewrite calculus](https://inria.hal.science/inria-00072840).

Eventually, I decided that I need constraints too, and they should live in pattern spans. So, optionally, additional constraints can be added to a span, for example

```
(f (g $x) (h $y $x)) {(neq $x $z), (pos $y)} -> (r $y (t $y) $z)
```

The constraints are just a standard [constraint handling rules](https://en.wikipedia.org/wiki/Constraint_Handling_Rules) database.

It's intuitively obvious that these pattern spans can be composed with each other, from the left or the right. Given a span `A -> B` and `C -> D`, we identify a most general matcher, in the form of a pair of substitutions, s1 and s2, that make `B[s1] = C [s2]`. We then (modulo variable renaming) return `A[s1] -> D[s2]`, which is the composition of `A -> B` and `C -> D`. In the language, we denote the composition of `R` and `S` as `R ; S`. Variable names are arbitrary and are only scoped to within a single pattern span; that's why we do matching instead of unification. Even if the same name is used, variables are never shared between spans. The constraint stores are conjoined after variable substitution and normalized to get a new store.

To this basic framework, I add three other operators.

1. Intersections, denoted `R & S`
2. Unions, denoted `R | S`
3. Recursive definitions

The recursive definitions are denoted on the top level with the `rel` keyword, the name of the relation, and the definition (which can refer to the name itself) in curly braces. For example, a standard unary addition example can be implemented as

```
rel add {
      (pair z $b) -> $b
    | [(pair (s $a) $b) -> (pair $a $b) ; add ; $c -> (s $c)]
}
```

the system also has no enforced scoping for recursive names, so mutual recursion works as well;

```
rel even {
    z -> true
  | (s $n) -> $n ; odd
}

rel odd {
    z -> false
  | (s $n) -> $n ; even
}
```

These, collectively, have an unambiguous intended semantics in terms of pure, mathematical relations, and it's sufficient to reproduce essentially everything that one would want out of an ordinary logic or relational language like MiniKanren or Prolog. However, it doesn't require fiddling with fresh variable creation, and, unlike those languages, the bound scope of variables means there's never any "action at a distance" as a consequence of unification.

With the unambiguous semantics, one of my goals was to create a language such that, anything that made sense semantically should "just work". Obviously, there are limits to this, in practice, but I think I've gotten practically as close to this dream as I ever expected was possible.

# Basic Examples

To demonstrate usage, we can "run" the addition relation on a fixed input by precomposing with the identity relation on that input (denoted with the sugar `@t`, which is the same as `t -> t`)

```
@(pair (s (s (s z))) (s (s z))) ; add
```

This will return

```
1. (pair (s (s (s z))) (s (s z))) -> (s (s (s (s (s z)))))
```

the first pattern span in the normal form. We can ask for further spans with `next`. In this case, it returns `No more answers.`.

We can, instead, fix the output with

```
add ; @(s (s (s (s (s z)))))
```

this will return

```
1. (pair z (s (s (s (s (s z)))))) -> (s (s (s (s (s z)))))
```

we can ask for the `next 10` answers, getting

```
2. (pair (s z) (s (s (s (s z))))) -> (s (s (s (s (s z)))))
3. (pair (s (s z)) (s (s (s z)))) -> (s (s (s (s (s z)))))
4. (pair (s (s (s z))) (s (s z))) -> (s (s (s (s (s z)))))
5. (pair (s (s (s (s z)))) (s z)) -> (s (s (s (s (s z)))))
6. (pair (s (s (s (s (s z))))) z) -> (s (s (s (s (s z)))))
```

and then exhausting.

We, of course, don't need either the input or the output to be fully ground. We can make a pattern span with the input having a free variable;

```
@(pair $x (s (s (s z)))) ; add ; @(s (s (s (s (s z)))))
```

In this case, the `$x` will be the result of 5 - 3;

```
1. (pair (s (s z)) (s (s (s z)))) -> (s (s (s (s (s z)))))
```

With a bit of manipulation, we can turn addition into the subtraction relation. First, it's worth noting that the relational dual can be defined in this system. Given a relation, `R`, we can write

```
(pair $a $b) -> $a ; R ; $b -> (pair $a $b)
```

Note that `$b` is completely free in `(pair $a $b) -> $a` and `$a` is free in `$b -> (pair $a $b)`. So this has the denotation

```
{((pair $a1 $b1), (pair $a2 $b2)) | $a1 R $b2}
```

with only the diagonal constrained. If we take the intersection of this with the identity relation,

```
[(pair $a $b) -> $a ; R ; $b -> (pair $a $b)] & $p -> $p
```

this now denotes

```
{((pair $a $b), (pair $a $b)) | $a R $b}
```

We can then throw away the first component of the first pair and the second of the second pair;

```
$b -> (pair $a $b)
  ; [[(pair $a $b) -> $a ; R ; $b -> (pair $a $b)] & $p -> $p]
  ; (pair $a $b) -> $a
```

to define the dual of `R`. Right now, the system doesn't support macros, but, if it did, this could be defined generically for any relation.

This was just a preamble to the intuition for turning addition into subtraction. We want a relation that takes a pair, $c and $a, and returns a $b such that $a + $b = $c. Start with

```
[(pair $a $b) -> $a ; add ; $b -> (pair $a $b)] & $p -> $p
```

Then throw away the second argument up front and extract it from the back;

```
rel sub {
  (pair $c $a) -> (pair (pair $a $b) $c)
     ; [[(pair $a $b) -> $a ; add ; $b -> (pair $a $b)] & $p -> $p]
     ; (pair (pair $a $b) $c) -> $b
}
```

we can now run this like we did addition;

```
@(pair (s (s (s (s (s z))))) (s (s (s z)))) ; sub
```

which returns

```
1. (pair (s (s (s (s (s z))))) (s (s (s z)))) -> (s (s z))
```

# Specs and Testing

One of the first things I did before implementing the language was spelling out the denotational semantics. I don't want this post to be too math heavy, so I will just say it's not that complicated and it gives a fully formal definition of correctness. I worked out, by hand, and implemented in Haskell an intuitive set of rewrite rules for evaluation, and I proved those rewrites sound with respect to the denotational semantics in Lean.

Having a clear, unambiguous standard for correctness is extremely important and extremely rare in software. I used this information to communicate the intent behind the language to LLMs, who did most of the actual coding. A good idea if you want a reliable end result is to specify, up front, what you want. Then build out the full API of the program with stubbed interfaces. These interfaces then must satisfy semantic contracts which are, ideally, specified declaratively via tests, especially prop tests, if possible. These tests should fail until the implementation is made. This is test driven development, and is extremely effective when working with AI.

Modern AIs are overzealous in that they want to start immediately, so you have to pull on the reins a bit and be willing to trash any work that wasn't done under the specific workflow you specified. You want the tests to emerge during planning, and the implementation to conform to the tests. Allowing the AI to conform the tests and implementation to each other can be dangerous. Older versions of Claude LOVED to modify tests to pass, because it didn't want to see errors. The current version is less likely to do this, but it makes other mistakes in the same paradigm, such as making a test that passes if and only if a specific bug is present; the opposite of what one wants. There are other issues I've seen, related to testing the implementation rather than the semantics. This locks in specific details that are not directly related to correctness. TDD makes sure there isn't actually an implementation to test up front, making proper semantics-based testing more likely to actually be implemented.

Full formal verification would be even better, but this is still too difficult to do quickly, especially for optimized code. LLMs have already sped up formalization significantly, but not enough to use it as a drop-in replacement for unit testing. As is typical, there were several bugs left after the optimizations that only surfaced when I started creating more elaborate examples. For example, a rare name collision w/ term hashing caused some constraints to be deleted through misidentification, but this only happened rarely on specific examples. Maybe fuzzing would have caught it?

Are all of my language tests properly formed by this standard? Probably not, but I have pushed the test suite more in this direction as development has gone on. Some of the tests were made through a project I gave Codex, asking it for a full coverage semantic test suite suitable for full reimplementation. It seemed to do a decent job.

All this is to say, if you want predictable results, you have to actually make a prediction and phrase/construct this prediction in the form of a concrete oracle.

# Synthesis; More substantial examples

The actual reason I wanted to make the language is to have a more intuitive setting for setting up automated reasoning and synthesis tasks. I will demonstrate this using the SK combinator calculus. To start off, we can define a, more or less, typical spine-unfolding SK-combinator evaluator;

```
theory sk_constraints {
    constraint no_c/1

    # no_c/1 rejects terms containing (c N).
    (no_c k) <=> .
    (no_c s) <=> .
    (no_c (a $x $y)) <=> (no_c $x), (no_c $y).
    (no_c (c $n)) <=> fail.
}

rel skEval {
    # Base Cases
    (p (c $i) $spine) -> (p (c $i) $spine) ; skFold
  | (p k nil) -> k
  | (p k (cons $x nil)) -> (p $x nil) ; skEval ; $x -> (a k $x)
  | (p s nil) -> s
  | (p s (cons $x nil)) -> (p $x nil) ; skEval ; $x -> (a s $x)
  | (p s (cons $x (cons $y nil))) -> (p (p $x nil) (p $y nil)) ; [
       [(p $x $y) -> $x ; skEval ; $x -> (p $x $y)] &
       [(p $x $y) -> $y ; skEval ; $y -> (p $x $y)]
    ] ; (p $x $y) -> (a (a s $x) $y)
    # Strip Application Spine
  | (p (a $x $y) $spine) -> (p $x (cons $y $spine)) ; skEval
    # K x y -> x
  | (p k (cons $x (cons $y $spine))) -> (p $x $spine) ; skEval
    # S f g x -> (f x) (g x)
  | (p s (cons $f (cons $g (cons $x $spine)))) 
      -> (p $f (cons $x (cons (a $g $x) $spine))) ; skEval
}

rel skFold {
    (p $t nil) -> $t
  | (p $t (cons $x $spine)) -> (t $t $x $spine) ; [
        [(t $t $x $spine) -> (p $x nil) 
          ; skEval
          ; $x -> (p (a $t $x) $spine)] &
        [(t $t $x $spine) -> (p (a $t $x2) $spine)]
    ] ; skFold
}
```

There are a few things specific to this implementation that are worthy of note. For one are the uninterpreted constant symbols, `(c $i)`, which halt computation and are used for specifying some things we want to synthesize, and the constraint which we can use to make sure none of the results actually have these constant symbols. Second, we have to make sure patterns don't overlap in order to make sure the evaluator is deterministic. We can't rely on any "match the first pattern, fall through on failure" mechanism that might be used in, for example, Haskell. Something similar has to be done when pattern matching in the [Curry](https://curry-lang.org/) functional-logic programming language. Apparently, some users have complained about this; here, due to the relational semantics, it's more obvious why you have to do this.

After we have this, we can run the evaluator forward;

```
@(p (a (a (a s k) k) (c z)) nil) ; skEval
```

This returns

```
1. (p (a (a (a s k) k) (c z)) nil) -> (c z)
```

Indicating that `(a (a (a s k) k)` is an identity function, returning its input unmodified. We can also run this backwards. If we want to synthesize a duplication combinator, `D x y = x y y`, we can run

```
$p { (no_c $p) } -> (p (a (a $p (c z)) (c (s z))) nil) ; skEval ; @(a (a (c z) (c (s z))) (c (s z)))
```

this will return

```
1. (a (a s s) (a s k)) -> (a (a (c z) (c (s z))) (c (s z)))
```

We can ask for more with `next 10`, returning

```
2. (a (a k (a (a s s) (a s k))) $0) { (no_c $0) } -> (a (a (c z) (c (s z))) (c (s z)))
3. (a (a (a (a k s) $0) s) (a s k)) { (no_c $0) } -> (a (a (c z) (c (s z))) (c (s z)))
4. (a (a s (a (a k s) $0)) (a s k)) { (no_c $0) } -> (a (a (c z) (c (s z))) (c (s z)))
5. (a (a (a k (a s s)) $0) (a s k)) { (no_c $0) } -> (a (a (c z) (c (s z))) (c (s z)))
6. (a (a (a s k) $0) (a (a s s) (a s k))) { (no_c $0) } -> (a (a (c z) (c (s z))) (c (s z)))
7. (a (a s s) (a k (a (a s k) $0))) { (no_c $0) } -> (a (a (c z) (c (s z))) (c (s z)))
8. (a (a k (a (a s s) (a k (a (a s k) $0)))) $1) { (no_c $1), (no_c $0) } -> (a (a (c z) (c (s z))) (c (s z)))
9. (a (a s (a (a k s) $0)) (a k (a (a s k) $1))) { (no_c $0), (no_c $1) } -> (a (a (c z) (c (s z))) (c (s z)))
10. (a (a (a (a k s) $0) s) (a k (a (a s k) $1))) { (no_c $0), (no_c $1) } -> (a (a (c z) (c (s z))) (c (s z)))
11. (a (a (a k (a s s)) $0) (a k (a (a s k) $1))) { (no_c $0), (no_c $1) } -> (a (a (c z) (c (s z))) (c (s z)))
```

We can also do type based program synthesis. We can implement a simple type checker for the SK combinator calculus with

```
rel infer {
    k -> (fun $a (fun $b $a))
  | s -> (fun (fun $a (fun $b $c)) (fun (fun $a $b) (fun $a $c)))
  | [[ (a $f $x) -> $x ; infer ; $a -> (fun $a $b)]
        &
        [(a $f $x) -> $f ; infer]
        ; (fun $a $b) -> $b
    ]
}
```

giving this to our identity function from earlier;

```
@(a (a s k) k) ; infer
```

we get

```
1. (a (a s k) k) -> (fun $0 $0)
```

we can use this to do type-based synthesis. The type of the duplication combinator from earlier can be specified with `(a -> a -> b) -> a -> b`, which we render as

```
infer ; @(fun (fun a (fun a b)) (fun a b))
```

getting

```
1. (a (a s s) (a s k)) -> (fun (fun a (fun a b)) (fun a b))
```

the same initial program as before. We fixed the variables to be uninterpreted constants to make sure the type was as general as possible. Asking for the `next 10`, we get

```
2. (a (a s s) (a s (a (a k k) k))) -> (fun (fun a (fun a b)) (fun a b))
3. (a (a s (a (a k s) k)) (a s k)) -> (fun (fun a (fun a b)) (fun a b))
4. (a (a s (a (a k s) k)) (a s (a (a k k) k))) -> (fun (fun a (fun a b)) (fun a b))
5. (a (a s s) (a s (a (a k k) s))) -> (fun (fun a (fun a b)) (fun a b))
6. (a (a s (a (a k s) k)) (a s (a (a k k) s))) -> (fun (fun a (fun a b)) (fun a b))
7. (a (a s (a (a k s) s)) (a s k)) -> (fun (fun a (fun a b)) (fun a b))
8. (a (a s (a (a k s) s)) (a s (a (a k k) k))) -> (fun (fun a (fun a b)) (fun a b))
9. (a (a s (a (a k s) s)) (a s (a (a k k) s))) -> (fun (fun a (fun a b)) (fun a b))
10. (a (a (a s k) k) (a (a s s) (a s k))) -> (fun (fun a (fun a b)) (fun a b))
11. (a (a (a s k) s) (a (a s s) (a s k))) -> (fun (fun a (fun a b)) (fun a b))
```

# Optimizing the Language

When I first implemented the language, most of these synthesis queries took several minutes to run, if they returned anything at all. Now, these all finish in less than a second on my laptop. This is largely a consequence of the AI-driven workflow I used to automatically identify, implement, test, and iterate optimization research.

I started by implementing a perf suite for the Rust implementation. I don't have any particular expertise in this, so I used Codex 5.2, asking it

```
Hello. I would like this system to be further optimized.
  "Measurement Infrastructure First" is a good first step. Start
  architecting a good test suite for different performance scenarios.
  A "corpus" is a good goal. Be ambitious with this; more visibility, more
  legibility, all the better
```

Yes, I do begin all my AI conversations with a greeting; it's only polite.

Codex implemented a bunch of things in a few minutes, after which I asked

```
Good. Are there any *further* additions you'd like to make that would
  meaningfully increase its utility? I guess what I'm asking is, what *all*
  would you want to see added to declare this corpus feature complete?
```

And I went a few rounds asking it variants of this same question after each revision. Eventually, it said it couldn't think of anything else. I asked it to use the system to give an initial baseline, and correct any problems it saw in doing so. It didn't change anything, but it did give an initial flame graph in addition to other baseline statistics.

After this, I asked it to come up with a list of speculative ideas for optimizing the program.

```
I'd like you to come up with a large list of areas of investigation for
  potential speedups. I believe several orders of magnitude improvements
  are possible, but they will require deeper thinking about architecture
  and organization than what simple micro optimizations can squeeze out.
  Such things are, by their nature, speculative and in need of testing.
  These can be fully fleshed out ideas, or simply observations that may
  lead to improvements. Document these in a .md file.
```

After this, I switched to using Opus 4.6. I basically just told it variants of

```
Hello. I would like to do some performance/optimization investigations. Please look at PERFORMANCE_INVESTIGATIONS.md. Pick something on the list; this session will be dedicated to investigating it. Make sure it's something not already fully investigated (see the perf investigation reports in docs for reference on past investigations). Use a perf measurement (such as a flamegraph) to judge what direction has the most potential.
```

and, after it was done, interrogated the result a bit. I eventually fell into orchestrating the following workflow;

1. Read the ideas file along with recent perf investigations (stored in a sibling folder)
2. Do initial flame graph on suite and, based on that, pick the optimization it thinks has the highest potential and implement it
3. Compare perfs across ten runs on a suite of examples
4. If the runtime improvements seemed unambiguous, keep it, otherwise trash the changes
5. Write a new perf investigation file and update the living file of potential improvements
6. Clean up the code (refactor any introduced redundancy then re-verify improvement + clippy and fmt)

I repeated this in a loop for a few hours allowing Claude to nearly 1/10th the runtime of a program synthesis task. I did notice it got in a microoptimization rut, intentionally avoiding ideas it deemed "risky". It leaves most ambitious "big picture" optimizations on the table.

Over time, it built up a history of perf report attempts. Eventually, I decided to make this fully autonomous. This culminated in the skill you can find [here](https://github.com/AHartNtkn/claude-optimize-skill).

I used the recently released [agent-teams](https://code.claude.com/docs/en/agent-teams) feature of Claude Code (which needs to be activated as it's a beta feature). The skill is called with an orchestrator agent that spawns 2-3 subagents with their own worktrees to implement and perf features. This isolates context, allowing each idea to be implemented in isolation, without biases from previous work and with fresh context each time. The orchestrator also only needs to review the end result of the agent's work; it doesn't need to see every step along the way. The worktrees are discarded if their implementation isn't good. I also got it to be more ambitious, investigating algorithmic changes, and it was able to squeeze another 4x on my synth task.

I did run into some issues. When running like this, compactions happen often, and the command gets compacted away. To get it to work long autonomously, I set up a hook that activates whenever compaction happens that looks up whatever skills were being used and copies them to the context after compaction. Without that, the agent relied on compact summaries which decayed in quality over time. This usually just biased the orchestrator to stop and ask if the user wants to continue, so it wasn't a catastrophic failure. Still, this hook in combination with concrete stopping criteria (which is three failed investigations in a row) has allowed it to work on its own with no input from me at all for over a dozen hours.

I also added a few programs I care about to the regression suite, ran after each addition. These were specific programs I cared about performing well, in particular. If any of them broke, the regression suite would stop otherwise attractive looking optimizations in their tracks.

Since I don't want to interact with it at all, it's set up to use `--dangerously-skip-permissions`. This means Claude has the power to brick my system. I did isolate it to a computer I'm not actively using, and I also [added this](https://gist.github.com/sgasser/efeb186bad7e68c146d6692ec05c1a57), which mitigates the worst failure cases, but that's needed for full autonomy.

This also assumes you already have a very good test and perf suite set up. Without that, I wouldn't expect this to be that helpful for optimization, and it could approve changes that break essential functionality if a test wouldn't catch it. It can add and improve perf tests every loop to maximize legibility for the specific optimization, but it doesn't set up initial infrastructure.

After I saw it working, I let it run overnight. 42 loops with no interference. Synth task was at another 2x, but the more impressive thing is that long searches use 100x less memory, and many searches were simply more efficient; actually possible where they weren't before. And everything else seems to work the same, just faster and cheaper.

# Conclusion

This is certainly useful, and it will open up more optimization-dependent projects in the future. There have been other ideas I've had, solvers and physics engines, which were resource constrained, and optimizing them was long and tedious and never ending. AI was always moderately helpful at making suggestions, but this loop allowed me to start it in the evening, and when I woke up I had a significantly more performant program. I can't guarantee it's as good as possible; I certainly don't believe that. But I do think it's likely better than it would be if I spent a year manually optimizing it. Probably worse than if I hired an expert.

As it stands, the basic paradigm of this is effective for automatic research. There's nothing special about optimization. Software is special in that it exists on the same substrate as the AI, making iteration easier, but this is me discovering how to do automated research more broadly. Eventually, something like this will be table stakes and a default mode for some AIs when given difficult tasks of any kind. And, as with most things, smarter AIs will have better ideas, will be able to execute those ideas more effectively, and be better at assessing the potential of those ideas while choosing what to prioritize. What a world we will soon find ourselves in...
