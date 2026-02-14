---
layout: page
title: About me
subtitle:
---

My name is Anthony Hart. I'm a functional programmer and formal verification engineer at Heliax, with a long-standing interest in the theory of programming and the foundations of mathematics.

## History

I started programming in C++ when I was 12; since then I've gained experience in a multitude of languages from Python, to Haskell, to Coq, to Lean, and more. These days I tend to prototype in Haskell and implement performance-critical applications in Rust. I have an interest in emulation (a while back I implemented a [simple emulator](https://community.wolfram.com/groups/-/m/t/138003?p_p_auth=UdH7DgrD) in Mathematica) and implementing mathematics on computers. For a while, I had an obsession with computer algebra systems, and I implemented a basic one as one of my first programs. It was awful and the code is lost to time, now, but I still maintain an interest in the topic.

I've also had an interest in the foundations of mathematics for over a decade. My interest began when I was 14. Through my self-study, I eventually found and made small contributions to the [Metamath project](http://us.metamath.org/). After that, I got experience with a wider variety of perspectives on the foundations of mathematics, about HoL from [Isabelle](https://isabelle.in.tum.de/), and about dependent type theory from [Agda](https://wiki.portal.chalmers.se/agda/pmwiki.php). Some of my repositories relate to formally verified mathematics through dependent type theory. More recently, I've been working extensively with [Lean 4](https://lean-lang.org/) for mechanized proofs, such as my [formalization of Heterogeneous Broadcast](https://zenodo.org/records/17611735).

From that experience, I make a hobby out of creating programming languages with sophisticated type-systems, some of which are capable of acting as foundations for mathematics. The handful that I've finished are posted to GitHub, but I'm constantly working on some new idea. The green icon on top of my blog comes from a graphical programming language that unfortunately never came to fruition. It denotes the type

    Π x : nat . id(nat, plus(x, zero) x)

## Current Interests

My work and research sit at the intersection of formal verification, programming language design, and cryptographic proof systems. I've published papers on topics including arithmetization of functional programs via interaction nets, intent machines, and constraint satisfaction.

I'm particularly drawn to relational programming. I've designed languages where programs are denoted by pure mathematical relations, enabling bidirectional computation and program synthesis. My most recent project is a relational programming language implemented in Rust that can, among other things, synthesize SK combinator programs from behavioral specifications or type signatures.

I also have a growing interest in using AI as an accelerator for the research and implementation of novel software. These days, I spend a lot of my time creating skills and workflows for various projects, such as automated optimization loops, educational content generation, and self-verified writing.
