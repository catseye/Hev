The Hev Programming Language
============================

Introduction
------------

Hey, does this thing look at all familiar?

    ()  []  ->  .  ++  --                        Left to right
    ++  --  +  -  !  ~  *  &  (type)  sizeof     Right to left
    *  /  %                                      Left to right
    +  -                                         Left to right
    <<  >>                                       Left to right
    <  <=  >  >=                                 Left to right
    ==  !=                                       Left to right
    &                                            Left to right
    ^                                            Left to right
    |                                            Left to right
    &&                                           Left to right
    ||                                           Left to right
    ?:                                           Left to right
    =  +=  -=  *=  /=  %=  &=  ^=  |=  <<=  >>=  Right to left
    ,                                            Left to right

That's right: it's the precedence table for operators in the C language.
OK, some of them (like `()`) are pretty easy to remember, I guess, but
the logic behind most of these choices escapes me. Really, can you give
me a *good* reason for why `&` should have a higher precedence than `|`?
And why is `^` in-between? And why in the world are `->` and `++` on the
same level? What I'm getting at is, how many times did you have to go
and reference this chart before these arbitrary rules got burned into
your nervous system somewhere between your brain and your fingers?

And hey, you think that's bad? Perl 5 has like 129 operators at like 24
levels of precedence.

You may well ask: is there something that can save us from this
insanity?

Yes, there is. In this document I will describe Hev, a novel, nay,
innovative, nay, radical, nay, revolutionary, nay, *totally gnarly* new
programming language which provides **the infix you love** with
**an unlimited number of operator precedence levels** and **absolutely
no need for parentheses or memorization!**

Sound too good to be true...? Read on!

Syntax
------

Hev's breathtaking syntactic slight-of-hand is accomplished by a
synergistic combination of two features:

-   Have an unbounded number of infix binary operators.
-   Make precedence explicit.

To fit this bill, all we need is a single syntactic construct that can
explicitly express an unbounded number of discrete operators, and at the
same time, their precedence.

Well, I chose integers.

Positive integers, to be precise. So `3` is an infix operator. So is
`15`, and it has a higher precedence than `3`. So is `514229`, and it
has an even higher precedence than `15`, but lower than
`25852016738884976640000`. *See* how easy it is? I can just name two
operators at random, and you can tell me which one has the higher
precedence without a second thought!

Oh, but what good are operators if they don't have anything to operate
on? We need values, too. And since we have an unbounded number of
operators, there's a certain sense to having only a bounded number of
values.

Well, why not the logical extreme: *no values at all*? Well, OK, for the
sake of syntax we need to have one value, but since there's nothing it
can be differentiated against, it's effectively no values.
Syntactically, this value, or lack thereof, is denoted `,`. (Yeah,
that's a comma.)

And, we'll probably need variables at some point, too, I'm guessing. We
should probably have a nice big supply of those, just so we don't run
into some artifical bound at some point that arbitrarily prevents Hev
from being Turing-complete. So, let's say that any string of consecutive
symbols drawn from `+`, `-`, `*` and `/` is an identifier for a
variable. That should do nicely.

There's still a bit of a problem, though -- those pesky parentheses. You
might need to nest a `5`-expression into the LHS or the RHS of a
`3`-expression, and that would seemingly require parentheses. How do we
avoid this? Well -- if we're flexible on what `3` and `5` actually
*mean*, maybe we can just avoid this dilemma entirely! This brings us
to...

Semantics
---------

So we have all these infix binary operators, and this one value which I
insist is essentially a non-value, and we need to be able to make
something sensible out of this mess -- *without* using parentheses to do
nesting.

Well, what can we build?

Trees.

Yep, binary trees. They're a bit unlike the "normal" trees of Computer
Science, which almost universally have some sort of values stored at
their leaves. These ones don't. They're just... you know, trees. But we
can definately build them. And we don't need any parentheses. If you
want to nest some expression inside another, you just pick operators
with higher precedence levels for that expression.

So `,5,10,5,` is a tree - a complete binary tree with 3 levels - a root
node (`10`), two intermediate nodes (both `5`) and four leaves (`,,,,`)
with no values in them (or a single, meaningless value, repeated four
times, if you like.) And please realize that this is the *same* tree as
`,1,3,2,` -- it's just that different operators were used to construct
it. Those operators aren't "in" the tree in any sense, and their
magnitude is used only to determine their precedence.

But now for the splendid part. We can put *variables* in these trees!
Which means, we can think of them as *patterns* that can match other
trees. Which means, we can specify *rules* as pairs of patterns and
substitutions, to be substituted in when the pattern matches. Which
means, we can construct a rule-based language! A rewriting language, in
fact. I think I'll call this approach valueless tree rewriting.

So, for example, the tree `+10*` matches that tree `,5,10,5,` given
above. The variables `+` and `*` both unify with `,5,`. But note that
this pattern matches `,41,76,` too, where `+` unifies with `,41,` and
`*` unifies with `,`. And in fact it matches countless other possible
valueless trees.

Execution Model
---------------

A Hev program consists of a valueless binary tree. The left branch of
the root leads to a ruleset; the right branch leads to a valueless
binary tree which represents the data of the program: it is the state of
the program, the thing that is being rewritten. This data tree may not
contain any variables: the leaves must be entirely `,`'s.

A ruleset consists of a node where the left branch leads to either a
ruleset or to a `,` and the right branch leads to a rule. A rule is a
node where the left branch is a pattern and the right branch is a
substitution. The pattern is a valueless binary tree which may contain
not only `,`'s but also any variables at its leaves. The substitution
may contain both `,`'s and variables, but it may not contain any
variables which do not appear in the corresponding pattern of the rule.

Each rule in the ruleset is considered in turn, starting with the rule
nearest the root. The pattern of the rule is matched against the data
tree. The structure of the tree must match some subtree of the data
tree; a variable can match any structure of the data tree, but no
variable can match two different structures. (The same variable
identifier may appear multiple times in a pattern; all instances of that
variable must match the same structure.) If there are multiple subtrees
of the data tree that match, only the **topmost** one is considered.
This is usually called "top-down rewriting".

When a match occurs, the substitution of the rule is instantiated. Any
variables occuring in the substitution are replaced with the structures
that those variables matched in the pattern. (This is why all the
variables appearing in the substitution must also appear in the
pattern.) The data tree is then modified: the subtree that was matched
is removed and in its place the instantiated substitution is grafted.
The process then repeats (starting over with the topmost rule.)

When a rule fails to match, the data tree is left alone and the next
rule (one node lower down in the ruleset) is tried. When there are no
more rules to try in the ruleset, the program ends.

Miscellaneous Notes
-------------------

You can leave out the `,` at the very beginning and very end of a Hev
program. It's implied. Also, whitespace is allowed, even between the
digits of an operator or the symbols of a variable... for whatever good
it'll do you.

Implementation
--------------

`hev.hs` is a reference implementation of Hev in Haskell. It can be used
as something to check this language description against - any
discrepancy is either a bug in the implementation, or an error in this
document. `hev.hs` shouldn't be used as an official reference for Hev
behaviour that's not described in this document, but heck, it's better
than nothing, right?

History
-------

It was sometime in November of 2005 when I came up with the idea to try
to "break the precedence barrier" and started writing Hev. I continued
to refine the idea and worked on it, on and off, after that. In October
of 2006 I got a stubborn notion in my head that the parser should only
make one pass over the program text, so I wasted a day trying to figure
out how to code that in Haskell. In June of 2007 I finally got down to
writing test cases and debugging it.

Happy `,`!

-Chris Pressey  
Cat's Eye Technologies  
June 17, 2007  
Vancouver, BC
