--
-- hev.hs
-- Reference Interpreter for the Hev Programming Language
-- Begun November 2005, fleshed out October 2006, polished off June 2007
-- Chris Pressey, Cat's Eye Technologies
--

--
-- Copyright (c)2005-2012 Chris Pressey, Cat's Eye Technologies.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--  1. Redistributions of source code must retain the above copyright
--     notices, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above copyright
--     notices, this list of conditions, and the following disclaimer in
--     the documentation and/or other materials provided with the
--     distribution.
--  3. Neither the names of the copyright holders nor the names of their
--     contributors may be used to endorse or promote products derived
--     from this software without specific prior written permission. 
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
-- COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module Hev where

-----------------------------------------------------------------------
-- ========================== Data types =========================== --
-----------------------------------------------------------------------

import Data.Char

--
-- A data type giving the structure of the trees that Hev programs
-- describe.  They contain no values, but the (sub)trees that will
-- be used as pattern-matching rules can contain variables.
--

data Tree = TreeBranch Tree Tree
          | TreeLeaf
          | TreeVar String
     deriving (Show, Read, Eq)

--
-- A data type describing unifiers.  Really, a unifier is just a
-- list of name-value associations, like an environment.
--

data Unifier = UnifierBinding String Tree Unifier
             | UnifierNil
             | UnificationFailure
     deriving (Show, Read, Eq)

--
-- A data type for possibly infinite numbers, so that various values
-- in this evaluator (specifically, the "roof" parameter to buildTree)
-- aren't artifically bounded.
--
-- I could probably do something fancy by overriding '>' in the
-- Ord type class here, but for now at least, I won't.
--

data PossiblyInfinite a = Finite a
                        | Infinity
     deriving (Show, Read, Eq)

isGreater i (Finite j) = i > j
isGreater i Infinity = False


-----------------------------------------------------------------------
-- ============================= Parser ============================ --
-----------------------------------------------------------------------

--
-- Determine the integer value of a decimal digit character.
--

digitVal '0' = 0
digitVal '1' = 1
digitVal '2' = 2
digitVal '3' = 3
digitVal '4' = 4
digitVal '5' = 5
digitVal '6' = 6
digitVal '7' = 7
digitVal '8' = 8
digitVal '9' = 9

--
-- Accumulate the value of a digit onto the end of a integer
-- and return the result.
--

accumulate char num =
    (num * 10) + digitVal char

--
-- Scan an integer in decimal notation at the start of
-- a string; return a pair consisting of the integer
-- and the rest of the string.
--

consumeOperator [] num = (num, [])
consumeOperator string@(char:chars) num
    | isSpace char =
        consumeOperator chars num
    | isDigit char =
        consumeOperator chars (accumulate char num)
    | otherwise =
        (num, string)

--
-- Determine if a given character is suitable for use in an atom
--

isAtomSymbol ',' = True
isAtomSymbol '+' = True
isAtomSymbol '-' = True
isAtomSymbol '*' = True
isAtomSymbol '/' = True
isAtomSymbol _   = False

--
-- Convert the textual representation of an atom to
-- its internal representation.
--

stringToTree []     = TreeLeaf
stringToTree ","    = TreeLeaf
stringToTree string = TreeVar string

--
-- Scan a symbol from the start of a string; return a pair
-- consisting of the corresponding tree representation of
-- the atom, and the rest of the string.
--

consumeAtom [] acc =
    (stringToTree acc, [])
consumeAtom string@(char:chars) acc
    | isSpace char =
        consumeAtom chars acc
    | isAtomSymbol char =
        consumeAtom chars (acc ++ [char])
    | otherwise =
        (stringToTree acc, string)

--
-- Convert the textual representation of a Hev program to
-- an internal representation (a list of operator-atom pairs.)
--

stringToPairs "" = []
stringToPairs string =
    let
        (op, string2) = consumeOperator string 0
        (atom, string3) = consumeAtom string2 []
    in
        ((op, atom) : (stringToPairs string3))

--
-- Be not deceived by the apparent simplicity of the following
-- function!  It took me the better part of a day to get it right.
--
-- This function builds a tree corresponding to each of the
-- operators in the list of (operator, atom) pairs, up until
-- (and not including) the first operator in the list which
-- exceeds a given maximum value (which we call the "roof".)
-- Once this roof-exceeding value is found (or there are no
-- more elements in the list,) this tree is returned (along
-- with the unused portion of the list.)
--
-- The root of the tree so built corresponds to the largest
-- operator found in the list.  The list is thus conceptually
-- divided into a left sublist and a right sublist.
-- Recursively, the left subtree of the root is associated
-- with the largest operator in the left sublist, and the
-- right subtree with the largest operator in the right sublist.
--
-- And in fact, the straightforward way to implement this
-- function would be to do just that: search for the largest
-- element of the list, split the list into two sublists, and
-- process each of those sublists recursively.  However, there
-- is a certain elegance (and presumably efficiency, although
-- that's not the motivation here) that is derived from doing
-- only one pass, left to right, through the list, and that's
-- the approach I've chosen to take.  Thus we have the
-- following implementation.
--
-- The function basically loops around, consuming the list from
-- left to right while tracking some state:
--
--   roof     - as noted, the upper limit to the value of operator
--              that we accept.  When we see it, we return the
--              built tree and the rest of the list to our caller.
--   bigOp    - the biggest operator we have seen in the list so
--              far, locally speaking.  That is, it will always be
--              smaller than the roof value.  It is used to decide
--              when to start building a subtree.
--   bigTree  - the tree value associated with bigOp; acts more or
--              less like an accumulator.
--   prevAtom - needed to get the variables into the leaves of the
--              tree where they logically belong.  (The left
--              subtree of the bottom branch actually needs the
--              variable that is paired with the previous operator
--              in the list.)
--
-- During the loop, behaviour is split into three cases:
--
-- Case 1: the operator exceeds the roof; return.
-- Case 2: the operator is bigger than the biggest operator
--         seen so far.  Use it as the biggest operator, construct
--         a tree node for it for use as the biggest tree, and
--         loop around to tackle the next operator in the list.
-- Case 3: the operator is smaller than the biggest operator
--         seen so far.  Create a subtree by recursively calling
--         buildTreeLoop.  For this call, the roof value is given
--         by the biggest operator, and the biggest operator is
--         initially set back to zero.  The returned subtree is
--         spliced into the biggest tree, as the right child.
--         The loop then continues.  The altered biggest tree,
--         and the amount of the list consumed by the creation of
--         the subtree are taken into account for the next loop
--         iteration, but the roof and biggest operator do not
--         change.
--

buildTree [] roof bigOp bigTree prevAtom =
    (bigTree, [])
buildTree pairs@((op, atom):restOfPairs) roof bigOp bigTree prevAtom
    | isGreater op roof =
        (bigTree, pairs)
    | op > bigOp =
        let
            newBigTree = TreeBranch bigTree atom
        in
            buildTree restOfPairs roof op newBigTree atom
    | op < bigOp =
        let
            (subTree, newPairs) = buildTree pairs (Finite bigOp) 0 prevAtom atom
            (TreeBranch bigTreeLeft bigTreeRight) = bigTree
            newBigTree = (TreeBranch bigTreeLeft subTree)
        in
            buildTree newPairs roof bigOp newBigTree atom

--
-- Parse a Hev program into a valueless tree.
--

parse string =
    fst (buildTree (stringToPairs string) Infinity 0 TreeLeaf TreeLeaf)


-----------------------------------------------------------------------
-- ======================= Static Checker ========================== --
-----------------------------------------------------------------------

--
-- Return a list of all variables that occur in a given tree.
--

getVariables TreeLeaf = []
getVariables (TreeVar var) = [var]
getVariables (TreeBranch left right) =
    (getVariables left) ++ (getVariables right)

--
-- Determine whether every element of the first list is also an element
-- of the second list.
--

isSubset [] _ = True
isSubset (first:rest) list =
   (elem first list) && (isSubset rest list)
   where
      elem x [] = False
      elem x (first:rest)
          | x == first = True
          | otherwise = elem x rest

--
-- Determine whether a tree is "ground", i.e. contains no variables.
--

isGround tree = getVariables tree == []

--
-- Determine whether a set of rules is complete (each rule is complete,
-- and the tree by which the ruleset itself is represented doesn't have
-- any variables.)
--

rulesComplete TreeLeaf = True
rulesComplete (TreeVar _) = False
rulesComplete (TreeBranch left right) =
    ruleComplete right && rulesComplete left

--
-- Determine whether a rule is complete (it has has both a head and a
-- body, and there are no variables in the body that aren't in the head.)
--

ruleComplete TreeLeaf = False
ruleComplete (TreeVar _) = False
ruleComplete (TreeBranch head body) =
    isSubset (getVariables body) (getVariables head)

--
-- Parse and check a Hev program.  Returns an illegal tree (which will
-- cause a Haskell runtime pattern-match error later on) if there are
-- static errors detected in the Hev program.
--

compile string
    | not (isGround stateTree) =
        TreeLeaf
    | not (rulesComplete ruleTree) =
        TreeLeaf
    | otherwise =
        tree
    where
        tree@(TreeBranch ruleTree stateTree) = parse string


-----------------------------------------------------------------------
-- ======================= Tree rewriting ========================== --
-----------------------------------------------------------------------

--
-- Given a variable and a unifier, get the value given in
-- the unifier for than variable, or Nothing if it is not found.
--

getBinding _ UnificationFailure =
    Nothing
getBinding _ UnifierNil =
    Nothing
getBinding targetVar (UnifierBinding sourceVar tree unifier)
    | targetVar == sourceVar =
        Just tree
    | otherwise =
        getBinding targetVar unifier

--
-- Match a "pattern" tree (the first argument) to a "state" tree and
-- return the most general unifier, or nothing.
--

match _ _ UnificationFailure = UnificationFailure
match TreeLeaf TreeLeaf unifier = unifier
match (TreeBranch left1 right1) (TreeBranch left2 right2) unifier =
    let
        unifier2 = match left1 left2 unifier
        unifier3 = match right1 right2 unifier2
    in
        unifier3
match (TreeVar var) subTree unifier
    | binding == Nothing =
        UnifierBinding var subTree unifier
    | binding /= Just subTree =
        UnificationFailure
    | otherwise =
        unifier
    where
        binding = getBinding var unifier
match _ _ _ = UnificationFailure

--
-- Given a tree containing variables and a unifier, construct
-- a "ground" tree (one with no variables) by replacing each
-- variable with the value associated with it in the unifier.
--

expand TreeLeaf unifier = TreeLeaf
expand (TreeBranch left right) unifier =
    TreeBranch (expand left unifier) (expand right unifier)
expand (TreeVar var) unifier =
    let
        (Just subTree) = getBinding var unifier
    in
        subTree

--
-- Try to match the given pattern (the head of a rule) in
-- the given tree.  If there are multiple places where
-- the pattern might match the tree, only the topmost leftmost
-- one is chosen.  Then return a new tree with the matched
-- portion replaced by the body (appropriately expanded
-- with any matched variables) if a match succeeded, or
-- the original tree if no match succeeded.  In either case,
-- a boolean indicating whether the match succeeded is
-- returned as well.
--

rewrite tree@(TreeBranch left right) head body
    | unifier /= UnificationFailure =
        (True, expand body unifier)
    | successLeft =
        (True, TreeBranch newSubTreeLeft right)
    | successRight =
        (True, TreeBranch left newSubTreeRight)
    | otherwise =
        (False, tree)
    where
        unifier = match head tree UnifierNil
        (successLeft, newSubTreeLeft) = (rewrite left head body)
        (successRight, newSubTreeRight) = (rewrite right head body)

rewrite tree head body
    | unifier /= UnificationFailure =
        (True, expand body unifier)
    | otherwise =
        (False, tree)
    where
        unifier = match head tree UnifierNil


-----------------------------------------------------------------------
-- =========================== Execution =========================== --
-----------------------------------------------------------------------

--
-- A program is represented by
--
--          _______root_______
--         /                  \
--        X               ...state...
--       / \
--      X   rule
--     / \
--  ...   rule
--

--
-- Each rule is represented by
--
--           __rule__
--          /        \
--        head      body
--
-- where the head is the pattern which will be matched against the
-- state, and the body is the replacement which will be substituted.
--

--
-- Here's how the interpreter works:
--
-- Assemble a working list of rules (initially all rules.)
--
-- Pick the first available rule from the working list of rules.
--
-- Match the head of the rule against the state tree.
--
-- If there was a match, replace the matched portion with an
-- appropriate instantiation of the body of the rule, and repeat
-- from the very beginning.
--
-- If there was no match, remove this pattern from the working list of
-- patterns and try again with the shorter working list.
--
-- If this was the last working pattern, end.
--

run (TreeBranch patternTree stateTree) =
    loop patternTree patternTree stateTree
    where
        loop TreeLeaf all state = state
        loop (TreeBranch rest pat@(TreeBranch head body)) all state
            | matched =
                loop all all state'
            | otherwise =
                loop rest all state
            where
                (matched, state') = rewrite state head body


-----------------------------------------------------------------------
-- ======================== Internal Tests ========================= --
-----------------------------------------------------------------------


matchTests =
    [
        (match
          (TreeLeaf) (TreeLeaf) UnifierNil,
          UnifierNil),
        (match
          (TreeLeaf) (TreeBranch TreeLeaf TreeLeaf) UnifierNil,
          UnificationFailure),
        (match
          (TreeVar "+") (TreeBranch TreeLeaf TreeLeaf) UnifierNil,
          UnifierBinding "+" (TreeBranch TreeLeaf TreeLeaf) UnifierNil),
        (match
          (TreeBranch (TreeVar "+") TreeLeaf)
          (TreeBranch (TreeBranch TreeLeaf TreeLeaf) TreeLeaf)
          UnifierNil, 
          UnifierBinding "+" (TreeBranch TreeLeaf TreeLeaf) UnifierNil),
        (match
          (TreeBranch (TreeVar "+") TreeLeaf)
          (TreeBranch TreeLeaf TreeLeaf)
          UnifierNil,
          UnifierBinding "+" TreeLeaf UnifierNil),
        (match
          (TreeBranch (TreeVar "+") TreeLeaf)
          (TreeBranch TreeLeaf (TreeBranch TreeLeaf TreeLeaf))
          UnifierNil,
          UnificationFailure)
    ]

rewriteTests =
    [
        (rewrite
          (TreeBranch TreeLeaf TreeLeaf)
          (TreeBranch TreeLeaf TreeLeaf)
          TreeLeaf,
          (True, TreeLeaf)),
        (rewrite
          (TreeBranch TreeLeaf TreeLeaf)
          (TreeBranch (TreeVar "+") TreeLeaf)
          TreeLeaf,
          (True, TreeLeaf)),
        (rewrite
          (TreeBranch (TreeBranch TreeLeaf TreeLeaf) TreeLeaf)
          (TreeBranch (TreeVar "+") TreeLeaf)
          (TreeVar "+"),
          (True, TreeBranch TreeLeaf TreeLeaf))
    ]

test_r n [] = Nothing
test_r n ((val, expected):ts) =
    case test_r (n+1) ts of
        Just error -> Just error
        Nothing -> case val == expected of
            True -> Nothing
            False -> Just (n, val, expected)

test =
    case test_r 1 matchTests of
        Nothing ->
            case test_r 1 rewriteTests of
                Nothing -> Nothing
                Just (n, val, expected) -> Just $ show ("rewrite", n, val, expected)
        Just (n, val, expected) -> Just $ show ("match", n, val, expected)
