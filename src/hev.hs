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


-----------------------------------------------------------------------
-- ========================== Data types =========================== --
-----------------------------------------------------------------------

import Char

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

data Num a => PossiblyInfinite a = Finite a
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
-- ============================= Tests ============================= --
-----------------------------------------------------------------------

--
-- Parsing Hev programs into trees
--

testParse1 = parse "1+2*3"
testParse2 = parse "3+2*1"
testParse3 = parse "1,3/2"
testParse4 = parse "1,6*2,9*3,5*4"
testParse5 = parse "1,9,2,13,3,10,4,15,5,11,6,14,7,12,8"
testParse6 = parse "120,160,180,271,272,257,15,4,7,121"
testParse7 = parse "120,160**180,271+-272,257+*15,4//7//121"

--
-- Parsing and statically checking Hev programs
--

testCompile1 = compile "43,13,23,53,33"  -- ok
testCompile2 = compile "1,3,2"           -- bad (incomplete patterns)
testCompile3 = compile "43,13,23,53+33"  -- bad (variables in state)
testCompile4 = compile "43,13,23+53,33"  -- bad (variables in body but not head)

--
-- Bindings used in unifiers
--

testBindings = (UnifierBinding "-" TreeLeaf (UnifierBinding "+" (TreeBranch TreeLeaf TreeLeaf) UnifierNil))

testGetBinding1 = getBinding "+" testBindings  -- Just (TreeBranch TreeLeaf TreeLeaf)
testGetBinding2 = getBinding "-" testBindings  -- Just TreeLeaf
testGetBinding3 = getBinding "*" testBindings  -- Nothing

--
-- Pattern matching
--

testMatch1 = match (TreeLeaf) (TreeLeaf) UnifierNil
                   -- UnifierNil
testMatch2 = match (TreeLeaf) (TreeBranch TreeLeaf TreeLeaf) UnifierNil
                   -- UnificationFailure
testMatch3 = match (TreeVar "+") (TreeBranch TreeLeaf TreeLeaf) UnifierNil
                   -- "+" => (TreeBranch TreeLeaf TreeLeaf)
testMatch4 = match (TreeBranch (TreeVar "+") TreeLeaf)
                   (TreeBranch (TreeBranch TreeLeaf TreeLeaf) TreeLeaf)
                   UnifierNil 
                   -- "+" => (TreeBranch TreeLeaf TreeLeaf)
testMatch5 = match (TreeBranch (TreeVar "+") TreeLeaf)
                   (TreeBranch TreeLeaf TreeLeaf)
                   UnifierNil 
                   -- "+" => TreeLeaf
testMatch6 = match (TreeBranch (TreeVar "+") TreeLeaf)
                   (TreeBranch TreeLeaf (TreeBranch TreeLeaf TreeLeaf))
                   UnifierNil
                   -- UnificationFailure

--
-- Tree rewriting
--

testRewrite1 = rewrite (TreeBranch TreeLeaf TreeLeaf)
                       (TreeBranch TreeLeaf TreeLeaf)
                       TreeLeaf

testRewrite2 = rewrite (TreeBranch TreeLeaf TreeLeaf)
                       (TreeBranch (TreeVar "+") TreeLeaf)
                       TreeLeaf

testRewrite3 = rewrite (TreeBranch (TreeBranch TreeLeaf TreeLeaf) TreeLeaf)
                       (TreeBranch (TreeVar "+") TreeLeaf)
                       (TreeVar "+")

--
-- Hev execution
--

--
-- Test a very simple (and completely ground) program.
--

--
--           __(root)_X_53______
-- (rule    /                   \
--  list)  X 43               33 X   (state)
--        / \                   / \
-- (end) ,   \                 ,   ,
--            \
--             \
--              \
--            23 X  (rule)
--              / \
--             /   \
--            /     \
--           /       \
--          /         \
-- (head)  X 13        ,   (body)
--        / \
--       ,   ,

testRun1 = run (compile "43,13,23,53,33")  -- TreeLeaf


--
-- Test pattern-matching with a variable, causing a single rewrite.
--

--
--           __(root)_X_81______
-- (rule    /                   \
--  list)  X 49               73 X   (state)
--        / \                   / \
-- (end) ,   \              64 X   \
--            \               / \   \
--             \             ,   ,   X 17
--              \                   / \
--            36 X  (rule)         ,   X 4
--              / \                   / \
--             /   \                 ,   ,
--            /     \
--           /       \
--          /         \
-- (head)  X 25     16 X   (body)
--        / \         / \
--     9 X   +       +   ,
--      / \
--     ,   ,

--
-- One rewrite of state: + matches branch at 17, new state becomes:
--
--        X
--       / \
--   17 X   ,
--     / \
--    ,   X 4
--       / \
--      ,   ,
-- 

testRun2 = run (compile "49,9,25+36+16,81,64,73,17,4")
        -- TreeBranch (TreeBranch TreeLeaf (TreeBranch TreeLeaf TreeLeaf)) TreeLeaf


--
-- Test several rewrites of state.  The state is reduced to a single leaf.
--

--
--           __(root)_X_4_______
-- (rule    /                   \
--  list)  X 3                 2 X   (state)
--        / \                   / \
-- (end) ,   \                 /   \
--            \               /     \
--             \             X 1   1 X
--              \           / \     / \
--             2 X  (rule) ,   ,   ,   ,
--              / \
--             /   \
--            /     \
--           /       \
--          /         \
-- (head)  X 1         *   (body)
--        / \
--       *   *

testRun3 = run (compile "3*1*2*4,1,2,1")    -- TreeLeaf


--
-- Test that rewriting is indeed top-down.
-- For this program, a bottom-up strategy produces a double branch,
-- while a top-down strategy produces a single branch.
-- (Work it out by hand if you doubt me!)
--

--
--           __(root)_X_99______
-- (rule    /                   \
--  list)  X 71                  \
--        / \                     \
-- (end) ,   \                     \
--            \                     \
--             \                     X 61 (state)
--              \                   / \
--            29 X  (rule)         /   \
--              / \               /     \
--             /   \          37 X       X 47
--            /     \           / \     / \
--           /       \         /   \   ,   ,
--          /         \     6 X   7 X
-- (head)  X 27 (body) *     / \   / \
--        / \               ,   , ,   ,
--       /   \
--    8 X     X 19
--     / \   / \
--    +   * ,   ,


testRun4 = run (compile "71+8*27,19,29*99,6,37,7,61,47")
        -- (TreeBranch TreeLeaf TreeLeaf)

--
-- Unterminated rewriting.
--

--
--           __(root)_X_26______
-- (rule    /                   \
--  list)  X 25                  X (state)
--        / \                   / \
-- (end) ,   \                 ,   ,
--            \
--             \
--              \
--            24 X  (rule)
--              / \
--             /   \
--            /     \
--           /       \
--          /         \
-- (head)  X   (body)  X 20
--        / \         / \
--       ,   *       ,   X 1
--                      / \
--                     ,   *

testRun5 = run (compile "25,20*24,20,1*26,25")

--
-- This is a helper function intended for testing testRun5,
-- analogous to Haskell's standard "take" function for lists.
-- It works when run on infiniteTree, but doesn't work when
-- run on testRun5, and I'm not certain why.
--

takeTree 0 tree = TreeLeaf
takeTree n TreeLeaf = TreeLeaf
takeTree n (TreeBranch left right) =
    TreeBranch (takeTree (n-1) left) (takeTree (n-1) right)

infiniteTree = TreeBranch infiniteTree infiniteTree

--
-- Test multiple rules.
--

--
--                            _________(root)__X__188___________
--                           /                                  \
--             (rule list)  X 154                            111 X   (state)
--                         / \                                  / \
--                        /   \                                ,   X 91
--                       /     \                                  / \
--                      /       \                                ,   X 77
--                     /         \                                  / \
--                    /       137 X  (rule)                        ,   ,
--                   /           / \
--                  /           /   \
--                 /    (head) X 6   , (body)
--                /           / \
--               /         2 X   ,
--              /           / \
-- (rule node) X 103       ,   ,
--            / \
--     (end) ,   X (rule) 86
--              / \
--             /   \
--            /     \
--    (head) X 5   9 X (body)
--          / \     / \
--         ,   X 4 -   ,
--            / \
--           ,   -

testRun6 = run (compile "103,5,4-86-9,154,2,6,137,188,111,91,77")  -- TreeLeaf
