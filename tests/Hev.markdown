Test Suite for Hev
==================

This test suite is written in the format of Falderal.  It is far from
exhaustive, but provides a basic sanity check that the language we've
implemented comes close to Hev.

Hev Parsing
-----------

    -> Tests for functionality "Parse Hev Program"

    -> Functionality "Parse Hev Program" is implemented by
    -> shell command
    -> "ghc src/hev.hs -e "parse \"%(test-text)\"""

    | 1+2*3
    = TreeBranch (TreeBranch (TreeBranch TreeLeaf (TreeVar "+")) (TreeVar "*")) TreeLeaf

    | 3+2*1
    = TreeBranch TreeLeaf (TreeBranch (TreeVar "+") (TreeBranch (TreeVar "*") TreeLeaf))

    | 1,3/2
    = TreeBranch (TreeBranch TreeLeaf TreeLeaf) (TreeBranch (TreeVar "/") TreeLeaf)

    | 1,6*2,9*3,5*4
    = TreeBranch (TreeBranch (TreeBranch TreeLeaf TreeLeaf) (TreeBranch (TreeVar "*") TreeLeaf)) (TreeBranch (TreeBranch (TreeVar "*") TreeLeaf) (TreeBranch (TreeVar "*") TreeLeaf))

    | 1,9,2,13,3,10,4,15,5,11,6,14,7,12,8
    = TreeBranch (TreeBranch (TreeBranch (TreeBranch TreeLeaf TreeLeaf) (TreeBranch TreeLeaf TreeLeaf)) (TreeBranch (TreeBranch TreeLeaf TreeLeaf) (TreeBranch TreeLeaf TreeLeaf))) (TreeBranch (TreeBranch (TreeBranch TreeLeaf TreeLeaf) (TreeBranch TreeLeaf TreeLeaf)) (TreeBranch (TreeBranch TreeLeaf TreeLeaf) (TreeBranch TreeLeaf TreeLeaf)))

    | 120,160,180,271,272,257,15,4,7,121
    = TreeBranch (TreeBranch (TreeBranch (TreeBranch (TreeBranch TreeLeaf TreeLeaf) TreeLeaf) TreeLeaf) TreeLeaf) (TreeBranch TreeLeaf (TreeBranch (TreeBranch TreeLeaf (TreeBranch (TreeBranch TreeLeaf TreeLeaf) TreeLeaf)) TreeLeaf))

    | 120,160**180,271+-272,257+*15,4//7//121
    = TreeBranch (TreeBranch (TreeBranch (TreeBranch (TreeBranch TreeLeaf TreeLeaf) (TreeVar "**")) TreeLeaf) (TreeVar "+-")) (TreeBranch TreeLeaf (TreeBranch (TreeBranch (TreeVar "+*") (TreeBranch (TreeBranch TreeLeaf (TreeVar "//")) (TreeVar "//"))) TreeLeaf))

Hev Compiling
-------------

    -> Tests for functionality "Compile Hev Program"

    -> Functionality "Compile Hev Program" is implemented by
    -> shell command
    -> "ghc src/hev.hs -e "compile \"%(test-text)\"""

Good.

    | 43,13,23,53,33
    = TreeBranch (TreeBranch TreeLeaf (TreeBranch (TreeBranch TreeLeaf TreeLeaf) TreeLeaf)) (TreeBranch TreeLeaf TreeLeaf)

Bad (incomplete patterns.)

    | 1,3,2
    = TreeLeaf

Bad (variables in state.)

    | 43,13,23,53+33
    = TreeLeaf

Bad (variables in body but not head.)

    | 43,13,23+53,33
    = TreeLeaf

Hev Binding
-----------

    -> Tests for functionality "Hev Binding"

    -> Functionality "Hev Binding" is implemented by
    -> shell command
    -> "ghc src/hev.hs -e "getBinding \"%(test-text)\" (UnifierBinding \"-\" TreeLeaf (UnifierBinding \"+\" (TreeBranch TreeLeaf TreeLeaf) UnifierNil))""

    | +
    = Just (TreeBranch TreeLeaf TreeLeaf)

    | -
    = Just TreeLeaf

    | *
    = Nothing

Hev Matching
------------

    -> Tests for functionality "Hev Matching"

    -> Functionality "Hev Matching" is implemented by
    -> shell command
    -> "ghc src/hev.hs -e "match %(test-text)""

    | (TreeLeaf) (TreeLeaf) UnifierNil
    = UnifierNil

    | (TreeLeaf) (TreeBranch TreeLeaf TreeLeaf) UnifierNil
    = UnificationFailure

Sorry about the backslashes here; Falderal is kind of naff that way. 

    | (TreeVar \"+\") (TreeBranch TreeLeaf TreeLeaf) UnifierNil
    = UnifierBinding "+" (TreeBranch TreeLeaf TreeLeaf) UnifierNil

    | (TreeBranch (TreeVar \"+\") TreeLeaf)
    | (TreeBranch (TreeBranch TreeLeaf TreeLeaf) TreeLeaf)
    | UnifierNil 
    = UnifierBinding "+" (TreeBranch TreeLeaf TreeLeaf) UnifierNil

    | (TreeBranch (TreeVar \"+\") TreeLeaf)
    | (TreeBranch TreeLeaf TreeLeaf)
    | UnifierNil
    = UnifierBinding "+" TreeLeaf UnifierNil

    | (TreeBranch (TreeVar \"+\") TreeLeaf)
    | (TreeBranch TreeLeaf (TreeBranch TreeLeaf TreeLeaf))
    | UnifierNil
    = UnificationFailure

Hev Rewriting
-------------

    -> Tests for functionality "Hev Rewriting"

    -> Functionality "Hev Rewriting" is implemented by
    -> shell command
    -> "ghc src/hev.hs -e "rewrite %(test-text)""

    | (TreeBranch TreeLeaf TreeLeaf)
    | (TreeBranch TreeLeaf TreeLeaf)
    | TreeLeaf
    = (True,TreeLeaf)

    | (TreeBranch TreeLeaf TreeLeaf)
    | (TreeBranch (TreeVar \"+\") TreeLeaf)
    | TreeLeaf
    = (True,TreeLeaf)

    | (TreeBranch (TreeBranch TreeLeaf TreeLeaf) TreeLeaf)
    | (TreeBranch (TreeVar \"+\") TreeLeaf)
    | (TreeVar \"+\")
    = (True,TreeBranch TreeLeaf TreeLeaf)

Hev Execution
-------------

    -> Tests for functionality "Hev Execution"

    -> Functionality "Hev Execution" is implemented by
    -> shell command
    -> "ghc src/hev.hs -e "run (compile \"%(test-text)\")""

Test a very simple (and completely ground) program.

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

    | 43,13,23,53,33
    = TreeLeaf

Test pattern-matching with a variable, causing a single rewrite.

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

After one rewrite of state: + matches branch at 17, new state becomes:

    --        X
    --       / \
    --   17 X   ,
    --     / \
    --    ,   X 4
    --       / \
    --      ,   ,

    | 49,9,25+36+16,81,64,73,17,4
    = TreeBranch (TreeBranch TreeLeaf (TreeBranch TreeLeaf TreeLeaf)) TreeLeaf

Test several rewrites of state.  The state is reduced to a single leaf.

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

    | 3*1*2*4,1,2,1
    = TreeLeaf

Test that rewriting is indeed top-down.
For this program, a bottom-up strategy produces a double branch,
while a top-down strategy produces a single branch.
(Work it out by hand if you doubt me!)

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

    | 71+8*27,19,29*99,6,37,7,61,47
    = TreeBranch TreeLeaf TreeLeaf

Test multiple rules.

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

    | 103,5,4-86-9,154,2,6,137,188,111,91,77
    = TreeLeaf

Obviously we can't test that execution of a nonterminating program truly never
terminates, but we give a nonterminating Hev program here as an example, which
you can run if you like:
    
    25,20*24,20,1*26,25

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
