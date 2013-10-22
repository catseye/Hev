Test Suite for Hev
============================

This test suite is written in the format of Falderal.  It is far from
exhaustive, but provides a basic sanity check that the language we've
implemented comes close to Hev.

Hev Parsing
-----------------

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
-----------------

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

