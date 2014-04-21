Test Suite for Hev Internals
============================

Hev Matching
------------

    -> Tests for functionality "Hev Matching"

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
