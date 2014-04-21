#!/bin/sh

if [ x`which ghc` = x -a x`which runhugs` = x ]; then
    echo "Neither ghc nor runhugs found on search path"
    exit 1
fi

touch fixture.markdown

if [ ! x`which ghc` = x ]; then
    cat >>fixture.markdown <<EOF
    -> Functionality "Parse Hev Program" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "parse \"%(test-text)\"""

    -> Functionality "Compile Hev Program" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "compile \"%(test-text)\"""

    -> Functionality "Hev Binding" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "getBinding \"%(test-text)\" (UnifierBinding \"-\" TreeLeaf (UnifierBinding \"+\" (TreeBranch TreeLeaf TreeLeaf) UnifierNil))""

    -> Functionality "Hev Matching" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "match %(test-text)""

    -> Functionality "Hev Rewriting" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "rewrite %(test-text)""

    -> Functionality "Hev Execution" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "run (compile \"%(test-text)\")""

EOF
fi

if [ ! x`which runhugs` = x ]; then
    cat >>fixture.markdown <<EOF
    -> Functionality "Parse Hev Program" is implemented by
    -> shell command
    -> "runhugs src/Main.hs parse %(test-body-file)"

    -> Functionality "Compile Hev Program" is implemented by
    -> shell command
    -> "runhugs src/Main.hs compile %(test-body-file)"

    -> Functionality "Hev Binding" is implemented by
    -> shell command
    -> "runhugs src/Main.hs getbinding %(test-body-file)"

    -> Functionality "Hev Matching" is implemented by
    -> shell command
    -> "runhugs src/Main.hs match %(test-body-file)"

    -> Functionality "Hev Rewriting" is implemented by
    -> shell command
    -> "runhugs src/Main.hs rewrite %(test-body-file)"

    -> Functionality "Hev Execution" is implemented by
    -> shell command
    -> "runhugs src/Main.hs run %(test-body-file)"

EOF
fi

falderal fixture.markdown tests/Hev.markdown
rm -f fixture.markdown
