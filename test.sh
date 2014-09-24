#!/bin/sh

if [ x`which ghc` = x -a x`which runhugs` = x ]; then
    echo "Neither ghc nor runhugs found on search path"
    exit 1
fi

touch fixture.markdown
touch Internals.markdown

if [ ! x`which ghc` = x ]; then
    cat >>fixture.markdown <<EOF
    -> Functionality "Parse Hev Program" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "parse \"%(test-body-text)\"""

    -> Functionality "Compile Hev Program" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "compile \"%(test-body-text)\"""

    -> Functionality "Hev Binding" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "getBinding \"%(test-body-text)\" (UnifierBinding \"-\" TreeLeaf (UnifierBinding \"+\" (TreeBranch TreeLeaf TreeLeaf) UnifierNil))""

    -> Functionality "Hev Matching" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "match %(test-body-text)""

    -> Functionality "Hev Rewriting" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "rewrite %(test-body-text)""

    -> Functionality "Hev Execution" is implemented by
    -> shell command
    -> "ghc src/Hev.hs -e "run (compile \"%(test-body-text)\")""

EOF
    cp tests/Internals.markdown .
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

    -> Functionality "Hev Execution" is implemented by
    -> shell command
    -> "runhugs src/Main.hs run %(test-body-file)"

EOF
fi

falderal fixture.markdown Internals.markdown tests/Hev.markdown
RESULT=$?
rm -f fixture.markdown Internals.markdown
exit $RESULT
