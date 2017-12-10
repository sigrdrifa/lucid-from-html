This repository hosts the code to the `lucid-from-html` tool, derived from [@dbaynard](https://github.com/dbaynard/lucid-from-html/).
Some examples are work, but test suite is in progress.

Please report any issues (including examples of html files that don’t parse correctly) at the [issue tracker](https://github.com/alogic0/lucid-from-html/issues).

# Install

1.  Clone the repository.

2.  Change to the directory of the repository.

3.  Run

        > cabal install

# Run

To convert *filename*.html to lucid text, run

    > lucid-from-html -t -s filename.html > filename.hs

The options here: _Do not trim ends of the strings from the space-symbols_ 
and _Create standalone code, ready to compile_

To see all options, run

    > lucid-from-html --help
