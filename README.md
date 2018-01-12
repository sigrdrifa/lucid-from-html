This repository hosts the code to the `lucid-from-html` tool, derived from [@dbaynard](https://github.com/dbaynard/lucid-from-html/).

Please report any issues (including examples of html files that don’t parse correctly) at the [issue tracker](https://github.com/alogic0/lucid-from-html/issues).

# Install

1.  Clone the repository.

2.  Change to the directory of the repository.

3.  Run

        > cabal install

# Run

To convert *filename*.html to lucid text, run

    > lucid-from-html -t -s filename.html > filename.hs

The options here: 

* -t &nbsp; _do not trim the ends of the strings from the whitespace characters_ 
* -s &nbsp; _create a standalone code, ready to compile_

To see all options, run

    > lucid-from-html --help
    
If you successfully got _filename.hs_ you can generate html back again by

    > runhaskell filename.hs > filename-out.html
