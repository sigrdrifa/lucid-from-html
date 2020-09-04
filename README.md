This repository hosts the code to the `lucid-from-html` tool, derived from [@dbaynard](https://github.com/dbaynard/lucid-from-html/).

Please report any issues (including examples of html files that don’t parse correctly) at the [issue tracker](https://github.com/alogic0/lucid-from-html/issues).

# Install using cabal

1.  Clone the repository.

2.  Install **hpack** tool. 

3.  Change to the directory of the repository.

4.  Run

        > hpack
        > cabal install

# Usage

To convert *filename*.html to lucid text, run

    > lucid-from-html -t -s filename.html > filename.hs

The options here: 

* -t &nbsp; _do not trim whitespace characters from the ends of the strings_ 
* -s &nbsp; _create a standalone code, ready to compile_

To see all options, run

    > lucid-from-html --help
    
If you successfully got _filename.hs_ you can generate html back again by

    > runhaskell filename.hs > filename-out.html
