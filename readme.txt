Programs developed to calculate the relational complexity of a group.
By Adam Harrison 05-04-2024.

CONTENTS OF FOLDER

RC_tup-1.0.1.g - program to find relational complexity using tuples (as described in alg 4.8)
RC_orb-1.1.g - program to find relational complexity using orbit extension - extended version of above (described in alg 4.13)

out-1.0 the outputs of the 1st program for degrees 2-30
out-1.1 the outputs of the 2nd program for degrees 2-50 (51-64 are available but not complete)

gap.sh - bash script to run programmes

TO RUN

To run these programs you need to have GAP 4.13.0. There is no promise of forward compability for future versions of GAP.
GAP can be downloaded from https://www.gap-system.org/Download/
The linked page details instructions on to how install GAP.

To run ensure that you are in the same directory as the gap file and the diectory contains the appropriate output directory 
(out-1.0 or out-1.1 depending on file ran). The directory can be change by editing the string in the output function.
Once in directory run:

./gap.sh <gap file>

where in place of <gap file>, put 'RC_tup-1.0.1.g', or 'RC_orb-1.1.g'

This will start gap with the chosen program loaded.

For RC_tup-1.0.1.g:
- to find irredundant base size of G - I(G): use function findI(G)
- to find the relational complexity of G with degree n use RC(G, n) 
- to find relational complexity for primitive groups of degree between lower and upper (inclusive) use runTests(lower, upper)

For RC_orb-1.1.g:
- to find irredundant base size of G - I(G): use function findI(G)
- to find the relational complexity of G with degree n, use RC(G, n, timeout) - where timeout is the number of seconds 
- to find relational complexity for primitive groups of degree between lower and upper (inclusive) use runTests(lower, upper)
