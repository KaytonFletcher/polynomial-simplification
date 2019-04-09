# Polynomial Simplification 
## Kayton Fletcher

To compile the program, run **make**

The input files take the form of _p#.in_ where # is any number
from 1 to the number of tests. When you run **make tests**, it will
create output files that correspond to the respective input file, namely _p#.out_

If you change the code at all, insure you run **make clean** before
recompiling with **make**

For algebraic expressions that cancel terms, I left the solution with the said term, with a coefficient of zero.

(i.e. 4x^2 + 3x - 4x^2 -> 0x^2 + 3x)

Additionally, I sorted the polynomials from highest to lowest power. I also chose to print negatives as subtraction, opting to not print parenthesis around negated terms (i.e. 7x^2 - 3x instead of 7x^2 + (-3x) )

