## NOTE
This is a first submission.

## Test environments
* local Windows 10, R 3.4.4
* travis Ubuntu Xenial 16.04
* win-builder (devel)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

Although, openFileInOS runs properly in R session 
but fails to open wideplot.html during CMD ckeck.
In CMD check it seems that the temporary directory
is deleted before that openFileInOS tries to open
the file. We think that this behaviour is particular
of CMD ckeck but that it actually works.

Acronyms are now explained and function names 
are unquoted in the description.

Examples are now simpler in order to speed up the 
outputs.

Arguments have been added in order to write the 
examples/vignettes/tests to tempdir() by default.

We would like to thank the helpful revision that 
Swetlana Herbrandt have provided.




