## NOTE
This is a first submission.

## Test environments
* local Windows 10, R 3.4.4
* travis Ubuntu Xenial 16.04
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs. Just one NOTE
concerning the possibly mis-spelled words in "EDA".

Vinettes now load precompiled figures.

While runing CMD check the system does not find 
the temporary file wideplot.html. This behaviour 
seems to be particular of CMD check since 
devtools::run_examples() works properly.

Examples are now simpler in order to speed up the 
outputs.

Arguments of functions have been added in order to 
write the examples/vignettes/tests to tempdir() 
by default.






