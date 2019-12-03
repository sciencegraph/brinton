## NOTE
This is a first submission.

## Test environments
* local Windows 10, R 3.4.4
* travis Ubuntu Xenial 16.04
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs. Just one NOTE
concerning the possibly mis-spelled words in "EDA".

Vignettes now load precompiled figures.

In order to avoid leaving browser open, examples that
produce a html file, are now wrapped in \dontrun{}.

Arguments of functions have been added in order to 
write the examples/vignettes/tests to tempdir() 
by default.

The requirement of pandoc is now declared and used conditionally.





