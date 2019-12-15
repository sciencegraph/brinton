## NOTE
This is a submission of package not currently 
available on CRAN.

## Test environments
* local Windows 10, R 3.4.4
* travis Ubuntu Xenial 16.04
* R Under development (unstable) (2019-12-02 r77499)

## R CMD check results
There were no ERRORs or WARNINGs. Just one NOTE
while checking CRAN incoming feasibility.

Examples that produce a html file, are now wrapped 
into \dontrun{} in order to avoid leaving browser open
while checking.

The requirement of pandoc is now declared and used 
conditionally. It has been tested on a machine 
without pandoc and exits with a warning that reminds 
that pandoc is requiered.
