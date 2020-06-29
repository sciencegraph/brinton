unlink("tests/1var_Specimen_files", recursive = TRUE)
unlink("tests/2var_Specimen_files", recursive = TRUE)

rmarkdown::render("tests/1var_Specimen.Rmd", "html_document")
rmarkdown::render("tests/2var_Specimen.Rmd", "html_document")
