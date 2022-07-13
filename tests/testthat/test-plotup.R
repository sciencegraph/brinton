test_that("1var_Specimen works", {
  figures <- list.files("../../tests/1var_Specimen_files/figure-html")
  for (i in seq_along(figures)) {
  expect_true(visualTest::isSimilar(paste0("../../tests/1var_Specimen_files/figure-html/", figures[1]), paste0("../../vignettes/figures/", figures[1]), threshold = 8))}
})

test_that("2var_Specimen works", {
  figures <- list.files("../../tests/2var_Specimen_files/figure-html")
  for (i in seq_along(figures)) {
    expect_true(visualTest::isSimilar(paste0("../../tests/2var_Specimen_files/figure-html/", figures[1]), paste0("../../vignettes/figures/", figures[1]), threshold = 8))}
})
