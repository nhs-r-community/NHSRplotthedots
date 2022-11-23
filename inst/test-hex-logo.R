testthat::test_that("hex_logo works", {
  tmp_file <- tempfile(fileext = ".png")
  out <- hex_logo(out_filename = tmp_file)
  testthat::expect_equal(file.exists(tmp_file), TRUE)
  unlink(tmp_file)
  testthat::expect_equal(file.exists(tmp_file), FALSE)
})
