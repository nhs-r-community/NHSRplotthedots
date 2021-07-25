test_that("capitalize correctly capitalises strings", {
  expect_equal(capitalise("hello world"), "Hello world")
  expect_equal(capitalise("HELLO WORLD"), "HELLO WORLD")
  expect_equal(capitalise("1hi World"), "1hi World")
})

test_that("titleCase correctly converts a string to title case", {
  expect_equal(titleCase("hello_world"), "Hello World")
})
