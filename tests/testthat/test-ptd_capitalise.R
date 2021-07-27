test_that("capitalize correctly capitalises strings", {
  expect_equal(ptd_capitalise("hello world"), "Hello world")
  expect_equal(ptd_capitalise("HELLO WORLD"), "HELLO WORLD")
  expect_equal(ptd_capitalise("1hi World"), "1hi World")
})

test_that("titleCase correctly converts a string to title case", {
  expect_equal(ptd_title_case("hello_world"), "Hello World")
})
