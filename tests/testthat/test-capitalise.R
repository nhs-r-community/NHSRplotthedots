test_that("it correctly capitalises strings", {
  expect_equal(capitalise("hello world"), "Hello world")
  expect_equal(capitalise("HELLO WORLD"), "HELLO WORLD")
  expect_equal(capitalise("1hi World"), "1hi World")
})
