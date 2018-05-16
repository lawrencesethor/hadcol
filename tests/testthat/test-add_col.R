context("add_col")

test_that("can add column in specified position", {
  df <- data.frame(x = 1)

  expect_equal(
    add_col(df, "y", 2),
    data.frame(x = 1, y = 2)
  )
  expect_equal(
    add_col(df, "y", 2, where = 1),
    data.frame(y = 2, x = 1)
  )
})

test_that("can override existing column", {
  df <- data.frame(x = 1)

  expect_equal(
    add_col(df, "x", 2),
    data.frame(x = 2)
  )
})

