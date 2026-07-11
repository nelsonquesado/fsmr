library(testthat)

test_that("fsm_reaggregate_matrix sums to a coarser system", {
  m <- matrix(
    c(0, 1, 2, 3,
      4, 0, 5, 6,
      7, 8, 0, 9,
      1, 2, 3, 0),
    nrow = 4,
    byrow = TRUE
  )
  rownames(m) <- colnames(m) <- c("a", "b", "c", "d")

  map <- data.frame(
    old = c("a", "b", "c", "d"),
    new = c("x", "x", "y", "y")
  )

  out <- fsm_reaggregate_matrix(m, map)

  expect_true(is.matrix(out))
  expect_identical(rownames(out), c("x", "y"))
  expect_identical(colnames(out), c("x", "y"))
  expect_equal(out["x", "x"], 5)
  expect_equal(out["x", "y"], 16)
  expect_equal(out["y", "x"], 18)
  expect_equal(out["y", "y"], 12)
})

test_that("fsm_reaggregate_matrix supports numeric targets", {
  m <- matrix(1:6, nrow = 2, dimnames = list(c("a", "b"), c("u", "v", "w")))
  row_map <- data.frame(old = c("a", "b"), new = c(10, 20))
  col_map <- data.frame(source = c("u", "v", "w"), target = c(100, 100, 200))

  out <- fsm_reaggregate_matrix(
    m,
    row_map,
    col_map,
    from = "old",
    to = "new",
    col_from = "source",
    col_to = "target"
  )

  expect_identical(rownames(out), c("10", "20"))
  expect_identical(colnames(out), c("100", "200"))
  expect_equal(sum(out), sum(m))
})

test_that("fsm_reaggregate_matrix rejects ambiguous correspondence tables", {
  m <- matrix(1:4, nrow = 2, dimnames = list(c("a", "b"), c("a", "b")))
  duplicate_map <- data.frame(old = c("a", "a", "b"), new = c("x", "y", "y"))

  expect_error(
    fsm_reaggregate_matrix(m, duplicate_map),
    "must be unique"
  )
})
