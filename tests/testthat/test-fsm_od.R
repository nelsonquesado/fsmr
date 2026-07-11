library(testthat)

test_that("fsm_od creates a keyed data.table by reference for a dummy network", {
  # Minimal dummy example (3 zones)
  dummy_data <- data.frame(
    zone_o = c(1, 1, 2, 3), 
    zone_d = c(2, 3, 1, 2), 
    trips = c(100, 50, 200, 30)
  )
  
  od <- fsm_od(data = dummy_data, 
               origins = "zone_o", 
               destinations = "zone_d", 
               demands = "trips")
  
  # Check class assignment
  expect_s3_class(od, "fsm_od")
  expect_s3_class(od, "data.table")
  
  # Verify binary search keys are set correctly
  expect_true(data.table::haskey(od))
  expect_equal(data.table::key(od), c("origin", "destination"))
})

test_that("fsm_toy_od is a keyed fsm_od object", {
  expect_s3_class(fsm_toy_od, "fsm_od")
  expect_s3_class(fsm_toy_od, "data.table")
  expect_identical(names(fsm_toy_od), c("origin", "destination", "demand"))
  expect_identical(data.table::key(fsm_toy_od), c("origin", "destination"))
  expect_equal(nrow(fsm_toy_od), 24)
  expect_equal(sum(fsm_toy_od$demand > 0, na.rm = TRUE), 19)
  expect_equal(sum(fsm_toy_od$demand == 0, na.rm = TRUE), 4)
  expect_equal(sum(is.na(fsm_toy_od$demand)), 1)
  expect_true(is.na(fsm_toy_od[origin == 3 & destination == 5, demand]))
  expect_false(7 %in% c(fsm_toy_od$origin, fsm_toy_od$destination))

  od_summary <- summary(fsm_toy_od)
  expect_equal(od_summary$total_demand, 14290)
  expect_equal(od_summary$missing_demand, 1)
  expect_equal(od_summary$zero_demand, 4)
})

test_that("fsm_od rejects invalid demand and duplicate pairs", {
  expect_error(
    fsm_od(data.frame(o = c(1, 1), d = c(2, 2), q = c(10, 20)), "o", "d", "q"),
    "must be unique"
  )
  expect_error(
    fsm_od(data.frame(o = 1, d = 2, q = -1), "o", "d", "q"),
    "negative"
  )
  expect_error(
    fsm_od(data.frame(o = 1, d = 2, q = Inf), "o", "d", "q"),
    "finite"
  )
  expect_error(
    fsm_od(data.frame(o = NA_integer_, d = 2, q = 1), "o", "d", "q"),
    "origin"
  )
})

test_that("fsm_od retains missing demand observations", {
  od <- fsm_od(
    data.frame(o = c(1, 2), d = c(2, 1), q = c(10, NA_real_)),
    "o",
    "d",
    "q"
  )

  expect_equal(sum(is.na(od$demand)), 1)
  expect_equal(summary(od)$missing_demand, 1)
})

test_that("plot.fsm_od returns the correctly oriented OD matrix", {
  mat <- plot(fsm_toy_od)

  expect_equal(mat["1", "2"], 1800)
  expect_equal(mat["2", "1"], 2100)
  expect_equal(mat["1", "5"], 0)
  expect_true(is.na(mat["3", "5"]))
})

test_that("print.fsm_od honors n", {
  default_output <- capture.output(result <- print(fsm_toy_od))
  full_output <- capture.output(print(fsm_toy_od, n = 30))

  expect_identical(result, fsm_toy_od)
  expect_true(any(grepl("rows: 24", default_output, fixed = TRUE)))
  expect_false(any(grepl("display:", default_output, fixed = TRUE)))
  expect_false(any(grepl("display:", full_output, fixed = TRUE)))
  expect_error(print(fsm_toy_od, n = -1), "non-negative whole number")
})
