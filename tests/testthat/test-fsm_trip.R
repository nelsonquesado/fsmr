library(testthat)

test_that("fsm_trip standardizes an origin-destination survey", {
  survey <- data.frame(
    id = 1:3,
    from = c(1, 2, 1),
    to = c(2, 1, 3),
    factor = c(1.5, 2, 1)
  )
  trips <- fsm_trip(survey, "id", "from", "to", "factor")

  expect_s3_class(trips, "fsm_trip")
  expect_identical(data.table::key(trips), "trip_id")
  expect_identical(names(trips)[1:4], c("trip_id", "origin", "destination", "expansion_factor"))
  expect_error(fsm_trip(), "`data` must be supplied")
  expect_error(fsm_trip(survey, "id", "from", "missing"), "Missing required column")
})

test_that("fsm_toy_trip contains one origin-destination survey trip per row", {
  expect_s3_class(fsm_toy_trip, "fsm_trip")
  expect_equal(nrow(fsm_toy_trip), 14290L)
  expect_identical(data.table::key(fsm_toy_trip), "trip_id")
  expect_identical(fsm_toy_trip$trip_id, seq_len(14290L))
  expect_true(all(fsm_toy_trip$expansion_factor == 1))
  expect_false("zone_id" %in% names(fsm_toy_trip))
  individuals <- fsm_toy_trip[!duplicated(individual_id)]
  expect_gt(mean(individuals$daily_trips), 2.3)
  expect_lt(mean(individuals$daily_trips), 2.5)
  expect_equal(mean(individuals$income_pc), 1800)
  expect_equal(range(fsm_toy_trip$income_pc), c(0, 4000))
  expect_gt(length(unique(individuals$income_pc)), 1000L)
  expect_equal(
    mean(individuals$vehicles),
    round(0.35 * nrow(individuals)) / nrow(individuals)
  )
  expect_equal(
    mean(individuals$gender == "female"),
    round(0.54 * nrow(individuals)) / nrow(individuals)
  )
  continuity <- fsm_toy_trip[, {
    all(destination[-.N] == origin[-1L])
  }, by = individual_id][["V1"]]
  expect_true(all(continuity))
})

test_that("fsm_trip inspection uses expansion factors", {
  output <- capture.output(print(fsm_toy_trip))
  trip_summary <- summary(fsm_toy_trip)

  expect_true(any(grepl("trips: 14,290", output, fixed = TRUE)))
  expect_false(any(grepl("rows:", output, fixed = TRUE)))
  expect_s3_class(trip_summary, "summary.fsm_trip")
  expect_equal(trip_summary$trips, 14290)
  expect_true(nrow(trip_summary$categorical_levels) > 0)
  expect_false("individual_id" %in% trip_summary$numeric$attribute)
  expect_identical(plot(fsm_toy_trip), fsm_toy_trip)
})
