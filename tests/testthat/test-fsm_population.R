library(testthat)

test_that("fsm_population preserves arbitrary attributes and keys by population_id", {
  pop <- data.frame(
    pid = 1:3,
    income_pc = c(1800, 2400, 1200),
    vehicles = c(1, 2, 0)
  )

  p <- fsm_population(pop, "pid")

  expect_s3_class(p, "fsm_population")
  expect_s3_class(p, "data.table")
  expect_true(data.table::haskey(p))
  expect_identical(data.table::key(p), "population_id")
  expect_identical(
    names(p),
    c("population_id", "population_count", "income_pc", "vehicles")
  )
  expect_false("zone_id" %in% names(p))
  expect_equal(p$population_count, rep(1L, 3L))
})

test_that("fsm_population supports groups of identical individuals", {
  pop <- data.frame(
    pid = 1:3,
    people = c(1, 4, 2),
    income_pc = c(1800, 2400, 1200)
  )

  p <- fsm_population(pop, "pid", population_count = "people")

  expect_identical(names(p)[1:2], c("population_id", "population_count"))
  expect_equal(p$population_count, c(1, 4, 2))

  expect_error(
    fsm_population(
      data.frame(pid = 1:2, people = c(1, 1.5)),
      "pid",
      population_count = "people"
    ),
    "positive whole numbers"
  )
})

test_that("fsm_toy_population is a keyed fsm_population object", {
  expect_s3_class(fsm_toy_population, "fsm_population")
  expect_s3_class(fsm_toy_population, "data.table")
  expect_identical(data.table::key(fsm_toy_population), "population_id")
  expect_equal(nrow(fsm_toy_population), 13599L)
  expect_equal(sum(fsm_toy_population$population_count), 14290L)
  person_weights <- with(
    fsm_toy_population,
    population_count / daily_trips
  )
  expect_equal(sum(person_weights), 7145)
  expect_equal(sum(fsm_toy_population$population_count) / sum(person_weights), 2)
  expect_setequal(unique(fsm_toy_population$daily_trips), 1:3)
  people_by_daily_trips <- with(
    fsm_toy_population,
    tapply(person_weights, daily_trips, sum)
  )
  expect_equal(as.vector(people_by_daily_trips), c(1786, 3573, 1786))
  expect_true(any(fsm_toy_population$population_count > 1L))
  expect_equal(lengths(fsm_toy_population$individual_id), fsm_toy_population$population_count)
  expect_setequal(
    unique(unlist(fsm_toy_population$individual_id)),
    seq_len(7145L)
  )
  attributes <- setdiff(
    names(fsm_toy_population),
    c("population_id", "population_count", "individual_id")
  )
  expect_false(any(duplicated(fsm_toy_population, by = attributes)))
  expect_true(all(c(
    "origin", "destination", "mode", "purpose_pair", "time_of_day"
  ) %in% names(fsm_toy_population)))
  expect_setequal(
    unique(fsm_toy_population$mode),
    c(
      "car", "transit", "walk", "bicycle", "ridehailing", "motorcycle",
      "walk+transit", "walk+ridehailing"
    )
  )
  expect_false(any(c("car+walk", "walk+transit+walk") %in% fsm_toy_population$mode))
  expect_true(any(grepl("+", fsm_toy_population$mode, fixed = TRUE)))
  expect_true(all(c(
    "home-work", "work-home", "work-work", "work-leisure"
  ) %in% fsm_toy_population$purpose_pair))
  purpose_components <- unique(unlist(strsplit(
    fsm_toy_population$purpose_pair,
    "-",
    fixed = TRUE
  )))
  expect_true(all(c("study", "shopping", "leisure", "health") %in% purpose_components))
  expect_true(any(grepl("^home-|-home$", fsm_toy_population$purpose_pair)))
  expect_true(any(!grepl("^home-|-home$", fsm_toy_population$purpose_pair)))
  expect_true(all(grepl(
    "^([01][0-9]|2[0-3]):[0-5][0-9]:00$",
    fsm_toy_population$time_of_day
  )))
  expect_equal(
    weighted.mean(
      fsm_toy_population$income_pc,
      person_weights,
      na.rm = TRUE
    ),
    1800
  )
  expect_equal(range(fsm_toy_population$income_pc, na.rm = TRUE), c(0, 4000))
  expect_true(any(!fsm_toy_population$employed & fsm_toy_population$income_pc == 0))
  expect_lt(
    max(fsm_toy_population[employed == FALSE, income_pc]),
    min(fsm_toy_population[employed == TRUE, income_pc])
  )
  expect_equal(
    weighted.mean(
      fsm_toy_population$vehicles,
      person_weights
    ),
    round(0.35 * 7145) / 7145
  )
  gender_totals <- with(
    fsm_toy_population,
    tapply(person_weights, gender, sum)
  )
  expect_equal(
    as.vector(gender_totals[c("female", "male")]),
    c(3858, 3287)
  )
  expect_type(fsm_toy_population$age, "integer")
  expect_type(fsm_toy_population$employed, "logical")
  expect_type(fsm_toy_population$student, "logical")
  expect_type(fsm_toy_population$vehicles, "logical")
  expect_false("workers" %in% names(fsm_toy_population))
})

test_that("fsm_population printing accepts n and plotting returns the object", {
  default_output <- capture.output(print(fsm_toy_population))
  full_output <- capture.output(print(fsm_toy_population, n = Inf))
  expected_rows <- paste0("rows: ", format(nrow(fsm_toy_population), big.mark = ","))
  expect_true(any(grepl(expected_rows, default_output, fixed = TRUE)))
  expect_false(any(grepl("display:", default_output, fixed = TRUE)))
  expect_false(any(grepl("display:", full_output, fixed = TRUE)))
  expect_true(any(grepl("represented trips: 14,290", full_output, fixed = TRUE)))
  expect_identical(plot(fsm_toy_population), fsm_toy_population)
})
