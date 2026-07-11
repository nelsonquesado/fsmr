library(testthat)

test_that("trip records reproduce positive toy OD demand", {
  od <- fsm_od_from_trips(fsm_toy_population)
  expected <- fsm_toy_od[!is.na(demand) & demand > 0]

  expect_s3_class(od, "fsm_od")
  expect_equal(nrow(od), 19L)
  expect_equal(od$origin, expected$origin)
  expect_equal(od$destination, expected$destination)
  expect_equal(od$demand, expected$demand)
})

test_that("trip helpers filter modes and derive totals and zones", {
  car_od <- fsm_od_from_trips(fsm_toy_population, modes = "car")
  zones <- fsm_zone_from_trips(fsm_toy_population)
  complete_zones <- fsm_zone_from_trips(fsm_toy_population, include = 7L)
  totals <- fsm_od_totals_from_trips(
    fsm_toy_population,
    zones = complete_zones
  )

  expect_equal(
    sum(car_od$demand),
    sum(vapply(
      strsplit(fsm_toy_population$mode, "+", fixed = TRUE),
      function(x) "car" %in% x,
      logical(1)
    ) * fsm_toy_population$population_count)
  )
  transit_od <- fsm_od_from_trips(fsm_toy_population, modes = "transit")
  expect_equal(
    sum(transit_od$demand),
    sum(vapply(
      strsplit(fsm_toy_population$mode, "+", fixed = TRUE),
      function(x) "transit" %in% x,
      logical(1)
    ) * fsm_toy_population$population_count)
  )
  expect_equal(zones$zone_id, 1:6)
  expect_equal(complete_zones$zone_id, 1:7)
  expect_equal(sum(totals$production), 14290)
  expect_equal(sum(totals$attraction), 14290)
  expect_equal(totals[zone_id == 7, production], 0)
  expect_equal(totals[zone_id == 7, attraction], 0)
})

test_that("trip helpers support weights and clean validation", {
  trips <- data.frame(
    from = c(1, 1, 2),
    to = c(2, 2, 1),
    expansion = c(1.5, 2.5, 3),
    travel_mode = c("car", "walk", "car")
  )
  od <- fsm_od_from_trips(
    trips,
    origins = "from",
    destinations = "to",
    weights = "expansion"
  )

  expect_equal(od[origin == 1 & destination == 2, demand], 4)
  expect_equal(od[origin == 2 & destination == 1, demand], 3)
  expect_error(fsm_od_from_trips(), "`data` must be supplied")
  expect_error(
    fsm_od_from_trips(trips, origins = "missing"),
    "Missing required column"
  )
  expect_error(
    fsm_od_from_trips(
      trips,
      origins = "from",
      destinations = "to",
      modes = "bus",
      mode = "travel_mode"
    ),
    "No trip records match"
  )
  expect_error(
    fsm_zone_from_trips(trips, origins = "from", destinations = "to", include = NA),
    "`include` cannot contain missing values"
  )
})
