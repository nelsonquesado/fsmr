library(testthat)

test_that("fsm_zone preserves arbitrary attributes and keys by zone_id", {
  zones <- data.frame(
    zone_code = c(10, 20, 30),
    residents = c(1200, 2500, 1800),
    employment = c(400, 1300, 900),
    school_places = c(150, 220, 80)
  )

  z <- fsm_zone(zones, "zone_code")

  expect_s3_class(z, "fsm_zone")
  expect_s3_class(z, "data.table")
  expect_true(data.table::haskey(z))
  expect_identical(data.table::key(z), "zone_id")
  expect_identical(
    names(z),
    c("zone_id", "residents", "employment", "school_places")
  )
})

test_that("fsm_zone validates zone uniqueness and OD coverage", {
  dup_zones <- data.frame(
    zone_code = c(1, 1, 2),
    x = c(10, 11, 12)
  )

  expect_error(
    fsm_zone(dup_zones, "zone_code"),
    "uniquely identify zones"
  )

  od <- fsm_toy_od
  zones <- fsm_toy_zone

  expect_identical(fsm_check_zone_od(zones, od), TRUE)

  incomplete_zones <- zones[-1]

  expect_error(
    fsm_check_zone_od(incomplete_zones, od),
    "does not cover all OD origins/destinations"
  )
})

test_that("fsm_toy_zone is a keyed fsm_zone object", {
  expect_s3_class(fsm_toy_zone, "fsm_zone")
  expect_s3_class(fsm_toy_zone, "data.table")
  expect_identical(data.table::key(fsm_toy_zone), "zone_id")
  expect_equal(nrow(fsm_toy_zone), 7)
  expect_true(all(c("area_type", "income_group") %in% names(fsm_toy_zone)))
  classification_cells <- unique(
    fsm_toy_zone[, c("area_type", "income_group"), with = FALSE]
  )
  expect_lt(nrow(classification_cells), nrow(fsm_toy_zone))
  expect_false(7 %in% c(fsm_toy_od$origin, fsm_toy_od$destination))
  expect_true(is.na(fsm_toy_zone[zone_id == 7, name]))
  expect_true(is.na(fsm_toy_zone[zone_id == 6, income_pc]))
})

test_that("summary.fsm_zone describes numeric attribute location and dispersion", {
  zone_summary <- summary(fsm_toy_zone)
  population <- zone_summary$statistics[
    zone_summary$statistics$attribute == "population",
  ]

  expect_s3_class(zone_summary, "summary.fsm_zone")
  expect_identical(
    zone_summary$numeric_attributes,
    c("population", "jobs", "households", "income_pc")
  )
  expect_equal(population$mean, mean(fsm_toy_zone$population))
  expect_equal(population$sd, stats::sd(fsm_toy_zone$population))
  expect_equal(population$min, min(fsm_toy_zone$population))
  expect_equal(population$max, max(fsm_toy_zone$population))
  expect_equal(population$missing, 0)
  expect_equal(
    zone_summary$statistics$missing[
      zone_summary$statistics$attribute == "income_pc"
    ],
    1
  )

  printed_summary <- paste(capture.output(print(zone_summary)), collapse = "\n")
  expect_match(printed_summary, "Numeric attributes")
  expect_match(printed_summary, "population")
})

test_that("print.fsm_zone honors n", {
  output <- capture.output(result <- print(fsm_toy_zone, n = Inf))

  expect_identical(result, fsm_toy_zone)
  expect_true(any(grepl("rows: 7", output, fixed = TRUE)))
  expect_false(any(grepl("display:", output, fixed = TRUE)))
})

test_that("fsm_od_totals returns observed productions and attractions", {
  od <- fsm_toy_od
  zones <- fsm_toy_zone

  totals <- fsm_od_totals(od, zones)

  expect_s3_class(totals, "data.table")
  expect_identical(data.table::key(totals), "zone_id")
  expect_identical(names(totals), c("zone_id", "production", "attraction"))
  expect_equal(totals[zone_id == 1, production], 3800)
  expect_equal(totals[zone_id == 1, attraction], 4900)
  expect_equal(totals[zone_id == 2, production], 3760)
  expect_equal(totals[zone_id == 2, attraction], 3830)
  expect_equal(totals[zone_id == 5, production], 1230)
  expect_equal(totals[zone_id == 5, attraction], 0)
  expect_equal(totals[zone_id == 6, production], 0)
  expect_equal(totals[zone_id == 6, attraction], 1440)
  expect_equal(totals[zone_id == 7, production], 0)
  expect_equal(totals[zone_id == 7, attraction], 0)

  totals_output <- capture.output(result <- print(totals))
  expect_identical(result, totals)
  expect_true(any(grepl("rows: 7", totals_output, fixed = TRUE)))
  expect_false(any(grepl("display:", totals_output, fixed = TRUE)))

  totals_summary <- summary(totals)
  expect_s3_class(totals_summary, "summary.fsm_od_totals")
  expect_equal(totals_summary$total_production, 14290)
  expect_equal(totals_summary$total_attraction, 14290)
  expect_equal(totals_summary$imbalance, 0)
  expect_true(totals_summary$balanced)
  expect_equal(
    totals_summary$statistics$zero_zones,
    c(2, 2)
  )
  expect_equal(
    totals_summary$statistics$sd,
    c(stats::sd(totals$production), stats::sd(totals$attraction))
  )

  printed_summary <- paste(capture.output(print(totals_summary)), collapse = "\n")
  expect_match(printed_summary, "Margin statistics")
  expect_match(printed_summary, "balanced: yes")
})

test_that("fsm_od_totals requires an fsm_od input", {
  expect_error(
    fsm_od_totals(data.frame()),
    "must be an `fsm_od` object"
  )
})
