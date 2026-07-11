library(testthat)

test_that("fsm_generation can fit and predict a regression model", {
  zones <- fsm_toy_zone
  od <- fsm_toy_od
  totals <- fsm_od_totals(od, zones)

  fit <- fsm_generation(
    zones,
    totals,
    method = "regression",
    production_formula = production ~ 0 + population,
    attraction_formula = attraction ~ 0 + jobs
  )

  pred <- predict(fit, zones)
  training_pred <- predict(fit)
  plotted <- plot(fit)

  expect_s3_class(fit, "fsm_generation")
  expect_null(fit$n_records)
  expect_s3_class(pred, "data.table")
  expect_identical(data.table::key(pred), "zone_id")
  expect_identical(names(pred), c("zone_id", "production", "attraction"))
  expect_identical(training_pred, pred)
  expect_identical(plotted, pred)
  expect_true(all(pred$production == round(pred$production)))
  expect_true(all(pred$attraction == round(pred$attraction)))

  fit_summary <- summary(fit)
  expect_s3_class(fit_summary, "summary.fsm_generation")
  expect_identical(fit_summary$method, "regression")
  expect_null(fit_summary$n_records)
  expect_equal(fit_summary$training_records, nrow(fit$training_data))
  expect_identical(names(fit_summary$models), c("production", "attraction"))
  expect_true(all(vapply(fit_summary$models, function(x) {
    all(c("formula", "coefficients", "r_squared", "adjusted_r_squared") %in% names(x))
  }, logical(1))))
  printed_summary <- paste(capture.output(print(fit_summary)), collapse = "\n")
  expect_match(printed_summary, "Production model")
  expect_match(printed_summary, "Attraction model")

  printed_fit <- paste(capture.output(print(fit)), collapse = "\n")
  expect_match(printed_fit, "training records: 7")
  expect_false(grepl("\\$training_data", printed_fit))
})

test_that("regression fits only the outcomes with supplied formulas", {
  zones <- fsm_toy_zone
  totals <- fsm_od_totals(fsm_toy_od, zones)

  production_fit <- fsm_generation(
    zones,
    totals[, c("zone_id", "production"), with = FALSE],
    method = "regression",
    production_formula = production ~ 0 + population
  )
  production_pred <- predict(production_fit, zones)

  expect_identical(production_fit$outcomes, "production")
  expect_s3_class(production_fit$production_fit, "lm")
  expect_null(production_fit$attraction_fit)
  expect_identical(names(production_pred), c("zone_id", "production"))
  expect_true(all(production_pred$production == round(production_pred$production)))
  expect_identical(names(summary(production_fit)$models), "production")

  attraction_fit <- fsm_generation(
    zones,
    totals[, c("zone_id", "attraction"), with = FALSE],
    method = "regression",
    attraction_formula = attraction ~ 0 + jobs
  )
  attraction_pred <- predict(attraction_fit, zones)

  expect_identical(attraction_fit$outcomes, "attraction")
  expect_null(attraction_fit$production_fit)
  expect_s3_class(attraction_fit$attraction_fit, "lm")
  expect_identical(names(attraction_pred), c("zone_id", "attraction"))

  expect_error(
    fsm_generation(zones, totals, method = "regression"),
    "At least one"
  )
})

test_that("fsm_generation returns regression coefficient confidence intervals", {
  zones <- fsm_toy_zone
  totals <- fsm_od_totals(fsm_toy_od, zones)
  fit <- fsm_generation(
    zones,
    totals,
    method = "regression",
    production_formula = production ~ 0 + population,
    attraction_formula = attraction ~ 0 + jobs
  )

  intervals <- confint(fit, level = 0.90)

  expect_identical(names(intervals), c("production", "attraction"))
  expect_equal(intervals$production, stats::confint(fit$production_fit, level = 0.90))
  expect_equal(intervals$attraction, stats::confint(fit$attraction_fit, level = 0.90))

  production_fit <- fsm_generation(
    zones,
    totals[, c("zone_id", "production"), with = FALSE],
    method = "regression",
    production_formula = production ~ 0 + population
  )
  expect_identical(names(confint(production_fit)), "production")

  growth_fit <- fsm_generation(
    totals = totals,
    method = "growth_factor",
    growth_factor = 1.1
  )
  expect_error(
    confint(growth_fit),
    "available only for"
  )
})

test_that("regression retains predictions with missing inputs as NA", {
  zones <- fsm_toy_zone
  totals <- fsm_od_totals(fsm_toy_od, zones)
  fit <- fsm_generation(
    zones,
    totals,
    method = "regression",
    production_formula = production ~ population + income_pc,
    attraction_formula = attraction ~ jobs
  )

  expect_warning(
    pred <- predict(fit),
    "production \\[zone_id 6\\]"
  )
  expect_true(is.na(pred[zone_id == 6, production]))
  expect_false(is.na(pred[zone_id == 6, attraction]))
  expect_false(anyNA(pred[zone_id != 6, production]))
})

test_that("fsm_generation can use a custom fit and predict function", {
  zones <- fsm_toy_zone
  od <- fsm_toy_od
  totals <- fsm_od_totals(od, zones)

  fit <- fsm_generation(
    zones,
    totals,
    method = "custom",
    custom_fit = function(data, totals, ...) {
      list(mean_production = mean(totals$production), mean_attraction = mean(totals$attraction))
    },
    custom_predict = function(object, newdata, ...) {
      data.table::data.table(
        zone_id = newdata$zone_id,
        production = object$mean_production,
        attraction = object$mean_attraction
      )
    }
  )

  pred <- predict(fit, zones)

  expect_s3_class(pred, "data.table")
  expect_equal(pred$production, rep(round(mean(totals$production)), nrow(zones)))
  expect_equal(pred$attraction, rep(round(mean(totals$attraction)), nrow(zones)))
  fit_summary <- summary(fit)
  expect_identical(fit_summary$custom_class[[1L]], "list")
  expect_identical(
    fit_summary$custom_components,
    c("mean_production", "mean_attraction")
  )
  expect_identical(
    fit_summary$custom_predict_arguments,
    c("object", "newdata", "...")
  )
  expect_identical(fit_summary$custom_object, fit$custom_object)
  printed_summary <- paste(capture.output(print(fit_summary)), collapse = "\n")
  expect_match(printed_summary, "fitted components: mean_production, mean_attraction")
  expect_match(printed_summary, "\\$custom_object")
})

test_that("fsm_generation accepts an already calibrated custom object", {
  parameters <- list(
    production = c(intercept = 100, population = 0.04),
    attraction = c(intercept = 50, jobs = 0.06)
  )
  fit <- fsm_generation(
    method = "custom",
    custom_object = parameters,
    custom_predict = function(object, newdata, ...) {
      data.frame(
        zone_id = newdata$zone_id,
        production = object$production[["intercept"]] +
          object$production[["population"]] * newdata$population,
        attraction = object$attraction[["intercept"]] +
          object$attraction[["jobs"]] * newdata$jobs
      )
    }
  )

  pred <- predict(fit, fsm_toy_zone)

  expect_identical(fit$custom_object, parameters)
  expect_null(fit$id_col)
  expect_null(fit$training_data)
  expect_s3_class(pred, "data.table")
  expect_identical(names(pred), c("zone_id", "production", "attraction"))
  expect_error(predict(fit), "`newdata` must be supplied")
})

test_that("custom generation requires one model source", {
  predictor <- function(object, newdata, ...) {
    data.frame(
      zone_id = newdata$zone_id,
      production = 1,
      attraction = 1
    )
  }

  expect_error(
    fsm_generation(
      fsm_toy_zone,
      method = "custom",
      custom_predict = predictor
    ),
    "either `custom_object` or a `custom_fit`"
  )
  expect_error(
    fsm_generation(
      fsm_toy_zone,
      method = "custom",
      custom_fit = function(data, totals, ...) list(),
      custom_object = list(),
      custom_predict = predictor
    ),
    "only one of `custom_object` and `custom_fit`"
  )
  expect_error(
    fsm_generation(method = "custom", custom_object = list()),
    "`custom_predict` must be a function"
  )
})

test_that("negative generation predictions are set to zero with a warning", {
  fit <- fsm_generation(
    fsm_toy_zone,
    method = "custom",
    custom_fit = function(data, totals, ...) list(),
    custom_predict = function(object, newdata, ...) {
      data.table::data.table(
        zone_id = newdata$zone_id,
        production = rep(-1.2, nrow(newdata)),
        attraction = c(-2.4, rep(2.4, nrow(newdata) - 1L))
      )
    }
  )

  expect_warning(
    pred <- predict(fit),
    "production \\[zone_id 1 = -1.2.*attraction \\[zone_id 1 = -2.4"
  )
  expect_equal(pred$production, rep(0, nrow(fsm_toy_zone)))
  expect_equal(pred$attraction, c(0, rep(2, nrow(fsm_toy_zone) - 1L)))

  kept <- predict(fit, negative = "keep")
  expect_equal(kept$production, rep(-1, nrow(fsm_toy_zone)))
  expect_equal(kept$attraction, c(-2, rep(2, nrow(fsm_toy_zone) - 1L)))

  expect_error(
    predict(fit, negative = "error"),
    "Negative generation predictions found: production \\[zone_id 1 = -1.2"
  )
})

test_that("cross classification estimates exposure-based trip rates", {
  zones <- fsm_toy_zone
  totals <- fsm_od_totals(fsm_toy_od, zones)

  fit <- fsm_generation(
    zones,
    totals,
    method = "cross_classification",
    cross_classification = c("area_type", "income_group"),
    production_exposure = "households",
    attraction_exposure = "jobs"
  )
  pred <- predict(fit, zones)
  fit_summary <- summary(fit)

  expect_identical(fit_summary$classification, c("area_type", "income_group"))
  classification_cells <- unique(zones[, c("area_type", "income_group"), with = FALSE])
  expect_equal(fit_summary$n_cells, nrow(classification_cells))
  expect_identical(fit_summary$cell_rates, fit$cell_rates)
  printed_summary <- paste(capture.output(print(fit_summary)), collapse = "\n")
  expect_match(printed_summary, "\\$cell_rates")
  expect_match(printed_summary, "central")

  for (cell_index in seq_len(nrow(classification_cells))) {
    cell <- classification_cells[cell_index]
    ids <- zones[
      area_type == cell[["area_type"]] & income_group == cell[["income_group"]],
      zone_id
    ]
    cell_zones <- zones[zone_id %in% ids]
    production_rate <- sum(totals[zone_id %in% ids, production]) /
      sum(cell_zones$households)
    attraction_rate <- sum(totals[zone_id %in% ids, attraction]) /
      sum(cell_zones$jobs)
    expect_equal(
      pred[zone_id %in% ids, production],
      round(production_rate * cell_zones$households)
    )
    expect_equal(
      pred[zone_id %in% ids, attraction],
      round(attraction_rate * cell_zones$jobs)
    )
  }
})

test_that("growth factors preserve each identifier's base totals", {
  zones <- fsm_toy_zone
  totals <- fsm_od_totals(fsm_toy_od, zones)
  factors <- c(1.01, 1.02, 1.03, 1.04, 1.05, 1.06, 1.07)

  fit <- fsm_generation(
    zones,
    totals,
    method = "growth_factor",
    growth_factor = factors
  )
  reversed_zones <- zones[rev(seq_len(nrow(zones)))]
  pred <- predict(fit, reversed_zones)
  expected_factor <- factors[match(pred$zone_id, zones$zone_id)]
  expected_totals <- totals[match(pred$zone_id, totals$zone_id)]
  fit_summary <- summary(fit)

  expect_equal(pred$production, round(expected_totals$production * expected_factor))
  expect_equal(pred$attraction, round(expected_totals$attraction * expected_factor))
  expect_identical(fit_summary$factor_type, "by identifier")
  expect_equal(
    fit_summary$projected_totals[["production"]],
    sum(round(totals$production * factors))
  )
  expect_equal(
    fit_summary$projected_imbalance,
    fit_summary$projected_totals[["production"]] -
      fit_summary$projected_totals[["attraction"]]
  )
  expect_identical(
    fit_summary$projected_balanced,
    fit_summary$projected_imbalance == 0
  )
})

test_that("growth factors support optional data and totals", {
  totals <- fsm_od_totals(fsm_toy_od, fsm_toy_zone)

  reusable <- fsm_generation(
    method = "growth_factor",
    growth_factor = 1.10
  )
  reusable_pred <- predict(reusable, totals)
  reusable_summary <- summary(reusable)

  expect_null(reusable$id_col)
  expect_null(reusable$base_totals)
  expect_equal(reusable_pred$production, round(totals$production * 1.10))
  expect_equal(reusable_pred$attraction, round(totals$attraction * 1.10))
  expect_false(reusable_summary$has_base_totals)
  expect_error(predict(reusable), "`newdata` must be supplied")

  totals_only <- fsm_generation(
    totals = totals,
    method = "growth_factor",
    growth_factor = 1.10
  )
  expect_equal(predict(totals_only), reusable_pred)

  data_only <- fsm_generation(
    data = fsm_toy_zone,
    method = "growth_factor",
    growth_factor = 1.10
  )
  expect_equal(predict(data_only, totals), reusable_pred)

  expect_error(
    fsm_generation(method = "growth_factor", growth_factor = 1:2),
    "requires `data` or `totals`"
  )
})

test_that("generation regression supports population-level identifiers", {
  population_totals <- data.frame(
    population_id = fsm_toy_population$population_id,
    production = fsm_toy_population$population_count *
      (1 + fsm_toy_population$employed + fsm_toy_population$vehicles),
    attraction = fsm_toy_population$population_count *
      (2 + 2 * fsm_toy_population$employed + fsm_toy_population$vehicles)
  )

  fit <- fsm_generation(
    fsm_toy_population,
    population_totals,
    method = "regression",
    production_formula = production ~ 0 + population_count +
      population_count:employed + population_count:vehicles,
    attraction_formula = attraction ~ 0 + population_count +
      population_count:employed + population_count:vehicles
  )
  pred <- predict(fit, fsm_toy_population)

  expect_identical(fit$id_col, "population_id")
  expect_identical(summary(fit)$unit, "trip")
  expect_identical(names(pred), c("population_id", "production", "attraction"))
  expect_identical(data.table::key(pred), "population_id")
})

test_that("generation validates totals and custom prediction output", {
  expect_error(
    fsm_generation(
      fsm_toy_zone,
      data.frame(zone_id = 1:6, production = 1:6),
      method = "regression",
      production_formula = production ~ 0 + population,
      attraction_formula = attraction ~ 0 + jobs
    ),
    "attraction"
  )

  fit <- fsm_generation(
    fsm_toy_zone,
    method = "custom",
    custom_fit = function(data, totals, ...) list(),
    custom_predict = function(object, newdata, ...) {
      data.frame(zone_id = newdata$zone_id, production = 1)
    }
  )

  expect_error(predict(fit, fsm_toy_zone), "attraction")
})
