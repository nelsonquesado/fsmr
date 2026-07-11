#' Print an Origin-Destination Object
#'
#' @param x An \code{fsm_od} object.
#' @param ... Additional arguments passed to the underlying data.table print
#' method.
#' @param n Maximum number of OD pairs to display. Use \code{Inf} to display
#' every pair.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.fsm_od <- function(x, ..., n = 6L) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  shown <- fsm_print_n(n, nrow(x))

  cat("<fsm_od>\n")
  cat("  rows: ", fsm_format_number(nrow(x)), "\n", sep = "")
  cat("  total demand: ", fsm_format_number(sum(x[["demand"]], na.rm = TRUE)), "\n", sep = "")
  cat("  key: ", paste(data.table::key(x), collapse = ", "), "\n", sep = "")
  cat("\n")

  print(utils::head(data.table::as.data.table(x), shown), ...)
  invisible(x)
}

#' Summarize an Origin-Destination Object
#'
#' @param object An \code{fsm_od} object.
#' @param ... Unused.
#'
#' @return A list with core OD diagnostics.
#' @export
summary.fsm_od <- function(object, ...) {
  if (missing(object)) {
    stop("`object` must be supplied.", call. = FALSE)
  }

  out <- list(
    n_pairs = nrow(object),
    n_origins = length(unique(object[["origin"]])),
    n_destinations = length(unique(object[["destination"]])),
    total_demand = sum(object[["demand"]], na.rm = TRUE),
    missing_demand = sum(is.na(object[["demand"]])),
    zero_demand = sum(object[["demand"]] == 0, na.rm = TRUE),
    key = data.table::key(object)
  )

  class(out) <- "summary.fsm_od"
  out
}

#' @export
print.summary.fsm_od <- function(x, ...) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  cat("<summary.fsm_od>\n")
  cat("  OD pairs: ", x$n_pairs, "\n", sep = "")
  cat("  origins: ", x$n_origins, "\n", sep = "")
  cat("  destinations: ", x$n_destinations, "\n", sep = "")
  cat("  total demand: ", fsm_format_number(x$total_demand), "\n", sep = "")
  cat("  missing demand: ", x$missing_demand, "\n", sep = "")
  cat("  zero demand: ", x$zero_demand, "\n", sep = "")
  cat("  key: ", paste(x$key, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Summarize Production and Attraction Totals
#'
#' @description Summarizes an \code{fsm_od_totals} object with marginal trip
#' totals, balance diagnostics, zero-trip zones, and descriptive statistics for
#' productions and attractions.
#'
#' @param object An \code{fsm_od_totals} object.
#' @param ... Unused.
#'
#' @return An object of class \code{summary.fsm_od_totals}. Its
#' \code{statistics} element contains one row for production and one for
#' attraction.
#' @export
#'
#' @examples
#' totals <- fsm_od_totals(fsm_toy_od, fsm_toy_zone)
#' summary(totals)
summary.fsm_od_totals <- function(object, ...) {
  if (missing(object)) {
    stop("`object` must be supplied.", call. = FALSE)
  }
  if (!inherits(object, "fsm_od_totals")) {
    stop("`object` must be an `fsm_od_totals` object.", call. = FALSE)
  }

  margins <- c("production", "attraction")
  statistics <- do.call(rbind, lapply(margins, function(margin) {
    values <- object[[margin]]
    observed <- values[!is.na(values)]
    data.frame(
      margin = margin,
      total = sum(observed),
      mean = if (length(observed)) mean(observed) else NA_real_,
      sd = if (length(observed) > 1L) stats::sd(observed) else NA_real_,
      min = if (length(observed)) min(observed) else NA_real_,
      max = if (length(observed)) max(observed) else NA_real_,
      zero_zones = sum(values == 0, na.rm = TRUE),
      missing = sum(is.na(values)),
      row.names = NULL
    )
  }))

  total_production <- statistics$total[statistics$margin == "production"]
  total_attraction <- statistics$total[statistics$margin == "attraction"]
  imbalance <- total_production - total_attraction
  balance_scale <- max(1, abs(total_production), abs(total_attraction))

  out <- list(
    n_zones = nrow(object),
    total_production = total_production,
    total_attraction = total_attraction,
    imbalance = imbalance,
    balanced = abs(imbalance) <= sqrt(.Machine$double.eps) * balance_scale,
    statistics = statistics,
    key = data.table::key(object)
  )
  class(out) <- "summary.fsm_od_totals"
  out
}

#' Print a Production and Attraction Totals Summary
#'
#' @param x A \code{summary.fsm_od_totals} object.
#' @param ... Unused.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.summary.fsm_od_totals <- function(x, ...) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  cat("<summary.fsm_od_totals>\n")
  cat("  zones: ", fsm_format_number(x$n_zones), "\n", sep = "")
  cat("  total production: ", fsm_format_number(x$total_production), "\n", sep = "")
  cat("  total attraction: ", fsm_format_number(x$total_attraction), "\n", sep = "")
  cat("  imbalance (P - A): ", fsm_format_number(x$imbalance), "\n", sep = "")
  cat("  balanced: ", if (x$balanced) "yes" else "no", "\n", sep = "")
  cat("  key: ", paste(x$key, collapse = ", "), "\n", sep = "")
  cat("\nMargin statistics\n")
  print(x$statistics, row.names = FALSE, digits = 4)
  invisible(x)
}

fsm_format_number <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

fsm_print_n <- function(n, total) {
  valid_infinite <- is.numeric(n) && length(n) == 1L && identical(n, Inf)
  valid_finite <- is.numeric(n) && length(n) == 1L && !is.na(n) &&
    is.finite(n) && n >= 0 && n == floor(n)

  if (!valid_infinite && !valid_finite) {
    stop("`n` must be a non-negative whole number or Inf.", call. = FALSE)
  }

  if (valid_infinite) {
    return(total)
  }

  as.integer(min(n, total))
}

#' Print Production and Attraction Totals
#'
#' @param x An \code{fsm_od_totals} object.
#' @param ... Additional arguments passed to the underlying data.table print
#' method.
#' @param n Maximum number of rows to display. Use \code{Inf} to display every
#' row.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.fsm_od_totals <- function(x, ..., n = 6L) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }
  if (!inherits(x, "fsm_od_totals")) {
    stop("`x` must be an `fsm_od_totals` object.", call. = FALSE)
  }

  shown <- fsm_print_n(n, nrow(x))

  cat("<fsm_od_totals>\n")
  cat("  rows: ", fsm_format_number(nrow(x)), "\n", sep = "")
  cat("  key: ", paste(data.table::key(x), collapse = ", "), "\n\n", sep = "")

  print(utils::head(data.table::as.data.table(x), shown), ...)
  invisible(x)
}

#' Print a Population Attribute Object
#'
#' @param x An \code{fsm_population} object.
#' @param ... Additional arguments passed to the underlying data.table print
#' method.
#' @param n Maximum number of disaggregate records to display. Use \code{Inf} to
#' display every record.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.fsm_population <- function(x, ..., n = 6L) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  shown <- fsm_print_n(n, nrow(x))

  cat("<fsm_population>\n")
  cat("  rows: ", fsm_format_number(nrow(x)), "\n", sep = "")
  represented_label <- if (all(c("origin", "destination") %in% names(x))) {
    "represented trips"
  } else {
    "represented population"
  }
  cat(
    "  ", represented_label, ": ",
    fsm_format_number(sum(x[["population_count"]])),
    "\n",
    sep = ""
  )
  cat("  attributes: ", ncol(x) - 2L, "\n", sep = "")
  cat("  key: ", paste(data.table::key(x), collapse = ", "), "\n", sep = "")
  cat("\n")

  print(utils::head(data.table::as.data.table(x), shown), ...)
  invisible(x)
}

#' Print a Zone Attribute Object
#'
#' @param x An \code{fsm_zone} object.
#' @param ... Additional arguments passed to the underlying data.table print
#' method.
#' @param n Maximum number of zones to display. Use \code{Inf} to display every
#' zone.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.fsm_zone <- function(x, ..., n = 6L) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  shown <- fsm_print_n(n, nrow(x))

  cat("<fsm_zone>\n")
  cat("  rows: ", fsm_format_number(nrow(x)), "\n", sep = "")
  cat("  attributes: ", ncol(x) - 1L, "\n", sep = "")
  cat("  key: ", paste(data.table::key(x), collapse = ", "), "\n", sep = "")
  cat("\n")

  print(utils::head(data.table::as.data.table(x), shown), ...)
  invisible(x)
}

#' Summarize a Zone Attribute Object
#'
#' @description Summarizes the structure of an \code{fsm_zone} object and
#' reports the mean, standard deviation, minimum, maximum, and number of missing
#' values for each numeric attribute. The zone identifier is not treated as an
#' attribute.
#'
#' @param object An \code{fsm_zone} object.
#' @param ... Unused.
#'
#' @return An object of class \code{summary.fsm_zone}. Its \code{statistics}
#' element is a data.frame with one row per numeric attribute.
#' @export
summary.fsm_zone <- function(object, ...) {
  if (missing(object)) {
    stop("`object` must be supplied.", call. = FALSE)
  }
  if (!inherits(object, "fsm_zone")) {
    stop("`object` must be an `fsm_zone` object.", call. = FALSE)
  }

  attributes <- setdiff(names(object), "zone_id")
  numeric_attributes <- attributes[vapply(
    object[, attributes, with = FALSE],
    is.numeric,
    logical(1)
  )]
  statistics <- do.call(rbind, lapply(numeric_attributes, function(attribute) {
    values <- object[[attribute]]
    observed <- values[!is.na(values)]
    data.frame(
      attribute = attribute,
      mean = if (length(observed)) mean(observed) else NA_real_,
      sd = if (length(observed) > 1L) stats::sd(observed) else NA_real_,
      min = if (length(observed)) min(observed) else NA_real_,
      max = if (length(observed)) max(observed) else NA_real_,
      missing = sum(is.na(values)),
      row.names = NULL
    )
  }))
  if (is.null(statistics)) {
    statistics <- data.frame(
      attribute = character(),
      mean = numeric(),
      sd = numeric(),
      min = numeric(),
      max = numeric(),
      missing = integer()
    )
  }

  out <- list(
    n_zones = nrow(object),
    n_attributes = ncol(object) - 1L,
    attributes = attributes,
    numeric_attributes = numeric_attributes,
    statistics = statistics,
    key = data.table::key(object)
  )

  class(out) <- "summary.fsm_zone"
  out
}

#' @export
print.summary.fsm_zone <- function(x, ...) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  cat("<summary.fsm_zone>\n")
  cat("  zones: ", x$n_zones, "\n", sep = "")
  cat("  attributes: ", x$n_attributes, "\n", sep = "")
  cat("  names: ", paste(x$attributes, collapse = ", "), "\n", sep = "")
  cat("  key: ", paste(x$key, collapse = ", "), "\n", sep = "")
  cat("\nNumeric attributes\n")
  if (nrow(x$statistics) == 0L) {
    cat("  none\n")
  } else {
    print(x$statistics, row.names = FALSE, digits = 4)
  }
  invisible(x)
}

#' Print a Generation Model
#'
#' @param x An \code{fsm_generation} object.
#' @param ... Unused.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.fsm_generation <- function(x, ...) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }
  if (!inherits(x, "fsm_generation")) {
    stop("`x` must be an `fsm_generation` object.", call. = FALSE)
  }

  training_records <- fsm_generation_training_records(x)
  unit <- fsm_generation_unit(x$id_col, x$data_names)

  cat("<fsm_generation>\n")
  cat("  method: ", gsub("_", " ", x$method, fixed = TRUE), "\n", sep = "")
  if (is.null(x$id_col)) {
    cat("  observational unit: set at prediction time\n")
  } else {
    cat("  observational unit: ", unit, " (`", x$id_col, "`)\n", sep = "")
  }
  cat("  training records: ", fsm_format_number(training_records), "\n", sep = "")
  cat("  modeled outcomes: ", paste(x$outcomes, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Confidence Intervals for a Generation Regression
#'
#' @description Computes coefficient confidence intervals for each fitted
#' regression outcome in an \code{fsm_generation} object. The result is a
#' named list containing a matrix for production, attraction, or both. Methods
#' without fitted regression coefficients, including cross-classification,
#' growth-factor, and custom models, do not provide confidence intervals.
#'
#' @param object A regression-based \code{fsm_generation} object.
#' @param parm A specification of parameters for which confidence intervals are
#' requested. Passed to \code{stats::confint()}.
#' @param level Confidence level. Passed to \code{stats::confint()}.
#' @param ... Additional arguments passed to \code{stats::confint()}.
#'
#' @return A named list of confidence-interval matrices, one for each fitted
#' outcome.
#' @export
#'
#' @examples
#' totals <- fsm_od_totals(fsm_toy_od, fsm_toy_zone)
#' fit <- fsm_generation(
#'   fsm_toy_zone,
#'   totals,
#'   method = "regression",
#'   production_formula = production ~ 0 + population,
#'   attraction_formula = attraction ~ 0 + jobs
#' )
#' confint(fit)
confint.fsm_generation <- function(object, parm, level = 0.95, ...) {
  if (missing(object)) {
    stop("`object` must be supplied.", call. = FALSE)
  }
  if (!inherits(object, "fsm_generation")) {
    stop("`object` must be an `fsm_generation` object.", call. = FALSE)
  }
  if (!identical(object$method, "regression")) {
    stop(
      "Confidence intervals are available only for `method = \"regression\"`.",
      call. = FALSE
    )
  }

  fits <- stats::setNames(lapply(object$outcomes, function(outcome) {
    object[[paste0(outcome, "_fit")]]
  }), object$outcomes)

  if (missing(parm)) {
    lapply(fits, stats::confint, level = level, ...)
  } else {
    lapply(fits, stats::confint, parm = parm, level = level, ...)
  }
}

fsm_generation_training_records <- function(object) {
  if (is.null(object$training_data)) 0L else nrow(object$training_data)
}

fsm_generation_unit <- function(id_col, data_names = character()) {
  if (is.null(id_col)) {
    return("set at prediction time")
  }
  if (identical(id_col, "zone_id")) {
    return("zone")
  }
  if (all(c("origin", "destination") %in% data_names)) "trip" else "population record"
}

#' Summarize a Generation Model
#'
#' @description Summarizes a fitted \code{fsm_generation} object using
#' diagnostics appropriate to its fitting method. Regression summaries report
#' model fit statistics separately for production and attraction. Other
#' methods report their defining rates, factors, or custom fitted object.
#'
#' @param object An \code{fsm_generation} object.
#' @param ... Unused.
#'
#' @return An object of class \code{summary.fsm_generation} containing common
#' model metadata and method-specific diagnostics.
#' @export
#'
#' @examples
#' totals <- fsm_od_totals(fsm_toy_od, fsm_toy_zone)
#' fit <- fsm_generation(
#'   fsm_toy_zone,
#'   totals,
#'   method = "regression",
#'   production_formula = production ~ 0 + population,
#'   attraction_formula = attraction ~ 0 + jobs
#' )
#' summary(fit)
summary.fsm_generation <- function(object, ...) {
  if (missing(object)) {
    stop("`object` must be supplied.", call. = FALSE)
  }
  if (!inherits(object, "fsm_generation")) {
    stop("`object` must be an `fsm_generation` object.", call. = FALSE)
  }

  out <- list(
    method = object$method,
    outcomes = object$outcomes,
    id_col = object$id_col,
    unit = fsm_generation_unit(object$id_col, object$data_names),
    training_records = fsm_generation_training_records(object)
  )

  if (object$method == "regression") {
    out$models <- stats::setNames(lapply(object$outcomes, function(outcome) {
      fit <- object[[paste0(outcome, "_fit")]]
      fit_summary <- summary(fit)
      list(
        formula = stats::formula(fit),
        n_observations = stats::nobs(fit),
        n_coefficients = sum(!is.na(stats::coef(fit))),
        rank_deficient = anyNA(stats::coef(fit)),
        coefficients = fit_summary$coefficients,
        r_squared = unname(fit_summary$r.squared),
        adjusted_r_squared = unname(fit_summary$adj.r.squared),
        residual_standard_error = unname(fit_summary$sigma)
      )
    }), object$outcomes)
  } else if (object$method == "cross_classification") {
    out$classification <- object$cross_classification
    out$n_cells <- nrow(object$cell_rates)
    out$cell_rates <- data.table::copy(object$cell_rates)
    out$production_exposure <- object$production_exposure
    out$attraction_exposure <- object$attraction_exposure
  } else if (object$method == "growth_factor") {
    factors <- if (!is.null(object$growth_factor)) {
      object$growth_factor
    } else {
      object$factor_map[["growth_factor"]]
    }
    out$factor_type <- if (is.null(object$factor_map)) "uniform" else "by identifier"
    out$factor_range <- range(factors)
    out$has_base_totals <- !is.null(object$base_totals)
    if (out$has_base_totals) {
      base_factors <- if (!is.null(object$growth_factor)) {
        rep(object$growth_factor, nrow(object$base_totals))
      } else {
        factor_index <- match(
          object$base_totals[[object$id_col]],
          object$factor_map[[object$id_col]]
        )
        object$factor_map[["growth_factor"]][factor_index]
      }
      out$base_totals <- c(
        production = sum(object$base_totals[["production"]]),
        attraction = sum(object$base_totals[["attraction"]])
      )
      out$projected_totals <- c(
        production = sum(round(object$base_totals[["production"]] * base_factors)),
        attraction = sum(round(object$base_totals[["attraction"]] * base_factors))
      )
      out$projected_imbalance <-
        out$projected_totals[["production"]] - out$projected_totals[["attraction"]]
      balance_scale <- max(1, abs(out$projected_totals))
      out$projected_balanced <-
        abs(out$projected_imbalance) <= sqrt(.Machine$double.eps) * balance_scale
    }
  } else {
    custom_class <- class(object$custom_object)
    out$custom_class <- if (length(custom_class)) custom_class else typeof(object$custom_object)
    out$custom_components <- names(object$custom_object)
    out$custom_predict_arguments <- names(formals(object$custom_predict))
    out$custom_object <- object$custom_object
  }

  class(out) <- "summary.fsm_generation"
  out
}

#' Print a Generation Model Summary
#'
#' @param x A \code{summary.fsm_generation} object.
#' @param ... Additional arguments passed to the data.table print method for
#' cross-classification cell rates.
#' @param n Maximum number of cross-classification cells to display. Use
#' \code{Inf} to display every cell.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.summary.fsm_generation <- function(x, ..., n = 10L) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  cat("<summary.fsm_generation>\n")
  cat("  method: ", gsub("_", " ", x$method, fixed = TRUE), "\n", sep = "")
  if (is.null(x$id_col)) {
    cat("  observational unit: set at prediction time\n")
  } else {
    cat("  observational unit: ", x$unit, " (`", x$id_col, "`)\n", sep = "")
  }
  cat("  training records: ", fsm_format_number(x$training_records), "\n", sep = "")
  cat("  modeled outcomes: ", paste(x$outcomes, collapse = ", "), "\n", sep = "")

  if (x$method == "regression") {
    for (outcome in x$outcomes) {
      model <- x$models[[outcome]]
      cat("\n", fsm_title_case(outcome), " model\n", sep = "")
      cat("  formula: ", paste(deparse(model$formula), collapse = " "), "\n", sep = "")
      cat("  observations: ", fsm_format_number(model$n_observations), "\n", sep = "")
      cat("  estimated coefficients: ", fsm_format_number(model$n_coefficients), "\n", sep = "")
      cat("  rank deficient: ", if (model$rank_deficient) "yes" else "no", "\n", sep = "")
      cat("  coefficient table:\n")
      stats::printCoefmat(model$coefficients, digits = 4L, signif.stars = FALSE)
      cat("  R-squared: ", fsm_format_metric(model$r_squared), "\n", sep = "")
      cat("  adjusted R-squared: ", fsm_format_metric(model$adjusted_r_squared), "\n", sep = "")
      cat(
        "  residual standard error: ",
        fsm_format_metric(model$residual_standard_error),
        "\n",
        sep = ""
      )
    }
  } else if (x$method == "cross_classification") {
    cat("\nClassification details\n")
    cat("  classification variables: ", paste(x$classification, collapse = ", "), "\n", sep = "")
    cat("  fitted cells: ", fsm_format_number(x$n_cells), "\n", sep = "")
    production_exposure <- if (is.null(x$production_exposure)) {
      "one unit per row"
    } else {
      x$production_exposure
    }
    attraction_exposure <- if (is.null(x$attraction_exposure)) {
      "one unit per row"
    } else {
      x$attraction_exposure
    }
    cat("  production exposure: ", production_exposure, "\n", sep = "")
    cat("  attraction exposure: ", attraction_exposure, "\n", sep = "")
    shown <- fsm_print_n(n, nrow(x$cell_rates))
    cat("\n$cell_rates\n")
    cat("  rows: ", fsm_format_number(nrow(x$cell_rates)), "\n", sep = "")
    cat("\n")
    print(utils::head(x$cell_rates, shown), ...)
  } else if (x$method == "growth_factor") {
    cat("\nGrowth details\n")
    cat("  factor type: ", x$factor_type, "\n", sep = "")
    cat("  factor range: ", fsm_format_range(x$factor_range), "\n", sep = "")
    if (!x$has_base_totals) {
      cat("  base totals: supplied at prediction time\n")
    } else {
      cat("  base production: ", fsm_format_number(x$base_totals[["production"]]), "\n", sep = "")
      cat("  projected production: ", fsm_format_number(x$projected_totals[["production"]]), "\n", sep = "")
      cat("  base attraction: ", fsm_format_number(x$base_totals[["attraction"]]), "\n", sep = "")
      cat("  projected attraction: ", fsm_format_number(x$projected_totals[["attraction"]]), "\n", sep = "")
      cat("  imbalance (P - A): ", fsm_format_number(x$projected_imbalance), "\n", sep = "")
      cat("  balanced: ", if (x$projected_balanced) "yes" else "no", "\n", sep = "")
    }
  } else {
    cat("\nCustom model\n")
    cat("  fitted object class: ", paste(x$custom_class, collapse = ", "), "\n", sep = "")
    if (length(x$custom_components)) {
      cat("  fitted components: ", paste(x$custom_components, collapse = ", "), "\n", sep = "")
    } else {
      cat("  fitted components: unnamed\n")
    }
    cat(
      "  prediction arguments: ",
      paste(x$custom_predict_arguments, collapse = ", "),
      "\n",
      sep = ""
    )
    cat("\n$custom_object\n")
    utils::str(
      x$custom_object,
      max.level = 1L,
      list.len = 10L,
      give.attr = FALSE
    )
  }

  invisible(x)
}

fsm_format_metric <- function(x, digits = 4L) {
  if (length(x) == 0L || is.na(x)) {
    return("NA")
  }
  format(signif(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fsm_format_range <- function(x) {
  paste(vapply(x, fsm_format_metric, character(1)), collapse = " to ")
}

fsm_title_case <- function(x) {
  paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
}
