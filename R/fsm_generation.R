#' Production and Attraction Totals from an OD Object
#'
#' @description Computes observed productions and attractions from an
#' \code{fsm_od} object. This is the minimum observed trip-generation summary:
#' productions are row totals by origin and attractions are column totals by
#' destination. Missing demand observations are excluded from both sums. In
#' fsmr, generation starts from an OD table; zone-level attributes are optional
#' and only used when you want to return a complete zone list or validate
#' coverage.
#'
#' @param od An \code{fsm_od} object.
#' @param zones Optional \code{fsm_zone} object used to return one row for every
#' zone and validate OD coverage.
#'
#' @return A keyed \code{data.table} with \code{zone_id}, \code{production}, and
#' \code{attraction}.
#' @export
#' @importFrom data.table setkeyv setnames
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See the treatment of demand flows and aggregate
#' travel-demand models.
#'
#' @examples
#' od <- fsm_toy_od
#' fsm_od_totals(od)
fsm_od_totals <- function(od, zones = NULL) {
  if (missing(od)) {
    stop("`od` must be supplied.", call. = FALSE)
  }

  if (!inherits(od, "fsm_od")) {
    stop("`od` must be an `fsm_od` object.", call. = FALSE)
  }

  productions <- od[, list(production = sum(get("demand"), na.rm = TRUE)), by = "origin"]
  data.table::setnames(productions, "origin", "zone_id")

  attractions <- od[, list(attraction = sum(get("demand"), na.rm = TRUE)), by = "destination"]
  data.table::setnames(attractions, "destination", "zone_id")

  if (is.null(zones)) {
    out <- merge(productions, attractions, by = "zone_id", all = TRUE)
  } else {
    if (!inherits(zones, "fsm_zone")) {
      stop("`zones` must be an `fsm_zone` object.", call. = FALSE)
    }

    fsm_check_zone_od(zones, od)
    out <- merge(
      data.table::as.data.table(zones)[, "zone_id"],
      productions,
      by = "zone_id",
      all.x = TRUE
    )
    out <- merge(out, attractions, by = "zone_id", all.x = TRUE)
  }

  data.table::set(out, which(is.na(out[["production"]])), "production", 0)
  data.table::set(out, which(is.na(out[["attraction"]])), "attraction", 0)
  data.table::setkeyv(out, "zone_id")
  class(out) <- c("fsm_od_totals", class(out))

  out
}

#' Generation Model Fitting and Prediction
#'
#' @description Fits a trip-generation model that can be applied to new zonal
#' or trip-level data. Built-in methods include regression,
#' cross-classification rates, and growth factors. A custom fitting function or
#' an already fitted object can also be supplied for full control over
#' estimation and prediction. The current implementation is trip-based and
#' does not jointly model decisions across trip chains or activity schedules.
#' When supplied, input data must be uniquely
#' identified by either \code{zone_id} or \code{trip_id}.
#' For trip-level data, \code{trip_id} remains the modeling identifier.
#' Trip-level predictions can be aggregated into zonal productions and
#' attractions with their origin and destination columns.
#' For regression, production and attraction models are optional and fitted
#' independently, but at least one formula must be supplied. Regression
#' predictions are rounded to whole trips. Negative predictions are replaced
#' with zero and produce a warning. If required predictors are missing for a
#' record, its affected prediction is retained as \code{NA} with a warning.
#'
#' @param data Optional \code{fsm_zone}, \code{fsm_trip}, or
#' data.frame-like table containing explanatory variables. It may be omitted
#' for \code{method = "growth_factor"} or when an already calibrated
#' \code{custom_object} is supplied.
#' @param totals An optional \code{fsm_od_totals} object or data.frame with the
#' same identifier as \code{data}, plus the observed outcome columns required
#' by the selected method. A one-sided regression needs only its modeled
#' outcome. For a reusable growth-factor model, totals may be omitted and later
#' supplied as prediction data.
#' @param method Character string. One of \code{"regression"},
#' \code{"cross_classification"}, \code{"growth_factor"}, or \code{"custom"}.
#' @param production_formula A formula for the production model, or \code{NULL}
#' to skip production. Used when \code{method = "regression"}.
#' @param attraction_formula A formula for the attraction model, or \code{NULL}
#' to skip attraction. Used when \code{method = "regression"}. At least one of
#' \code{production_formula} and \code{attraction_formula} must be supplied.
#' @param cross_classification Character vector of column names used to form
#' cross-classification cells.
#' @param production_exposure Optional name of a non-negative numeric column
#' giving the number of production units represented by each row, such as
#' households. If \code{NULL}, each row represents one unit.
#' @param attraction_exposure Optional name of a non-negative numeric column
#' giving the number of attraction units represented by each row, such as jobs.
#' If \code{NULL}, each row represents one unit.
#' @param growth_factor Numeric scalar or numeric vector used to scale observed
#' totals when \code{method = "growth_factor"}. A vector must have one value
#' per identifier supplied through \code{data} or \code{totals}; factors are
#' associated with identifiers, not row positions during later prediction. A
#' scalar can be stored without \code{data} or \code{totals} and later applied
#' to prediction data containing production and attraction columns.
#' @param custom_fit Optional fitting function used when
#' \code{method = "custom"}. It must accept \code{data}, \code{totals}, and
#' \code{...}, and return any object understood by \code{custom_predict}. Supply
#' either \code{custom_fit} or \code{custom_object}, but not both.
#' @param custom_object Optional already fitted model or parameter object used
#' when \code{method = "custom"}. This allows externally calibrated parameters
#' to be stored directly without a dummy fitting function. Supply either
#' \code{custom_object} or \code{custom_fit}, but not both.
#' @param custom_predict Optional function used when \code{method = "custom"}.
#' It must accept \code{object}, \code{newdata}, and \code{...}, and return a
#' table with the appropriate identifier, \code{production}, and
#' \code{attraction}.
#' @param ... Additional arguments passed to the selected fitting or prediction
#' function.
#'
#' @return An object of class \code{fsm_generation}. It can be passed to
#' \code{predict()} with new zonal or trip-level data. Regression
#' predictions contain only the outcomes whose formulas were supplied.
#' @export
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See Sect. 4.3.1.1 for classification-table and
#' trip-rate regression models.
#'
#' @examples
#' zones <- fsm_toy_zone
#' od <- fsm_toy_od
#' totals <- fsm_od_totals(od, zones)
#'
#' fit <- fsm_generation(zones, totals, method = "regression",
#'   production_formula = production ~ 0 + population,
#'   attraction_formula = attraction ~ 0 + jobs)
#' predict(fit, zones)
#'
#' fit_cc <- fsm_generation(
#'   zones,
#'   totals,
#'   method = "cross_classification",
#'   cross_classification = c("area_type", "income_group"),
#'   production_exposure = "households",
#'   attraction_exposure = "jobs"
#' )
#' predict(fit_cc, zones)
#'
#' fit_gf <- fsm_generation(
#'   zones,
#'   totals,
#'   method = "growth_factor",
#'   growth_factor = 1.10
#' )
#' predict(fit_gf, zones)
#'
#' fit_gf_reusable <- fsm_generation(
#'   method = "growth_factor",
#'   growth_factor = 1.10
#' )
#' predict(fit_gf_reusable, totals)
#'
#' fit_gf_totals <- fsm_generation(
#'   totals = totals,
#'   method = "growth_factor",
#'   growth_factor = 1.10
#' )
#' predict(fit_gf_totals)
#'
#' fit_custom <- fsm_generation(
#'   zones,
#'   totals,
#'   method = "custom",
#'   custom_fit = function(data, totals, ...) list(mean = mean(totals$production)),
#'   custom_predict = function(object, newdata, ...) {
#'     data.table::data.table(
#'       zone_id = newdata$zone_id,
#'       production = object$mean,
#'       attraction = object$mean
#'     )
#'   }
#' )
#' predict(fit_custom, zones)
#'
#' # Apply externally calibrated parameters without observed totals.
#' parameters <- list(production = c(intercept = 1, jobs = 0.04))
#' fit_external <- fsm_generation(
#'   method = "custom",
#'   custom_object = parameters,
#'   custom_predict = function(object, newdata, ...) {
#'     data.frame(
#'       zone_id = newdata$zone_id,
#'       production = object$production[["intercept"]] +
#'         object$production[["jobs"]] * newdata$jobs,
#'       attraction = object$production[["intercept"]] +
#'         object$production[["jobs"]] * newdata$jobs
#'     )
#'   }
#' )
#' predict(fit_external, zones)
fsm_generation <- function(
  data = NULL,
  totals = NULL,
  method = c("regression", "cross_classification", "growth_factor", "custom"),
  production_formula = NULL,
  attraction_formula = NULL,
  cross_classification = NULL,
  production_exposure = NULL,
  attraction_exposure = NULL,
  growth_factor = NULL,
  custom_fit = NULL,
  custom_object = NULL,
  custom_predict = NULL,
  ...
) {
  custom_object_supplied <- !missing(custom_object)

  if (!is.null(data) && !is.data.frame(data) && !is.list(data)) {
    stop("`data` must be a data.frame, data.table, or list.", call. = FALSE)
  }

  method <- match.arg(method)

  outcomes <- c("production", "attraction")
  if (method == "regression") {
    outcomes <- c(
      if (!is.null(production_formula)) "production",
      if (!is.null(attraction_formula)) "attraction"
    )
    if (length(outcomes) == 0L) {
      stop(
        "At least one of `production_formula` and `attraction_formula` must be supplied for regression.",
        call. = FALSE
      )
    }
  }

  direct_custom <- method == "custom" && custom_object_supplied
  if (is.null(data) && method != "growth_factor" && !direct_custom) {
    stop("`data` must be supplied for method `", method, "`.", call. = FALSE)
  }

  if (!is.null(data) && !inherits(data, "data.table")) {
    data <- data.table::as.data.table(data)
  }
  training_data <- if (is.null(data)) NULL else data.table::copy(data)

  id_col <- if (is.null(data)) NULL else fsm_generation_id(data)

  if (!is.null(totals)) {
    if (!is.data.frame(totals) && !inherits(totals, "data.table")) {
      stop("`totals` must be a data.frame or data.table.", call. = FALSE)
    }
    if (is.null(id_col)) {
      id_col <- fsm_generation_id(data.table::as.data.table(totals), "totals")
    }
    totals <- fsm_generation_totals(totals, id_col, outcomes)
    missing_totals <- if (is.null(data)) NULL else {
      setdiff(data[[id_col]], totals[[id_col]])
    }
    if (length(missing_totals)) {
      stop(
        "`totals` does not cover every row in `data`. Missing identifier(s): ",
        paste(utils::head(missing_totals, 10L), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  if (method == "regression") {
    if (is.null(totals)) {
      stop("`totals` must be supplied for regression.", call. = FALSE)
    }

    model_data <- merge(data, totals, by = id_col, all.x = TRUE, sort = FALSE)
    production_predictors <- attraction_predictors <- character()
    production_fit <- attraction_fit <- NULL

    if (!is.null(production_formula)) {
      production_predictors <- fsm_generation_formula(
        production_formula,
        "production",
        names(model_data)
      )
      production_fit <- stats::lm(production_formula, data = model_data, ...)
    }
    if (!is.null(attraction_formula)) {
      attraction_predictors <- fsm_generation_formula(
        attraction_formula,
        "attraction",
        names(model_data)
      )
      attraction_fit <- stats::lm(attraction_formula, data = model_data, ...)
    }

    structure(
      list(
        method = method,
        outcomes = outcomes,
        production_fit = production_fit,
        attraction_fit = attraction_fit,
        id_col = id_col,
        training_data = training_data,
        predictors = unique(c(production_predictors, attraction_predictors)),
        data_names = names(data),
        terms = list(
          production = production_formula,
          attraction = attraction_formula
        )
      ),
      class = "fsm_generation"
    )
  } else if (method == "cross_classification") {
    if (is.null(totals)) {
      stop("`totals` must be supplied for cross classification.", call. = FALSE)
    }

    if (!is.character(cross_classification) || !length(cross_classification) ||
        anyNA(cross_classification) || any(cross_classification == "")) {
      stop("`cross_classification` must name at least one column.", call. = FALSE)
    }

    cross_classification <- unique(cross_classification)
    missing_classes <- setdiff(cross_classification, names(data))
    if (length(missing_classes) > 0L) {
      stop(
        "Cross-classification column(s) not found in `data`: ",
        paste(missing_classes, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    if (any(vapply(data[, cross_classification, with = FALSE], anyNA, logical(1)))) {
      stop("Cross-classification columns cannot contain missing values.", call. = FALSE)
    }

    cells <- merge(
      data,
      totals,
      by = id_col,
      all.x = TRUE,
      sort = FALSE
    )
    data.table::set(
      cells,
      j = ".fsm_production_exposure",
      value = fsm_generation_exposure(
        cells,
        production_exposure,
        "production_exposure"
      )
    )
    data.table::set(
      cells,
      j = ".fsm_attraction_exposure",
      value = fsm_generation_exposure(
        cells,
        attraction_exposure,
        "attraction_exposure"
      )
    )

    cell_rates <- cells[, list(
      production_total = sum(production),
      attraction_total = sum(attraction),
      production_exposure = sum(.fsm_production_exposure),
      attraction_exposure = sum(.fsm_attraction_exposure)
    ), by = cross_classification]

    if (any(cell_rates[["production_exposure"]] <= 0)) {
      stop("Every cross-classification cell must have positive production exposure.", call. = FALSE)
    }
    if (any(cell_rates[["attraction_exposure"]] <= 0)) {
      stop("Every cross-classification cell must have positive attraction exposure.", call. = FALSE)
    }

    data.table::set(
      cell_rates,
      j = "production_rate",
      value = cell_rates[["production_total"]] / cell_rates[["production_exposure"]]
    )
    data.table::set(
      cell_rates,
      j = "attraction_rate",
      value = cell_rates[["attraction_total"]] / cell_rates[["attraction_exposure"]]
    )
    cell_rates <- cell_rates[, c(
      cross_classification,
      "production_rate",
      "attraction_rate"
    ), with = FALSE]

    structure(
      list(
        method = method,
        outcomes = outcomes,
        id_col = id_col,
        training_data = training_data,
        cell_rates = cell_rates,
        cross_classification = cross_classification,
        production_exposure = production_exposure,
        attraction_exposure = attraction_exposure,
        data_names = names(data)
      ),
      class = "fsm_generation"
    )
  } else if (method == "growth_factor") {
    if (is.null(growth_factor)) {
      growth_factor <- 1
    }

    if (!is.numeric(growth_factor) || anyNA(growth_factor) ||
        any(!is.finite(growth_factor)) || any(growth_factor < 0)) {
      stop("`growth_factor` must contain finite, non-negative numeric values.", call. = FALSE)
    }

    reference_ids <- if (!is.null(data)) {
      data[[id_col]]
    } else if (!is.null(totals)) {
      totals[[id_col]]
    } else {
      NULL
    }
    if (length(growth_factor) > 1L && is.null(reference_ids)) {
      stop(
        "A vector `growth_factor` requires `data` or `totals` to identify its values.",
        call. = FALSE
      )
    }
    if (length(growth_factor) > 1L && length(growth_factor) != length(reference_ids)) {
      stop(
        "A vector `growth_factor` must match the number of identifiers in `data` or `totals`.",
        call. = FALSE
      )
    }

    factor_map <- NULL
    if (length(growth_factor) > 1L) {
      factor_map <- data.table::data.table(
        id = reference_ids,
        growth_factor = unname(growth_factor)
      )
      data.table::setnames(factor_map, "id", id_col)
    }
    if (is.null(training_data) && !is.null(totals)) {
      training_data <- totals[, id_col, with = FALSE]
    }

    structure(
      list(
        method = method,
        outcomes = outcomes,
        id_col = id_col,
        training_data = training_data,
        growth_factor = if (length(growth_factor) == 1L) growth_factor else NULL,
        factor_map = factor_map,
        base_totals = totals,
        data_names = if (is.null(data)) character() else names(data)
      ),
      class = "fsm_generation"
    )
  } else {
    if (!is.function(custom_predict)) {
      stop("`custom_predict` must be a function.", call. = FALSE)
    }
    if (custom_object_supplied && !is.null(custom_fit)) {
      stop("Supply only one of `custom_object` and `custom_fit`.", call. = FALSE)
    }
    if (!custom_object_supplied && !is.function(custom_fit)) {
      stop("Supply either `custom_object` or a `custom_fit` function.", call. = FALSE)
    }

    object <- if (custom_object_supplied) {
      custom_object
    } else {
      custom_fit(data = data, totals = totals, ...)
    }
    structure(
      list(
        method = method,
        outcomes = outcomes,
        id_col = id_col,
        training_data = training_data,
        custom_object = object,
        custom_predict = custom_predict,
        data_names = if (is.null(data)) character() else names(data)
      ),
      class = "fsm_generation"
    )
  }
}

#' Predict from a Generation Model
#'
#' @param object An \code{fsm_generation} object.
#' @param newdata Optional zonal or trip-level data used for prediction.
#' It must use the same identifier type as the fitted model. If omitted, the
#' training data stored in \code{object} are used.
#' @param negative Character string controlling negative predictions:
#' \code{"zero"} replaces them with zero and warns, \code{"keep"} retains
#' them, and \code{"error"} stops prediction. Messages identify the affected
#' outcome, record identifier, and original value.
#' @param ... Additional arguments passed to the underlying prediction method.
#'
#' @return A keyed \code{data.table} with \code{zone_id} or
#' \code{trip_id}, plus the modeled outcome columns. Regression returns
#' only the outcomes whose formulas were supplied. All observed predicted
#' outcomes are rounded to whole trips. Predictions that cannot be calculated
#' because of missing inputs are retained as \code{NA} with a warning. By
#' default, negative predictions are replaced with zero and produce a warning.
#' @export
#' @importFrom stats predict
predict.fsm_generation <- function(
  object,
  newdata = NULL,
  negative = c("zero", "keep", "error"),
  ...
) {
  if (missing(object)) {
    stop("`object` must be supplied.", call. = FALSE)
  }

  if (!inherits(object, "fsm_generation")) {
    stop("`object` must be an `fsm_generation` object.", call. = FALSE)
  }
  negative <- match.arg(negative)

  if (is.null(newdata)) {
    newdata <- object$training_data
    if (is.null(newdata)) {
      stop(
        "`newdata` must be supplied because the fitted object has no stored training data.",
        call. = FALSE
      )
    }
  }

  if (!inherits(newdata, "data.table")) {
    newdata <- data.table::as.data.table(newdata)
  }

  new_id_col <- fsm_generation_id(newdata, "newdata")
  id_col <- if (is.null(object$id_col)) new_id_col else object$id_col
  if (!is.null(object$id_col) && !identical(new_id_col, id_col)) {
    stop("`newdata` must contain `", id_col, "` as its identifier.", call. = FALSE)
  }

  if (object$method == "regression") {
    missing_predictors <- setdiff(object$predictors, names(newdata))
    if (length(missing_predictors) > 0L) {
      stop(
        "`newdata` is missing predictor column(s): ",
        paste(missing_predictors, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    out <- data.table::data.table(id = newdata[[id_col]])
    data.table::setnames(out, "id", id_col)
    if ("production" %in% object$outcomes) {
      data.table::set(
        out,
        j = "production",
        value = stats::predict(object$production_fit, newdata = newdata, ...)
      )
    }
    if ("attraction" %in% object$outcomes) {
      data.table::set(
        out,
        j = "attraction",
        value = stats::predict(object$attraction_fit, newdata = newdata, ...)
      )
    }
  } else if (object$method == "cross_classification") {
    if (!all(object$cross_classification %in% names(newdata))) {
      stop("`newdata` must contain all cross-classification columns.", call. = FALSE)
    }

    out <- merge(
      newdata,
      object$cell_rates,
      by = object$cross_classification,
      all.x = TRUE,
      sort = FALSE
    )

    unmatched <- is.na(out[["production_rate"]]) | is.na(out[["attraction_rate"]])
    if (any(unmatched)) {
      unmatched_ids <- out[[id_col]][unmatched]
      stop(
        "No fitted cross-classification rate for identifier(s): ",
        paste(utils::head(unmatched_ids, 10L), collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    production_units <- fsm_generation_exposure(
      out,
      object$production_exposure,
      "production_exposure"
    )
    attraction_units <- fsm_generation_exposure(
      out,
      object$attraction_exposure,
      "attraction_exposure"
    )
    data.table::set(
      out,
      j = "production",
      value = out[["production_rate"]] * production_units
    )
    data.table::set(
      out,
      j = "attraction",
      value = out[["attraction_rate"]] * attraction_units
    )
    out_order <- match(out[[id_col]], newdata[[id_col]])
    out <- out[order(out_order), c(id_col, "production", "attraction"), with = FALSE]
  } else if (object$method == "growth_factor") {
    if (is.null(object$base_totals)) {
      base <- fsm_generation_totals(newdata, id_col, object$outcomes)
      base_index <- match(newdata[[id_col]], base[[id_col]])
      base <- base[base_index]
    } else {
      base_index <- match(newdata[[id_col]], object$base_totals[[id_col]])
      if (anyNA(base_index)) {
        missing_ids <- newdata[[id_col]][is.na(base_index)]
        stop(
          "No base totals are available for identifier(s): ",
          paste(utils::head(missing_ids, 10L), collapse = ", "),
          ".",
          call. = FALSE
        )
      }
      base <- object$base_totals[base_index]
    }

    if (!is.null(object$growth_factor)) {
      factor <- rep(object$growth_factor, nrow(newdata))
    } else {
      factor_index <- match(newdata[[id_col]], object$factor_map[[id_col]])
      if (anyNA(factor_index)) {
        stop("No growth factor is available for every row in `newdata`.", call. = FALSE)
      }
      factor <- object$factor_map[["growth_factor"]][factor_index]
    }

    out <- data.table::data.table(
      id = newdata[[id_col]],
      production = base[["production"]] * factor,
      attraction = base[["attraction"]] * factor
    )
    data.table::setnames(out, "id", id_col)
  } else {
    out <- object$custom_predict(object$custom_object, newdata = newdata, ...)
  }

  out <- fsm_validate_generation_output(
    out,
    id_col,
    object$outcomes,
    negative = negative
  )
  missing_predictions <- setdiff(newdata[[id_col]], out[[id_col]])
  extra_predictions <- setdiff(out[[id_col]], newdata[[id_col]])
  if (length(missing_predictions) > 0L || length(extra_predictions) > 0L) {
    stop(
      "Generation predictions must contain exactly the identifiers in `newdata`.",
      call. = FALSE
    )
  }
  data.table::setkeyv(out, id_col)
  out
}
