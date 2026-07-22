# Internal scalar string validator used by constructors.
fsm_is_single_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

# Resolve the observational unit used by generation models.
fsm_generation_id <- function(data, argument = "data") {
  if (inherits(data, "fsm_trip") && "trip_id" %in% names(data)) {
    id_columns <- "trip_id"
  } else if (inherits(data, "fsm_zone") && "zone_id" %in% names(data)) {
    id_columns <- "zone_id"
  } else {
    id_columns <- intersect(c("zone_id", "trip_id"), names(data))
  }

  if (length(id_columns) == 0L) {
    stop(
      "`", argument, "` must contain either `zone_id` or `trip_id`.",
      call. = FALSE
    )
  }

  if (length(id_columns) > 1L) {
    stop(
      "`", argument, "` cannot contain both `zone_id` and `trip_id`.",
      call. = FALSE
    )
  }

  id_col <- id_columns[[1L]]
  id <- data[[id_col]]

  if (anyNA(id)) {
    stop("`", id_col, "` cannot contain missing values.", call. = FALSE)
  }

  if (anyDuplicated(id)) {
    stop("`", id_col, "` must uniquely identify rows.", call. = FALSE)
  }

  id_col
}

fsm_generation_totals <- function(
  totals,
  id_col,
  outcomes = c("production", "attraction")
) {
  if (!is.data.frame(totals) && !inherits(totals, "data.table")) {
    stop("`totals` must be a data.frame or data.table.", call. = FALSE)
  }

  totals <- data.table::as.data.table(totals)
  required <- c(id_col, outcomes)
  missing_columns <- setdiff(required, names(totals))

  if (length(missing_columns) > 0L) {
    stop(
      "`totals` is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  fsm_generation_id(totals, "totals")

  for (column in outcomes) {
    values <- totals[[column]]
    if (!is.numeric(values)) {
      stop("`totals$", column, "` must be numeric.", call. = FALSE)
    }
    if (anyNA(values) || any(!is.finite(values))) {
      stop("`totals$", column, "` must contain only finite values.", call. = FALSE)
    }
    if (any(values < 0)) {
      stop("`totals$", column, "` cannot contain negative values.", call. = FALSE)
    }
  }

  totals[, required, with = FALSE]
}

fsm_generation_exposure <- function(data, column, argument) {
  if (is.null(column)) {
    return(rep(1, nrow(data)))
  }

  if (!fsm_is_single_string(column)) {
    stop("`", argument, "` must be NULL or a single character string.", call. = FALSE)
  }

  if (!column %in% names(data)) {
    stop("`", argument, "` column not found in `data`: ", column, ".", call. = FALSE)
  }

  values <- data[[column]]
  if (!is.numeric(values) || anyNA(values) || any(!is.finite(values))) {
    stop("`", argument, "` must identify a finite numeric column.", call. = FALSE)
  }

  if (any(values < 0)) {
    stop("`", argument, "` cannot contain negative values.", call. = FALSE)
  }

  values
}

fsm_generation_formula <- function(formula, response, available_columns) {
  argument <- paste0(response, "_formula")

  if (!inherits(formula, "formula")) {
    stop("`", argument, "` must be a formula.", call. = FALSE)
  }

  if (length(formula) != 3L || !identical(formula[[2L]], as.name(response))) {
    stop("`", argument, "` must use `", response, "` as its response.", call. = FALSE)
  }

  predictors <- all.vars(formula[[3L]])
  missing_predictors <- setdiff(predictors, available_columns)
  if (length(missing_predictors) > 0L) {
    stop(
      "`", argument, "` refers to missing column(s): ",
      paste(missing_predictors, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  predictors
}

fsm_validate_generation_output <- function(
  out,
  id_col,
  outcomes = c("production", "attraction"),
  negative = c("zero", "keep", "error")
) {
  negative <- match.arg(negative)

  if (!is.data.frame(out) && !inherits(out, "data.table")) {
    stop("Generation predictions must be a data.frame or data.table.", call. = FALSE)
  }

  out <- data.table::as.data.table(out)
  required <- c(id_col, outcomes)
  missing_columns <- setdiff(required, names(out))

  if (length(missing_columns) > 0L) {
    stop(
      "Generation predictions are missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  fsm_generation_id(out, "predictions")

  for (column in outcomes) {
    values <- out[[column]]
    if (!is.numeric(values)) {
      stop("Predicted `", column, "` must be numeric.", call. = FALSE)
    }
    if (any(is.infinite(values))) {
      stop("Predicted `", column, "` cannot contain infinite values.", call. = FALSE)
    }
    if (anyNA(values)) {
      values[is.na(values)] <- NA_real_
      data.table::set(out, j = column, value = values)
    }
  }

  missing_details <- fsm_missing_prediction_details(out, id_col, outcomes)
  if (length(missing_details)) {
    warning(
      "Missing generation predictions were retained as NA: ",
      paste(missing_details, collapse = "; "),
      ".",
      call. = FALSE
    )
  }

  negative_details <- fsm_negative_prediction_details(out, id_col, outcomes)
  if (length(negative_details) && negative == "error") {
    stop(
      "Negative generation predictions found: ",
      paste(negative_details, collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  if (length(negative_details) && negative == "zero") {
    for (column in outcomes) {
      values <- out[[column]]
      values[which(values < 0)] <- 0
      data.table::set(out, j = column, value = values)
    }
    warning(
      "Negative generation predictions were set to zero: ",
      paste(negative_details, collapse = "; "),
      ".",
      call. = FALSE
    )
  }

  for (column in outcomes) {
    data.table::set(out, j = column, value = round(out[[column]]))
  }

  out[, required, with = FALSE]
}

fsm_missing_prediction_details <- function(out, id_col, outcomes, max_items = 10L) {
  details <- character()
  for (column in outcomes) {
    missing_rows <- which(is.na(out[[column]]))
    if (!length(missing_rows)) {
      next
    }

    shown_rows <- utils::head(missing_rows, max_items)
    entries <- paste0(id_col, " ", out[[id_col]][shown_rows])
    if (length(missing_rows) > length(shown_rows)) {
      entries <- c(
        entries,
        paste0("... and ", length(missing_rows) - length(shown_rows), " more")
      )
    }
    details <- c(details, paste0(column, " [", paste(entries, collapse = ", "), "]"))
  }
  details
}

fsm_negative_prediction_details <- function(out, id_col, outcomes, max_items = 10L) {
  details <- character()
  for (column in outcomes) {
    negative_rows <- which(out[[column]] < 0)
    if (!length(negative_rows)) {
      next
    }

    shown_rows <- utils::head(negative_rows, max_items)
    entries <- paste0(
      id_col,
      " ",
      out[[id_col]][shown_rows],
      " = ",
      format(out[[column]][shown_rows], scientific = FALSE, trim = TRUE)
    )
    if (length(negative_rows) > length(shown_rows)) {
      entries <- c(
        entries,
        paste0("... and ", length(negative_rows) - length(shown_rows), " more")
      )
    }
    details <- c(details, paste0(column, " [", paste(entries, collapse = ", "), "]"))
  }
  details
}
