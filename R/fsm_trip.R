#' Origin-Destination Survey Constructor
#'
#' @description Converts an origin-destination survey table into a keyed
#' \code{fsm_trip} object. Each row is one observed trip and is uniquely
#' identified by \code{trip_id}. Origin and destination columns are
#' standardized to \code{origin} and \code{destination}; all other columns
#' remain user-defined trip or traveler attributes. An optional
#' \code{expansion_factor} column represents the number of trips represented
#' by an observed trip. When absent, each record has unit weight. The factor is
#' used by trip aggregation helpers unless another weight column is chosen.
#'
#' @param data A data.frame, list, or data.table containing one row per
#' observed trip.
#' @param trip_id Character string specifying the unique trip-identifier column.
#' @param origins Character string specifying the origin-zone column.
#' @param destinations Character string specifying the destination-zone column.
#' @param expansion_factor Optional character string specifying a finite,
#' non-negative numeric expansion-factor column.
#'
#' @return An object of classes \code{fsm_trip} and \code{data.table}, keyed
#' by \code{trip_id}.
#' @export
#' @importFrom data.table setDT setnames setkeyv setattr
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See Chaps. 3 and 4 for trip-based demand models.
#'
#' @examples
#' survey <- data.frame(
#'   id = 1:3,
#'   from = c(1, 2, 1),
#'   to = c(2, 1, 3),
#'   mode = c("car", "walk", "transit"),
#'   factor = c(1.2, 0.8, 1.5)
#' )
#'
#' fsm_trip(survey, "id", "from", "to", expansion_factor = "factor")
fsm_trip <- function(
  data,
  trip_id,
  origins = "origin",
  destinations = "destination",
  expansion_factor = NULL
) {
  if (missing(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }
  if (missing(trip_id)) {
    stop("`trip_id` must be supplied.", call. = FALSE)
  }
  if (!is.data.frame(data) && !is.list(data)) {
    stop("`data` must be a data.frame, data.table, or list.", call. = FALSE)
  }

  column_args <- c(trip_id = trip_id, origins = origins, destinations = destinations)
  if (any(!vapply(column_args, fsm_is_single_string, logical(1)))) {
    stop("`trip_id`, `origins`, and `destinations` must be single character strings.", call. = FALSE)
  }
  if (length(unique(column_args)) != length(column_args)) {
    stop("`trip_id`, `origins`, and `destinations` must identify different columns.", call. = FALSE)
  }
  if (!is.null(expansion_factor) && !fsm_is_single_string(expansion_factor)) {
    stop("`expansion_factor` must be NULL or a single character string.", call. = FALSE)
  }

  if (!inherits(data, "data.table")) {
    data.table::setDT(data)
  }
  required <- c(trip_id, origins, destinations, expansion_factor)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop("Missing required column(s): ", paste(missing_columns, collapse = ", "), ".", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("`data` must contain at least one trip record.", call. = FALSE)
  }

  standardized <- c(trip_id = "trip_id", origins = "origin", destinations = "destination")
  old <- unname(column_args)
  new <- unname(standardized)
  if (!is.null(expansion_factor)) {
    old <- c(old, expansion_factor)
    new <- c(new, "expansion_factor")
  }
  projected_names <- names(data)
  projected_names[match(old, projected_names)] <- new
  if (anyDuplicated(projected_names)) {
    stop("Renaming would create duplicate column names.", call. = FALSE)
  }
  data.table::setnames(data, old = old, new = new)

  if (anyNA(data[["trip_id"]]) || anyDuplicated(data[["trip_id"]])) {
    stop("`trip_id` must uniquely identify non-missing records.", call. = FALSE)
  }
  if (anyNA(data[["origin"]]) || anyNA(data[["destination"]])) {
    stop("Trip origins and destinations cannot contain missing values.", call. = FALSE)
  }
  if ("expansion_factor" %in% names(data)) {
    factors <- data[["expansion_factor"]]
    if (!is.numeric(factors) || anyNA(factors) || any(!is.finite(factors)) || any(factors < 0)) {
      stop("`expansion_factor` must contain finite, non-negative numeric values.", call. = FALSE)
    }
  }

  data.table::setcolorder(
    data,
    c("trip_id", "origin", "destination", setdiff(names(data), c("trip_id", "origin", "destination")))
  )
  data.table::setkeyv(data, "trip_id")
  data.table::setattr(data, "class", c("fsm_trip", class(data)))
  data
}
