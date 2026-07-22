#' Derive an Origin-Destination Object from Trip Records
#'
#' @description Aggregates disaggregate trip records into an \code{fsm_od}
#' object. Each row represents one trip unless \code{weights} names a numeric
#' trip-count or expansion-weight column. Selected modes may be aggregated by
#' supplying \code{modes}. This provides a direct bridge from trip-level
#' observations to aggregate demand used by the trip-based four-step model.
#' Person identifiers may link records, but this aggregation does not model
#' dependencies among trips in a chain.
#'
#' @param data A data.frame, data.table, list, or \code{fsm_trip}
#' containing trip records.
#' @param origins Character string naming the origin-zone column.
#' @param destinations Character string naming the destination-zone column.
#' @param weights Optional character string naming a finite, non-negative
#' numeric trip-weight column. For an \code{fsm_trip}, the default uses
#' \code{expansion_factor} when present; otherwise each row has weight one.
#' @param modes Optional vector of mode values to retain before aggregation.
#' A value matches either a complete mode-chain label or any component of a
#' chain separated by \code{+}; for example, \code{"transit"} matches both
#' \code{"transit"} and \code{"walk+transit"}. Multiple values use OR
#' matching. If \code{NULL}, all modes are included.
#' @param mode Character string naming the mode column. It is required only
#' when \code{modes} is not \code{NULL}.
#'
#' @return An \code{fsm_od} object containing one row per observed
#' origin-destination pair.
#' @export
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See Chaps. 3 and 4 for disaggregate demand and its
#' aggregation into origin-destination flows.
#'
#' @examples
#' od <- fsm_od_from_trips(fsm_toy_trip)
#' car_od <- fsm_od_from_trips(fsm_toy_trip, modes = "car")
fsm_od_from_trips <- function(
  data,
  origins = "origin",
  destinations = "destination",
  weights = NULL,
  modes = NULL,
  mode = "mode"
) {
  if (missing(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }
  if (!is.data.frame(data) && !is.list(data)) {
    stop("`data` must be a data.frame, data.table, or list.", call. = FALSE)
  }

  column_args <- c(origins = origins, destinations = destinations)
  bad_args <- names(column_args)[!vapply(
    column_args,
    fsm_is_single_string,
    logical(1)
  )]
  if (length(bad_args)) {
    stop(
      "Column arguments must be single character strings: ",
      paste(bad_args, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (identical(origins, destinations)) {
    stop("`origins` and `destinations` must identify different columns.", call. = FALSE)
  }
  if (!is.null(weights) && !fsm_is_single_string(weights)) {
    stop("`weights` must be NULL or a single character string.", call. = FALSE)
  }
  if (!is.null(modes) && (!length(modes) || anyNA(modes))) {
    stop("`modes` must contain at least one non-missing value.", call. = FALSE)
  }
  if (!is.null(modes) && !fsm_is_single_string(mode)) {
    stop("`mode` must be a single character string.", call. = FALSE)
  }

  trips <- data.table::as.data.table(data.table::copy(data))
  required <- c(origins, destinations)
  if (is.null(weights) && inherits(data, "fsm_trip") && "expansion_factor" %in% names(trips)) {
    weights <- "expansion_factor"
  }
  if (!is.null(weights)) {
    required <- c(required, weights)
  }
  if (!is.null(modes)) {
    required <- c(required, mode)
  }
  missing_columns <- setdiff(required, names(trips))
  if (length(missing_columns)) {
    stop(
      "Missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (anyNA(trips[[origins]])) {
    stop("Trip origins cannot contain missing values.", call. = FALSE)
  }
  if (anyNA(trips[[destinations]])) {
    stop("Trip destinations cannot contain missing values.", call. = FALSE)
  }

  if (!is.null(modes)) {
    keep_modes <- fsm_trip_mode_match(trips[[mode]], modes)
    trips <- trips[keep_modes]
    if (!nrow(trips)) {
      stop("No trip records match `modes`.", call. = FALSE)
    }
  }

  trip_weights <- if (is.null(weights)) {
    rep(1, nrow(trips))
  } else {
    trips[[weights]]
  }
  if (!is.numeric(trip_weights) || anyNA(trip_weights) ||
      any(!is.finite(trip_weights)) || any(trip_weights < 0)) {
    stop("Trip `weights` must contain finite, non-negative numeric values.", call. = FALSE)
  }

  aggregate_data <- data.table::data.table(
    origin = trips[[origins]],
    destination = trips[[destinations]],
    demand = trip_weights
  )
  aggregate_data <- aggregate_data[
    , list(demand = sum(demand)),
    by = c("origin", "destination")
  ]

  fsm_od(aggregate_data, "origin", "destination", "demand")
}

fsm_trip_mode_match <- function(trip_modes, modes) {
  trip_modes <- as.character(trip_modes)
  vapply(trip_modes, function(trip_mode) {
    if (is.na(trip_mode)) {
      return(FALSE)
    }
    trip_mode %in% modes || any(strsplit(trip_mode, "+", fixed = TRUE)[[1L]] %in% modes)
  }, logical(1))
}

#' Derive Production and Attraction Totals from Trip Records
#'
#' @description Aggregates trip records into an \code{fsm_od} object and then
#' computes productions and attractions. Supplying \code{zones} includes known
#' zones with no observed trips and validates trip-zone coverage.
#'
#' @inheritParams fsm_od_from_trips
#' @param zones Optional \code{fsm_zone} object passed to
#' \code{fsm_od_totals()}.
#'
#' @return An \code{fsm_od_totals} object.
#' @export
#'
#' @examples
#' fsm_od_totals_from_trips(fsm_toy_trip, zones = fsm_toy_zone)
fsm_od_totals_from_trips <- function(
  data,
  origins = "origin",
  destinations = "destination",
  weights = NULL,
  modes = NULL,
  mode = "mode",
  zones = NULL
) {
  if (missing(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }
  od <- fsm_od_from_trips(
    data = data,
    origins = origins,
    destinations = destinations,
    weights = weights,
    modes = modes,
    mode = mode
  )
  fsm_od_totals(od, zones = zones)
}

#' Derive a Zone Set from Trip Records
#'
#' @description Extracts the distinct origin and destination identifiers from
#' trip records and returns a minimal \code{fsm_zone} object. Zones with no
#' observed trips cannot be inferred from trip records; supply their identifiers
#' through \code{include} when they are known.
#'
#' @inheritParams fsm_od_from_trips
#' @param include Optional vector of additional zone identifiers, such as zones
#' with no observed trips.
#'
#' @return An \code{fsm_zone} object containing a \code{zone_id} column.
#' @export
#'
#' @examples
#' fsm_zone_from_trips(fsm_toy_trip)
#' fsm_zone_from_trips(fsm_toy_trip, include = 7L)
fsm_zone_from_trips <- function(
  data,
  origins = "origin",
  destinations = "destination",
  weights = NULL,
  modes = NULL,
  mode = "mode",
  include = NULL
) {
  if (missing(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }
  if (anyNA(include)) {
    stop("`include` cannot contain missing values.", call. = FALSE)
  }

  od <- fsm_od_from_trips(
    data = data,
    origins = origins,
    destinations = destinations,
    weights = weights,
    modes = modes,
    mode = mode
  )
  zone_ids <- unique(c(include, od[["origin"]], od[["destination"]]))
  zone_data <- data.table::data.table(zone_id = zone_ids)
  fsm_zone(zone_data, "zone_id", od = od)
}
