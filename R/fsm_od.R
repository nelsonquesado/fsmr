#' Origin-Destination Table Constructor
#'
#' @description Converts an aggregate tabular input into an `fsm_od` object.
#' It uses `data.table` to keep the OD table keyed by origin and destination for
#' straightforward lookup and manipulation during distribution, mode choice, and
#' assignment steps. Each origin-destination pair must occur once, and demand
#' must be finite when observed and cannot be negative. Missing demand is
#' retained as \code{NA} so incomplete observations can be diagnosed. The input
#' represents trip-based demand; dependencies among trips in a chain are not
#' encoded by this object. The input
#' is converted and renamed by reference; use \code{data.table::copy()} first
#' when the original object must remain unchanged.
#'
#' @param data A data.frame, list, or data.table.
#' @param origins Character string specifying the origin zone column name.
#' @param destinations Character string specifying the destination zone column name.
#' @param demands Character string specifying the demand or trip volume column name.
#'
#' @return An object of classes \code{fsm_od} and \code{data.table}, keyed by origin and destination.
#' @export
#' @importFrom data.table setDT setnames setkeyv setattr
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See the discussion of travel-demand models,
#' origin-destination demand flows, and assignment models using OD demand.
#'
#' @examples
#' raw_demand <- data.frame(
#'   zone_o = c(1, 1, 2, 3),
#'   zone_d = c(2, 3, 1, 2),
#'   trips = c(100, 50, 200, 30)
#' )
#'
#' od <- fsm_od(raw_demand, "zone_o", "zone_d", "trips")
fsm_od <- function(data, origins, destinations, demands) {
  if (missing(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }

  if (missing(origins)) {
    stop("`origins` must be supplied.", call. = FALSE)
  }

  if (missing(destinations)) {
    stop("`destinations` must be supplied.", call. = FALSE)
  }

  if (missing(demands)) {
    stop("`demands` must be supplied.", call. = FALSE)
  }

  if (!is.data.frame(data) && !is.list(data)) {
    stop("`data` must be a data.frame, data.table, or list.", call. = FALSE)
  }

  column_args <- c(origins = origins, destinations = destinations, demands = demands)
  bad_args <- names(column_args)[!vapply(column_args, fsm_is_single_string, logical(1))]

  if (length(bad_args) > 0L) {
    stop(
      "Column arguments must be single character strings: ",
      paste(bad_args, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!inherits(data, "data.table")) {
    data.table::setDT(data)
  }

  old_names <- unname(column_args)
  missing_cols <- setdiff(old_names, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (anyDuplicated(old_names)) {
    stop(
      "`origins`, `destinations`, and `demands` must refer to different columns.",
      call. = FALSE
    )
  }

  projected_names <- names(data)
  match_pos <- match(old_names, projected_names)
  projected_names[match_pos] <- c("origin", "destination", "demand")

  if (anyDuplicated(projected_names)) {
    stop(
      "Renaming would create duplicate column names. Rename existing origin, ",
      "destination, or demand columns before calling `fsm_od()`.",
      call. = FALSE
    )
  }

  data.table::setnames(
    data,
    old = old_names,
    new = c("origin", "destination", "demand"),
    skip_absent = FALSE
  )

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one OD pair.", call. = FALSE)
  }

  if (anyNA(data[["origin"]])) {
    stop("`origin` cannot contain missing values.", call. = FALSE)
  }

  if (anyNA(data[["destination"]])) {
    stop("`destination` cannot contain missing values.", call. = FALSE)
  }

  if (!is.numeric(data[["demand"]])) {
    stop("`demand` must be numeric.", call. = FALSE)
  }

  observed_demand <- data[["demand"]][!is.na(data[["demand"]])]
  if (any(!is.finite(observed_demand))) {
    stop("`demand` must contain only finite values.", call. = FALSE)
  }

  if (any(data[["demand"]] < 0, na.rm = TRUE)) {
    stop("`demand` cannot contain negative values.", call. = FALSE)
  }

  duplicate_pairs <- duplicated(data, by = c("origin", "destination"))
  if (any(duplicate_pairs)) {
    duplicated_rows <- data[duplicate_pairs, c("origin", "destination"), with = FALSE]
    first_pair <- duplicated_rows[1L]
    stop(
      "Each origin-destination pair must be unique. Duplicated pair: ",
      first_pair[["origin"]], " -> ", first_pair[["destination"]], ".",
      call. = FALSE
    )
  }

  data.table::setkeyv(data, c("origin", "destination"))
  data.table::setattr(data, "class", c("fsm_od", class(data)))

  data
}
