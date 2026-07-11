#' Zone Attribute Constructor
#'
#' @description Converts a tabular zone input into a keyed \code{fsm_zone}
#' object. Only the zone identifier is standardized to \code{zone_id}; all other
#' columns are kept as user-defined attributes and may represent population,
#' jobs, income, schools, land use, area, or any other modeling variable. The
#' package does not force a fixed zone schema beyond the unique identifier. The
#' input is converted and renamed by reference; use \code{data.table::copy()}
#' first when the original object must remain unchanged.
#'
#' If an \code{fsm_od} object is supplied, the constructor checks that every
#' origin and destination appearing in the OD table is present in the zone table.
#'
#' @param data A data.frame, list, or data.table containing one row per zone.
#' @param zones Character string specifying the zone identifier column name.
#' @param od Optional \code{fsm_od} object used to validate zone coverage.
#'
#' @return An object of classes \code{fsm_zone} and \code{data.table}, keyed by
#' \code{zone_id}.
#' @export
#' @importFrom data.table setDT setnames setkeyv setattr
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See the discussion of aggregate, zone-based
#' travel-demand models and zonal levels of representation.
#'
#' @examples
#' zones <- data.frame(
#'   id = c(1, 2, 3),
#'   population = c(1000, 2500, 1800),
#'   jobs = c(300, 1200, 700)
#' )
#'
#' fsm_zone(zones, "id")
fsm_zone <- function(data, zones, od = NULL) {
  if (missing(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }

  if (missing(zones)) {
    stop("`zones` must be supplied.", call. = FALSE)
  }

  if (!is.data.frame(data) && !is.list(data)) {
    stop("`data` must be a data.frame, data.table, or list.", call. = FALSE)
  }

  if (!fsm_is_single_string(zones)) {
    stop("`zones` must be a single character string.", call. = FALSE)
  }

  if (!inherits(data, "data.table")) {
    data.table::setDT(data)
  }

  if (!zones %in% names(data)) {
    stop("Missing required column: ", zones, ".", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one zone.", call. = FALSE)
  }

  projected_names <- names(data)
  projected_names[match(zones, projected_names)] <- "zone_id"

  if (anyDuplicated(projected_names)) {
    stop(
      "Renaming would create duplicate column names. Rename the existing ",
      "`zone_id` column before calling `fsm_zone()`.",
      call. = FALSE
    )
  }

  if (!identical(zones, "zone_id")) {
    data.table::setnames(data, old = zones, new = "zone_id", skip_absent = FALSE)
  }

  if (anyNA(data[["zone_id"]])) {
    stop("`zone_id` cannot contain missing values.", call. = FALSE)
  }

  duplicated_zones <- data[["zone_id"]][duplicated(data[["zone_id"]])]
  if (length(duplicated_zones) > 0L) {
    stop(
      "`zone_id` must uniquely identify zones. Duplicated value(s): ",
      paste(utils::head(unique(duplicated_zones), 5L), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.null(od)) {
    fsm_check_zone_od(data, od)
  }

  data.table::setkeyv(data, "zone_id")
  data.table::setattr(data, "class", c("fsm_zone", class(data)))

  data
}

#' @noRd
fsm_check_zone_od <- function(zones, od) {
  if (missing(zones)) {
    stop("`zones` must be supplied.", call. = FALSE)
  }

  if (missing(od)) {
    stop("`od` must be supplied.", call. = FALSE)
  }

  if (!inherits(od, "fsm_od")) {
    stop("`od` must be an `fsm_od` object.", call. = FALSE)
  }

  if (!"zone_id" %in% names(zones)) {
    stop("`zones` must contain a `zone_id` column.", call. = FALSE)
  }

  od_zones <- unique(c(od[["origin"]], od[["destination"]]))
  missing_zones <- setdiff(od_zones, zones[["zone_id"]])

  if (length(missing_zones) > 0L) {
    stop(
      "`zones` does not cover all OD origins/destinations. Missing zone(s): ",
      paste(utils::head(missing_zones, 10L), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
