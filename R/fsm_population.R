#' Population Attribute Constructor
#'
#' @description Converts a tabular population input into a keyed
#' \code{fsm_population} object. The identifier is standardized to
#' \code{population_id}. Each row represents one disaggregate observation,
#' such as an individual or a trip carrying individual attributes. A row may
#' represent multiple identical observations through \code{population_count},
#' which defaults to one. All other columns are kept as user-defined attributes. A
#' \code{zone_id} column is optional and has no special requirement in the
#' constructor. Person and trip identifiers may be retained to prepare future
#' trip-chain analyses, but current fsmr models treat trips independently. The
#' input is converted and renamed by reference; use
#' \code{data.table::copy()} first when the original object must remain
#' unchanged.
#'
#' @param data A data.frame, list, or data.table containing one row per record.
#' @param population_id Character string specifying the unique record-identifier
#' column name.
#' @param population_count Optional character string specifying a column with
#' the number of identical observations represented by each row. For trip
#' records, this can be a trip expansion count. Values must be positive whole
#' numbers. If omitted, an existing \code{population_count} column is used;
#' otherwise every row is assigned a count of one.
#'
#' @return An object of classes \code{fsm_population} and \code{data.table},
#' keyed by \code{population_id}. The standardized \code{population_count}
#' column records observation multiplicity.
#' @export
#' @importFrom data.table setDT setnames setkeyv setattr
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See Chaps. 3 and 4 for disaggregate demand models
#' and individual- and household-level attributes.
#'
#' @examples
#' pop <- data.frame(
#'   id = 1:3,
#'   people = c(1, 2, 1),
#'   income = c(1800, 2400, 1200),
#'   car_ownership = c(1, 2, 0)
#' )
#'
#' fsm_population(pop, "id", population_count = "people")
fsm_population <- function(data, population_id, population_count = NULL) {
  if (missing(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }

  if (missing(population_id)) {
    stop("`population_id` must be supplied.", call. = FALSE)
  }

  if (!is.data.frame(data) && !is.list(data)) {
    stop("`data` must be a data.frame, data.table, or list.", call. = FALSE)
  }

  if (!fsm_is_single_string(population_id)) {
    stop("`population_id` must be a single character string.", call. = FALSE)
  }

  if (!is.null(population_count) && !fsm_is_single_string(population_count)) {
    stop("`population_count` must be NULL or a single character string.", call. = FALSE)
  }

  if (!is.null(population_count) && identical(population_id, population_count)) {
    stop("`population_id` and `population_count` must identify different columns.", call. = FALSE)
  }

  if (!inherits(data, "data.table")) {
    data.table::setDT(data)
  }

  if (!population_id %in% names(data)) {
    stop("Missing required column: ", population_id, ".", call. = FALSE)
  }

  if (!is.null(population_count) && !population_count %in% names(data)) {
    stop("Missing required column: ", population_count, ".", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one disaggregate record.", call. = FALSE)
  }

  projected_names <- names(data)
  projected_names[match(population_id, projected_names)] <- "population_id"
  if (!is.null(population_count)) {
    projected_names[match(population_count, names(data))] <- "population_count"
  }

  if (anyDuplicated(projected_names)) {
    stop(
      "Renaming would create duplicate column names. Rename the existing ",
      "standardized identifier or count columns before calling ",
      "`fsm_population()`.",
      call. = FALSE
    )
  }

  rename_old <- rename_new <- character()
  if (!identical(population_id, "population_id")) {
    rename_old <- c(rename_old, population_id)
    rename_new <- c(rename_new, "population_id")
  }
  if (!is.null(population_count) && !identical(population_count, "population_count")) {
    rename_old <- c(rename_old, population_count)
    rename_new <- c(rename_new, "population_count")
  }
  if (length(rename_old)) {
    data.table::setnames(
      data,
      old = rename_old,
      new = rename_new,
      skip_absent = FALSE
    )
  }
  if (is.null(population_count) && !"population_count" %in% names(data)) {
    data.table::set(data, j = "population_count", value = rep(1L, nrow(data)))
  }

  if (anyNA(data[["population_id"]])) {
    stop("`population_id` cannot contain missing values.", call. = FALSE)
  }

  if (anyDuplicated(data[["population_id"]])) {
    stop("`population_id` must uniquely identify records.", call. = FALSE)
  }

  counts <- data[["population_count"]]
  if (!is.numeric(counts) || anyNA(counts) || any(!is.finite(counts)) ||
      any(counts <= 0) || any(counts != floor(counts))) {
    stop("`population_count` must contain positive whole numbers.", call. = FALSE)
  }

  data.table::setcolorder(
    data,
    c("population_id", "population_count", setdiff(names(data), c("population_id", "population_count")))
  )
  data.table::setkeyv(data, "population_id")
  data.table::setattr(data, "class", c("fsm_population", class(data)))

  data
}
