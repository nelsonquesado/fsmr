#' Re-aggregate a Matrix to a Different Zone System
#'
#' @description Re-aggregates a numeric matrix by summing values according to
#' correspondence tables. This is useful when mapping blocks to zones, zones to
#' districts, or any other many-to-one spatial classification. Row and column
#' mappings may differ, and target identifiers may be numeric or character.
#'
#' @param x A numeric matrix with unique row and column names.
#' @param row_map A data.frame or data.table with two columns describing the
#' row correspondence.
#' @param col_map Optional correspondence table for the columns. If omitted,
#' the same mapping used for rows is also applied to columns.
#' @param from Optional name of the column in the correspondence table
#' containing the original identifiers. Defaults to the first column.
#' @param to Optional name of the column in the correspondence table containing
#' the target identifiers. Defaults to the second column.
#' @param col_from Optional source-identifier column in \code{col_map}. Defaults
#' to \code{from}.
#' @param col_to Optional target-identifier column in \code{col_map}. Defaults
#' to \code{to}.
#'
#' @return A numeric matrix aggregated to the target row and column systems.
#' @export
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See Sect. 1.3.1 for zoning and Sect. 4.3.1.2 for
#' properties related to zonal aggregation.
#'
#' @examples
#' m <- matrix(
#'   c(0, 1, 2, 3,
#'     4, 0, 5, 6,
#'     7, 8, 0, 9,
#'     1, 2, 3, 0),
#'   nrow = 4,
#'   byrow = TRUE
#' )
#' rownames(m) <- colnames(m) <- c("a", "b", "c", "d")
#'
#' map <- data.frame(
#'   old = c("a", "b", "c", "d"),
#'   new = c("x", "x", "y", "y")
#' )
#'
#' fsm_reaggregate_matrix(m, map)
fsm_reaggregate_matrix <- function(
  x,
  row_map,
  col_map = row_map,
  from = NULL,
  to = NULL,
  col_from = from,
  col_to = to
) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  if (missing(row_map)) {
    stop("`row_map` must be supplied.", call. = FALSE)
  }

  if (!is.matrix(x) || !is.numeric(x)) {
    stop("`x` must be a numeric matrix.", call. = FALSE)
  }

  if (!is.data.frame(row_map) && !inherits(row_map, "data.table")) {
    stop("`row_map` must be a data.frame or data.table.", call. = FALSE)
  }

  if (!is.data.frame(col_map) && !inherits(col_map, "data.table")) {
    stop("`col_map` must be a data.frame or data.table.", call. = FALSE)
  }

  row_map <- data.table::as.data.table(row_map)
  col_map <- data.table::as.data.table(col_map)

  if (ncol(row_map) < 2L) {
    stop("`row_map` must contain at least two columns.", call. = FALSE)
  }

  if (ncol(col_map) < 2L) {
    stop("`col_map` must contain at least two columns.", call. = FALSE)
  }

  if (is.null(from)) {
    from <- names(row_map)[1L]
  }

  if (is.null(to)) {
    to <- names(row_map)[2L]
  }

  if (is.null(col_from)) {
    col_from <- names(col_map)[1L]
  }

  if (is.null(col_to)) {
    col_to <- names(col_map)[2L]
  }

  mapping_arguments <- list(from = from, to = to, col_from = col_from, col_to = col_to)
  invalid_arguments <- names(mapping_arguments)[
    !vapply(mapping_arguments, fsm_is_single_string, logical(1))
  ]
  if (length(invalid_arguments) > 0L) {
    stop(
      "Mapping-column arguments must be single character strings: ",
      paste(invalid_arguments, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!all(c(from, to) %in% names(row_map))) {
    stop("`row_map` must contain the `from` and `to` columns.", call. = FALSE)
  }

  if (!all(c(col_from, col_to) %in% names(col_map))) {
    stop("`col_map` must contain the `col_from` and `col_to` columns.", call. = FALSE)
  }

  if (anyNA(row_map[[from]]) || anyNA(row_map[[to]])) {
    stop("`row_map` identifiers cannot contain missing values.", call. = FALSE)
  }

  if (anyNA(col_map[[col_from]]) || anyNA(col_map[[col_to]])) {
    stop("`col_map` identifiers cannot contain missing values.", call. = FALSE)
  }

  if (anyDuplicated(row_map[[from]])) {
    stop("Source identifiers in `row_map` must be unique.", call. = FALSE)
  }

  if (anyDuplicated(col_map[[col_from]])) {
    stop("Source identifiers in `col_map` must be unique.", call. = FALSE)
  }

  row_ids <- rownames(x)
  col_ids <- colnames(x)

  if (is.null(row_ids) || is.null(col_ids)) {
    stop("`x` must have row names and column names.", call. = FALSE)
  }

  if (anyDuplicated(row_ids) || anyDuplicated(col_ids)) {
    stop("Row names and column names in `x` must be unique.", call. = FALSE)
  }

  row_match <- row_map[match(row_ids, row_map[[from]]), ]
  col_match <- col_map[match(col_ids, col_map[[col_from]]), ]

  if (anyNA(row_match[[to]]) || anyNA(col_match[[col_to]])) {
    stop("Correspondence tables must cover all row and column identifiers.", call. = FALSE)
  }

  row_target <- row_match[[to]]
  col_target <- col_match[[col_to]]

  row_levels <- unique(row_target)
  col_levels <- unique(col_target)

  out <- matrix(0, nrow = length(row_levels), ncol = length(col_levels))
  rownames(out) <- row_levels
  colnames(out) <- col_levels

  row_position <- match(row_target, row_levels)
  col_position <- match(col_target, col_levels)

  for (i in seq_along(row_ids)) {
    for (j in seq_along(col_ids)) {
      out[row_position[i], col_position[j]] <-
        out[row_position[i], col_position[j]] + x[i, j]
    }
  }

  out
}
