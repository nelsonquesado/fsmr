#' Plot an Origin-Destination Object
#'
#' @description Plots an origin-destination matrix from an \code{fsm_od}
#' object. Missing demand is labeled \code{NA}. This gives a quick visual check
#' of the trip table before distribution, mode choice, or assignment steps.
#'
#' @param x An \code{fsm_od} object.
#' @param zones Optional \code{fsm_zone} object. When supplied, all zones are
#' shown and OD coverage is validated.
#' @param ... Additional arguments passed to \code{graphics::image()}.
#'
#' @return Invisibly returns the OD matrix used for plotting.
#' @export
#'
#' @examples
#' od <- fsm_toy_od
#' plot(od)
plot.fsm_od <- function(x, zones = NULL, ...) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  if (!inherits(x, "fsm_od")) {
    stop("`x` must be an `fsm_od` object.", call. = FALSE)
  }

  od_dt <- data.table::as.data.table(x)
  if (!is.null(zones)) {
    if (!inherits(zones, "fsm_zone")) {
      stop("`zones` must be an `fsm_zone` object.", call. = FALSE)
    }
    fsm_check_zone_od(zones, x)
    zone_ids <- data.table::as.data.table(zones)[["zone_id"]]
  } else {
    zone_ids <- sort(unique(c(od_dt[["origin"]], od_dt[["destination"]])))
  }

  mat <- matrix(0, nrow = length(zone_ids), ncol = length(zone_ids))
  rownames(mat) <- zone_ids
  colnames(mat) <- zone_ids

  idx_row <- match(od_dt[["origin"]], zone_ids)
  idx_col <- match(od_dt[["destination"]], zone_ids)
  mat[cbind(idx_row, idx_col)] <- od_dt[["demand"]]

  graphics::image(
    x = seq_along(zone_ids),
    y = seq_along(zone_ids),
    z = mat[, rev(seq_len(ncol(mat))), drop = FALSE],
    axes = FALSE,
    xlab = "Origin",
    ylab = "Destination",
    col = grDevices::colorRampPalette(c("#F7FBFF", "#6BAED6", "#08519C"))(12),
    ...
  )
  graphics::axis(1, at = seq_along(zone_ids), labels = zone_ids)
  graphics::axis(2, at = seq_along(zone_ids), labels = rev(zone_ids))
  graphics::box()

  label_df <- expand.grid(
    origin = seq_along(zone_ids),
    destination = seq_along(zone_ids)
  )
  label_df$value <- as.vector(mat)
  fill_range <- range(mat, finite = TRUE)
  fill_norm <- if (diff(fill_range) == 0) {
    matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  } else {
    (mat - fill_range[1]) / diff(fill_range)
  }
  fill_df <- expand.grid(
    origin = seq_along(zone_ids),
    destination = seq_along(zone_ids)
  )
  fill_df$fill_norm <- as.vector(fill_norm)
  label_df$x <- label_df$origin
  label_df$y <- length(zone_ids) - label_df$destination + 1L
  label_df$label <- ifelse(
    is.na(label_df$value),
    "NA",
    ifelse(
      label_df$value > 0,
      format(label_df$value, trim = TRUE, scientific = FALSE),
      ""
    )
  )
  label_df$col <- ifelse(
    !is.na(fill_df$fill_norm) & fill_df$fill_norm > 0.55,
    "#F9FAFB",
    "#1F2937"
  )
  label_df$col[label_df$label == ""] <- "#1F2937"

  graphics::text(
    x = label_df$x,
    y = label_df$y,
    labels = label_df$label,
    col = label_df$col,
    cex = 0.75
  )

  invisible(mat)
}

#' Plot OD Totals
#'
#' @description Plots observed productions and attractions from an
#' \code{fsm_od_totals} object.
#'
#' @param x An \code{fsm_od_totals} object.
#' @param ... Additional arguments passed to \code{graphics::barplot()}.
#'
#' @return Invisibly returns \code{x}.
#' @export
#'
#' @examples
#' od <- fsm_toy_od
#' totals <- fsm_od_totals(od)
#' plot(totals)
plot.fsm_od_totals <- function(x, ...) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  if (!inherits(x, "fsm_od_totals")) {
    stop("`x` must be an `fsm_od_totals` object.", call. = FALSE)
  }

  values <- rbind(
    production = x[["production"]],
    attraction = x[["attraction"]]
  )

  graphics::barplot(
    values,
    beside = TRUE,
    names.arg = x[["zone_id"]],
    col = c("#2B6CB0", "#D97706"),
    border = NA,
    xlab = "Zone",
    ylab = "Trips",
    legend.text = c("Production", "Attraction"),
    args.legend = list(x = "topright", bty = "n"),
    ...
  )

  invisible(x)
}

#' Plot Zone Attributes
#'
#' @description Plots the numeric attributes stored in an \code{fsm_zone}
#' object against \code{zone_id}. The panels are arranged in a wrapped grid so
#' each numeric attribute gets its own y axis while the page stays compact.
#' Non-numeric attributes are ignored.
#'
#' @param x An \code{fsm_zone} object.
#' @param ... Additional arguments passed to \code{graphics::plot()}.
#'
#' @return Invisibly returns \code{x}.
#' @export
#'
#' @examples
#' zones <- fsm_toy_zone
#' plot(zones)
plot.fsm_zone <- function(x, ...) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  if (!inherits(x, "fsm_zone")) {
    stop("`x` must be an `fsm_zone` object.", call. = FALSE)
  }

  numeric_cols <- names(x)[vapply(x, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, "zone_id")

  if (length(numeric_cols) == 0L) {
    stop("`x` must contain at least one numeric attribute to plot.", call. = FALSE)
  }

  values <- data.table::as.data.table(x)[, c("zone_id", numeric_cols), with = FALSE]

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  n_panels <- length(numeric_cols)
  n_cols <- ceiling(sqrt(n_panels))
  n_rows <- ceiling(n_panels / n_cols)
  graphics::par(
    mfrow = c(n_rows, n_cols),
    mar = c(4.8, 4.5, 2.5, 1) + 0.1,
    mgp = c(2.8, 0.8, 0)
  )

  for (panel_index in seq_along(numeric_cols)) {
    col_name <- numeric_cols[[panel_index]]
    x_label <- if (panel_index > (n_rows - 1L) * n_cols) "Zone" else ""

    graphics::plot(
      x = values[["zone_id"]],
      y = values[[col_name]],
      type = "b",
      pch = 19,
      lty = 1,
      col = "#2B6CB0",
      xlab = x_label,
      ylab = col_name,
      main = col_name,
      ...
    )
  }

  invisible(x)
}

#' Plot Origin-Destination Survey Attributes
#'
#' @description Plots weighted distributions for the attributes in an
#' \code{fsm_trip} object according to column class. When present,
#' \code{expansion_factor} supplies the trip weights; otherwise each record
#' has unit weight. Numeric and
#' integer attributes are shown as weighted histograms. Character, factor,
#' ordered, and logical attributes are shown as weighted bars. Character times
#' in \code{HH:MM:SS} format are shown as weighted numeric histograms.
#' Unsupported classes are skipped with a warning. Panels are arranged in a wrapped grid.
#' The optional \code{zone_id}, \code{origin}, and \code{destination} columns
#' are shown as weighted bar plots. Record and person identifiers are not
#' plotted as attributes.
#'
#' @param x An \code{fsm_trip} object.
#' @param ... Additional graphical arguments passed to \code{graphics::plot()}
#' for numeric attributes and \code{graphics::barplot()} for categorical
#' attributes.
#'
#' @return Invisibly returns \code{x}.
#' @export
#'
#' @examples
#' plot(fsm_toy_trip)
plot.fsm_trip <- function(x, ...) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }
  if (!inherits(x, "fsm_trip")) {
    stop("`x` must be an `fsm_trip` object.", call. = FALSE)
  }

  attribute_cols <- setdiff(
    names(x),
    c("trip_id", "individual_id", "person_id", "expansion_factor")
  )
  if (length(attribute_cols) == 0L) {
    stop("`x` must contain at least one trip attribute to plot.", call. = FALSE)
  }

  supported <- vapply(attribute_cols, function(attribute) {
    values <- x[[attribute]]
    is.numeric(values) || is.character(values) || is.factor(values) ||
      is.logical(values)
  }, logical(1))
  unsupported <- attribute_cols[!supported]
  if (length(unsupported)) {
    warning(
      "Unsupported trip attribute class; skipped column(s): ",
      paste(unsupported, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  attribute_cols <- attribute_cols[supported]
  if (length(attribute_cols) == 0L) {
    stop("`x` has no supported trip attributes to plot.", call. = FALSE)
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  n_panels <- length(attribute_cols)
  n_cols <- ceiling(sqrt(n_panels))
  n_rows <- ceiling(n_panels / n_cols)
  graphics::par(mfrow = c(n_rows, n_cols))

  trip_weights <- fsm_trip_weights(x)
  for (panel_index in seq_along(attribute_cols)) {
    attribute <- attribute_cols[[panel_index]]
    weights <- trip_weights
    values <- x[[attribute]]
    observed_time <- !is.na(values)
    if (is.character(values) && any(observed_time) && all(grepl(
      "^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$",
      values[observed_time]
    ))) {
      values <- as.numeric(substr(values, 1L, 2L)) +
        as.numeric(substr(values, 4L, 5L)) / 60 +
        as.numeric(substr(values, 7L, 8L)) / 3600
    }
    y_label <- if ((panel_index - 1L) %% n_cols == 0L) {
      "Represented trips"
    } else {
      ""
    }

    categorical_ids <- c("zone_id", "home_zone", "origin", "destination")
    if (is.numeric(values) && !attribute %in% categorical_ids) {
      graphics::par(mar = c(3.5, 4, 2.5, 1) + 0.1)
      observed <- !is.na(values)
      if (!any(observed)) {
        graphics::plot.new()
        graphics::title(main = attribute)
        graphics::text(0.5, 0.5, "No observed values", col = "#4B5563")
        next
      }

      histogram <- graphics::hist(values[observed], plot = FALSE)
      bins <- cut(
        values[observed],
        breaks = histogram$breaks,
        include.lowest = TRUE,
        labels = FALSE
      )
      bin_counts <- numeric(length(histogram$counts))
      weighted_bins <- tapply(weights[observed], bins, sum)
      bin_counts[as.integer(names(weighted_bins))] <- weighted_bins
      histogram$counts <- bin_counts
      histogram$density <- bin_counts / sum(bin_counts) / diff(histogram$breaks)
      graphics::plot(
        histogram,
        freq = TRUE,
        col = "#2B6CB0",
        border = "white",
        xlab = "",
        ylab = y_label,
        main = attribute,
        ...
      )

      missing_count <- sum(weights[is.na(values)])
      if (missing_count > 0) {
        graphics::mtext(
          paste0("Missing: ", fsm_format_number(missing_count)),
          side = 3,
          adj = 1,
          cex = 0.75,
          col = "#4B5563"
        )
      }
    } else {
      labels <- ifelse(is.na(values), "Missing", as.character(values))
      represented <- tapply(weights, labels, sum)
      bottom_margin <- min(9, max(5, 2.5 + 0.35 * max(nchar(names(represented)))))
      graphics::par(mar = c(bottom_margin, 4, 2.5, 1) + 0.1)
      graphics::barplot(
        represented,
        col = "#2B6CB0",
        border = NA,
        xlab = "",
        ylab = y_label,
        main = attribute,
        las = 2,
        cex.names = 0.8,
        ...
      )
    }
  }

  invisible(x)
}

#' Plot a Generation Model
#'
#' @description Plots the available production and attraction predictions from
#' a fitted \code{fsm_generation} object against its stored training data or
#' new zonal or trip-level data.
#'
#' @param x An \code{fsm_generation} object.
#' @param newdata Optional zonal or trip-level data used to generate
#' predictions. If omitted, the training data stored in \code{x} are used.
#' @param negative Character string controlling negative predictions. See
#' \code{predict.fsm_generation()}.
#' @param ... Additional arguments passed to \code{graphics::barplot()}.
#'
#' @return Invisibly returns the prediction data.table used for plotting.
#' @export
#'
#' @examples
#' zones <- fsm_toy_zone
#' od <- fsm_toy_od
#' totals <- fsm_od_totals(od, zones)
#' fit <- fsm_generation(
#'   zones,
#'   totals,
#'   method = "regression",
#'   production_formula = production ~ 0 + population,
#'   attraction_formula = attraction ~ 0 + jobs
#' )
#' plot(fit)
plot.fsm_generation <- function(
  x,
  newdata = NULL,
  negative = c("zero", "keep", "error"),
  ...
) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  if (!inherits(x, "fsm_generation")) {
    stop("`x` must be an `fsm_generation` object.", call. = FALSE)
  }
  negative <- match.arg(negative)

  pred <- predict(x, newdata = newdata, negative = negative)
  id_col <- if (is.null(x$id_col)) {
    fsm_generation_id(pred, "predictions")
  } else {
    x$id_col
  }
  outcomes <- x$outcomes
  values <- t(as.matrix(pred[, outcomes, with = FALSE]))
  colors <- c(production = "#2B6CB0", attraction = "#D97706")[outcomes]
  labels <- c(production = "Production", attraction = "Attraction")[outcomes]

  graphics::barplot(
    values,
    beside = TRUE,
    names.arg = pred[[id_col]],
    col = colors,
    border = NA,
    xlab = if (identical(id_col, "zone_id")) "Zone" else "Trip record",
    ylab = "Trips",
    legend.text = labels,
    args.legend = list(x = "topright", bty = "n"),
    ...
  )

  invisible(pred)
}
