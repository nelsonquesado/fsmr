#' Toy origin-destination demand
#'
#' @description A small aggregate OD table for examples and demonstrations. It
#' includes positive, zero, and missing demand records and is aligned with
#' \code{fsm_toy_zone}.
#'
#' @format A keyed \code{fsm_od} object with 24 rows and 3 columns:
#' \describe{
#'   \item{origin}{Origin zone identifier.}
#'   \item{destination}{Destination zone identifier.}
#'   \item{demand}{Observed number of trips.}
#' }
#' @usage fsm_toy_od
#' @name fsm_toy_od
#' @docType data
#' @keywords datasets
#' @export
fsm_toy_od <- data.table::data.table(
  origin = c(
    1L, 1L, 1L, 1L, 1L,
    2L, 2L, 2L, 2L, 2L,
    3L, 3L, 3L, 3L, 3L,
    4L, 4L, 4L, 4L,
    5L, 5L, 5L, 5L,
    6L
  ),
  destination = c(
    2L, 3L, 4L, 5L, 6L,
    1L, 3L, 4L, 5L, 6L,
    1L, 2L, 4L, 5L, 6L,
    1L, 2L, 3L, 6L,
    1L, 2L, 3L, 4L,
    1L
  ),
  demand = c(
    1800, 950, 620, 0, 430,
    2100, 560, 720, 0, 380,
    1600, 740, 510, NA, 290,
    680, 880, 460, 340,
    520, 410, 300, 0,
    0
  )
)
data.table::setkeyv(fsm_toy_od, c("origin", "destination"))
data.table::setattr(fsm_toy_od, "class", c("fsm_od", class(fsm_toy_od)))

#' Toy zone attributes
#'
#' @description A small zone table aligned with \code{fsm_toy_od}. Zone 5 has
#' no positive attractions, zone 6 has no positive productions, and zone 7 does
#' not appear in the OD object at all. One zone name and one per-capita income
#' value are missing. The numeric attributes deliberately use different scales
#' to illustrate the separate panels used by \code{plot.fsm_zone()}.
#'
#' @format A keyed \code{fsm_zone} object with 7 rows and 8 columns:
#' \describe{
#'   \item{zone_id}{Unique zone identifier.}
#'   \item{name}{Illustrative zone name.}
#'   \item{area_type}{Illustrative cross-classification category.}
#'   \item{income_group}{Illustrative income category for multidimensional
#'   cross-classification examples.}
#'   \item{population}{Resident population.}
#'   \item{jobs}{Number of jobs.}
#'   \item{households}{Number of households.}
#'   \item{income_pc}{Average per-capita income.}
#' }
#' @usage fsm_toy_zone
#' @name fsm_toy_zone
#' @docType data
#' @keywords datasets
#' @export
fsm_toy_zone <- data.table::data.table(
  zone_id = 1:7,
  name = c(
    "Centro", "Aldeota", "Antonio Bezerra", "Benfica", "Messejana",
    "Passare", NA_character_
  ),
  area_type = c("central", "central", "outer", "central", "outer", "outer", "outer"),
  income_group = c("medium", "high", "medium", "medium", "low", "low", "low"),
  population = c(28500, 41500, 52000, 18500, 61000, 12400, 27500),
  jobs = c(64000, 71000, 19000, 28000, 24000, 5600, 12000),
  households = c(9300, 15100, 17400, 6200, 20300, 4100, 8800),
  income_pc = c(1830, 2800, 1500, 1970, 1400, NA, 1600)
)
data.table::setkeyv(fsm_toy_zone, "zone_id")
data.table::setattr(fsm_toy_zone, "class", c("fsm_zone", class(fsm_toy_zone)))

#' Toy origin-destination survey trips
#'
#' @description A deterministic disaggregate trip table aligned with the
#' positive-demand records in \code{fsm_toy_od}. Each row represents one trip
#' in an origin-destination survey. The toy contains a sample of approximately
#' 6,000 people making 14,290 trips, with an average of about 2.4 trips per
#' person. Consecutive trips made by the same
#' individual form a continuous origin-destination chain.
#' \code{expansion_factor} is the trip expansion factor and is one throughout,
#' so the records reproduce the
#' positive-demand OD flows. The sample is
#' approximately 54 percent female and has an approximately
#' 35 percent vehicle-ownership share. Per-capita income varies across
#' individuals, averages 1,800, and ranges from zero to 4,000. Each trip also
#' has an ordered activity pair, such as home-work or work-work, and a clock
#' time. The mode
#' column may contain an ordered multimodal chain separated by \code{+}, such as
#' walk-transit represented as \code{"walk+transit"}. Route or path
#' information is deliberately left for a later extension.
#'
#' @format A keyed \code{fsm_trip} object with 14,290 trip records and
#' 15 columns:
#' \describe{
#'   \item{trip_id}{Unique trip-record identifier.}
#'   \item{individual_id}{Identifier of the individual making the trip.}
#'   \item{expansion_factor}{Trip expansion factor, always one.}
#'   \item{age}{Age in completed years.}
#'   \item{gender}{Gender category: 54 percent female and 46 percent male.}
#'   \item{income_pc}{Varied per-capita income, including zero income.}
#'   \item{employed}{Whether the individual is employed.}
#'   \item{student}{Whether the individual is a student.}
#'   \item{vehicles}{Whether the individual has a vehicle.}
#'   \item{daily_trips}{Total number of trips made by the individual that day.}
#'   \item{origin}{Origin zone identifier.}
#'   \item{destination}{Destination zone identifier.}
#'   \item{mode}{Observed mode or ordered multimodal chain, with components
#'   separated by \code{+}.}
#'   \item{purpose_pair}{Ordered origin-destination activity pair. The first
#'   activity is performed at the origin and the second at the destination.}
#'   \item{time_of_day}{Trip time in 24-hour \code{HH:MM:SS} format, with
#'   seconds fixed at zero.}
#' }
#' @usage fsm_toy_trip
#' @name fsm_toy_trip
#' @docType data
#' @keywords datasets
#' @export
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See the classification of trips by ordered pairs of
#' activities at the origin and destination.
fsm_toy_trip <- local({
  positive_od <- fsm_toy_od[!is.na(demand) & demand > 0]
  observed_origin <- rep(
    positive_od[["origin"]],
    times = as.integer(positive_od[["demand"]])
  )
  observed_destination <- rep(
    positive_od[["destination"]],
    times = as.integer(positive_od[["demand"]])
  )
  n_trips <- length(observed_origin)

  # Add balancing arcs, then split Euler trails at those arcs. The resulting
  # observed arcs preserve the OD matrix while remaining continuous per person.
  nodes <- sort(unique(c(observed_origin, observed_destination)))
  outflow <- tabulate(match(observed_origin, nodes), nbins = length(nodes))
  inflow <- tabulate(match(observed_destination, nodes), nbins = length(nodes))
  imbalance <- outflow - inflow
  surplus <- rep(nodes[imbalance > 0L], times = imbalance[imbalance > 0L])
  deficit <- rep(nodes[imbalance < 0L], times = -imbalance[imbalance < 0L])
  virtual_origin <- deficit
  virtual_destination <- surplus

  edge_origin <- c(observed_origin, virtual_origin)
  edge_destination <- c(observed_destination, virtual_destination)
  is_virtual <- c(rep(FALSE, n_trips), rep(TRUE, length(virtual_origin)))
  outgoing <- split(seq_along(edge_origin), as.character(edge_origin))
  cursor <- stats::setNames(integer(length(outgoing)), names(outgoing))
  used <- rep(FALSE, length(edge_origin))
  circuits <- list()

  while (any(!used)) {
    start <- edge_origin[[which(!used)[[1L]]]]
    stack_nodes <- start
    stack_edges <- integer()
    circuit <- integer()

    while (length(stack_nodes)) {
      node <- stack_nodes[[length(stack_nodes)]]
      node_name <- as.character(node)
      candidates <- outgoing[[node_name]]
      position <- cursor[[node_name]] + 1L
      while (position <= length(candidates) && used[[candidates[[position]]]]) {
        position <- position + 1L
      }
      cursor[[node_name]] <- position

      if (position <= length(candidates)) {
        edge <- candidates[[position]]
        used[[edge]] <- TRUE
        stack_nodes <- c(stack_nodes, edge_destination[[edge]])
        stack_edges <- c(stack_edges, edge)
      } else {
        stack_nodes <- stack_nodes[-length(stack_nodes)]
        if (length(stack_edges)) {
          circuit <- c(circuit, stack_edges[[length(stack_edges)]])
          stack_edges <- stack_edges[-length(stack_edges)]
        }
      }
    }
    circuits[[length(circuits) + 1L]] <- rev(circuit)
  }

  trails <- list()
  for (circuit in circuits) {
    virtual_positions <- which(is_virtual[circuit])
    first_virtual <- if (length(virtual_positions)) virtual_positions[[1L]] else NA_integer_
    if (!is.na(first_virtual) && first_virtual < length(circuit)) {
      circuit <- c(circuit[(first_virtual + 1L):length(circuit)], circuit[seq_len(first_virtual)])
    }
    trail <- integer()
    for (edge in circuit) {
      if (is_virtual[[edge]]) {
        if (length(trail)) trails[[length(trails) + 1L]] <- trail
        trail <- integer()
      } else {
        trail <- c(trail, edge)
      }
    }
    if (length(trail)) trails[[length(trails) + 1L]] <- trail
  }

  # Join compatible one-leg fragments before splitting trails into daily chains.
  repeat {
    one_leg <- which(lengths(trails) == 1L)
    merged <- FALSE
    for (trail_index in one_leg) {
      fragment <- trails[[trail_index]]
      if (!length(fragment)) next

      starts <- vapply(trails, function(trail) {
        if (length(trail)) edge_origin[[trail[[1L]]]] else NA_integer_
      }, integer(1))
      ends <- vapply(trails, function(trail) {
        if (length(trail)) edge_destination[[trail[[length(trail)]]]] else NA_integer_
      }, integer(1))

      append_to <- setdiff(which(ends == edge_origin[[fragment[[1L]]]]), trail_index)
      prepend_to <- setdiff(which(starts == edge_destination[[fragment[[1L]]]]), trail_index)
      candidates <- c(append_to, prepend_to)
      if (!length(candidates)) next

      candidate <- candidates[[1L]]
      if (candidate %in% append_to) {
        trails[[candidate]] <- c(trails[[candidate]], fragment)
      } else {
        trails[[candidate]] <- c(fragment, trails[[candidate]])
      }
      trails[[trail_index]] <- integer()
      merged <- TRUE
    }
    trails <- Filter(length, trails)
    if (!merged) break
  }

  trail_lengths <- lengths(trails)
  minimum_chunks <- ifelse(trail_lengths == 1L, 1L, ceiling(trail_lengths / 3))
  maximum_chunks <- ifelse(trail_lengths == 1L, 1L, floor(trail_lengths / 2))
  target_people <- max(ceiling(n_trips / 2.5), sum(minimum_chunks))
  if (target_people > sum(maximum_chunks)) {
    stop("Toy trip chains cannot be divided to a feasible average length.", call. = FALSE)
  }
  n_chunks <- minimum_chunks
  remaining_chunks <- target_people - sum(n_chunks)
  for (trail_index in seq_along(trails)) {
    add <- min(remaining_chunks, maximum_chunks[[trail_index]] - n_chunks[[trail_index]])
    n_chunks[[trail_index]] <- n_chunks[[trail_index]] + add
    remaining_chunks <- remaining_chunks - add
  }

  chain_edges <- list()
  chain_sizes <- integer()
  for (trail_index in seq_along(trails)) {
    trail_length <- trail_lengths[[trail_index]]
    n_chain <- n_chunks[[trail_index]]
    sizes <- if (trail_length == 1L) {
      1L
    } else {
      c(
        rep.int(3L, trail_length - 2L * n_chain),
        rep.int(2L, 3L * n_chain - trail_length)
      )
    }
    position <- 0L
    for (size in sizes) {
      chain_edges[[length(chain_edges) + 1L]] <-
        trails[[trail_index]][position + seq_len(size)]
      chain_sizes <- c(chain_sizes, size)
      position <- position + size
    }
  }

  trip_edges <- unlist(chain_edges, use.names = FALSE)
  origin <- edge_origin[trip_edges]
  destination <- edge_destination[trip_edges]
  n_people <- length(chain_edges)
  person_id <- rep(seq_len(n_people), times = chain_sizes)
  trip_number <- unlist(lapply(chain_sizes, seq_len), use.names = FALSE)
  daily_trips <- rep(chain_sizes, times = chain_sizes)

  person_age <- 18L + as.integer((seq_len(n_people) * 7L) %% 65L)
  person_gender <- rep("male", n_people)
  gender_order <- order((seq_len(n_people) * 37L) %% n_people)
  person_gender[gender_order[seq_len(round(0.54 * n_people))]] <- "female"
  person_vehicles <- rep(FALSE, n_people)
  vehicle_order <- order((seq_len(n_people) * 53L) %% n_people)
  person_vehicles[vehicle_order[seq_len(round(0.35 * n_people))]] <- TRUE
  person_student <- person_age <= 25L
  person_employed <- person_age <= 67L & (seq_len(n_people) %% 5L != 0L)
  person_income <- ifelse(
    person_employed,
    1500L + as.integer((seq_len(n_people) * 37L) %% 1301L),
    400L + as.integer((seq_len(n_people) * 29L) %% 801L)
  )
  person_income[!person_employed & seq_len(n_people) %% 4L == 0L] <- 0L
  first_employed <- which(person_employed)[1L]
  person_income[first_employed] <- 4000L
  income_adjustment <- 1800L * n_people - sum(person_income)
  adjustable <- setdiff(which(person_employed), first_employed)
  person_income[adjustable] <- person_income[adjustable] +
    income_adjustment %/% length(adjustable)
  remainder <- income_adjustment %% length(adjustable)
  if (remainder > 0L) {
    person_income[adjustable[seq_len(remainder)]] <-
      person_income[adjustable[seq_len(remainder)]] + 1L
  }

  age <- person_age[person_id]
  income_pc <- person_income[person_id]
  gender <- person_gender[person_id]
  vehicles <- person_vehicles[person_id]
  employed <- person_employed[person_id]
  student <- person_student[person_id]
  primary_activity <- ifelse(student, "study", ifelse(
    employed,
    "work",
    ifelse(
      person_id %% 7L == 0L,
      "health",
      ifelse(person_id %% 3L == 0L, "shopping", "leisure")
    )
  ))
  intermediate_activity <- ifelse(
    person_id %% 4L == 0L,
    "shopping",
    ifelse(person_id %% 7L == 0L, "health", "leisure")
  )
  origin_activity <- ifelse(
    trip_number == 1L,
    "home",
    ifelse(trip_number == 2L, primary_activity, intermediate_activity)
  )
  destination_activity <- ifelse(
    daily_trips == 1L,
    primary_activity,
    ifelse(
      trip_number == daily_trips,
      "home",
      ifelse(trip_number == 1L, primary_activity, intermediate_activity)
    )
  )
  nonhome_based <- person_id %% 11L == 0L & trip_number == 1L
  origin_activity[nonhome_based] <- primary_activity[nonhome_based]
  purpose_pair <- paste(origin_activity, destination_activity, sep = "-")

  departure_hour <- ifelse(
    daily_trips == 1L,
    8L + person_id %% 8L,
    ifelse(
      trip_number == 1L,
      6L + person_id %% 4L,
      ifelse(trip_number == daily_trips, 16L + person_id %% 7L, 11L + person_id %% 5L)
    )
  )
  departure_minute <- (person_id * 15L + trip_number * 5L) %% 60L
  midnight <- person_id %% 97L == 0L & trip_number == daily_trips
  departure_hour[midnight] <- 0L
  departure_minute[midnight] <- 0L
  time_of_day <- sprintf("%02d:%02d:00", departure_hour, departure_minute)
  mode_cycle <- c(
    rep("car", 5L),
    rep("transit", 3L),
    rep("walk", 3L),
    "bicycle",
    rep("ridehailing", 2L),
    rep("motorcycle", 2L),
    rep("walk+transit", 2L),
    rep("walk+ridehailing", 2L)
  )

  expanded <- data.table::data.table(
    individual_id = person_id,
    expansion_factor = rep.int(1, n_trips),
    age = age,
    gender = gender,
    income_pc = income_pc,
    employed = employed,
    student = student,
    vehicles = vehicles,
    daily_trips = daily_trips,
    origin = origin,
    destination = destination,
    mode = rep(mode_cycle, length.out = n_trips),
    purpose_pair = purpose_pair,
    time_of_day = time_of_day
  )
  data.table::set(expanded, j = "trip_id", value = seq_len(nrow(expanded)))
  data.table::setcolorder(
    expanded,
    c(
      "trip_id", "origin", "destination",
      setdiff(names(expanded), c("trip_id", "origin", "destination"))
    )
  )
  expanded
})
data.table::setkeyv(fsm_toy_trip, "trip_id")
data.table::setattr(fsm_toy_trip, "class", c("fsm_trip", class(fsm_toy_trip)))
