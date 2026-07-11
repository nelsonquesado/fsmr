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

#' Toy population trip records
#'
#' @description A deterministic disaggregate trip table aligned with the
#' positive-demand records in \code{fsm_toy_od}. Its rows are unique
#' traveler-trip profiles and may represent multiple individuals only when all
#' recorded traveler and trip attributes are identical. The number represented
#' is stored in \code{population_count}. The toy represents 7,145 people making
#' 14,290 trips: individuals make one, two, or three trips, with an average of
#' two trips per person. The represented sample is approximately 54 percent female and has an
#' approximately 35 percent vehicle-ownership share. Per-capita income averages
#' 1,800 and ranges from zero to 4,000; unemployed people have lower incomes
#' and some have no income. Each trip also has an ordered activity
#' pair, such as home-work or work-work, and a clock time. The mode
#' column may contain an ordered multimodal chain separated by \code{+}, such as
#' walk-transit represented as \code{"walk+transit"}. Route or path
#' information is deliberately left for a later extension.
#'
#' @format A keyed \code{fsm_population} object with 13,599 unique
#' traveler-trip profiles and 15 columns:
#' \describe{
#'   \item{population_id}{Unique disaggregate-record identifier.}
#'   \item{population_count}{Number of identical individuals and trips represented.}
#'   \item{individual_id}{List-column containing the represented individual identifiers.}
#'   \item{age}{Age in completed years.}
#'   \item{gender}{Gender category: 54 percent female and 46 percent male.}
#'   \item{income_pc}{Per-capita income, including zero income.}
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
#' @usage fsm_toy_population
#' @name fsm_toy_population
#' @docType data
#' @keywords datasets
#' @export
#'
#' @references
#' Cascetta, E. (2009). \emph{Transportation Systems Analysis: Models and
#' Applications}. Springer. See the classification of trips by ordered pairs of
#' activities at the origin and destination.
fsm_toy_population <- local({
  positive_od <- fsm_toy_od[!is.na(demand) & demand > 0]
  origin <- rep(positive_od[["origin"]], times = as.integer(positive_od[["demand"]]))
  destination <- rep(
    positive_od[["destination"]],
    times = as.integer(positive_od[["demand"]])
  )
  n_trips <- length(origin)
  n_people <- n_trips %/% 2L
  one_or_three <- (n_people - 1L) %/% 4L
  person_daily_trips <- c(
    rep(1L, one_or_three),
    rep(2L, n_people - 2L * one_or_three),
    rep(3L, one_or_three)
  )
  person_id <- rep(seq_len(n_people), times = person_daily_trips)
  trip_number <- sequence(person_daily_trips)
  daily_trips <- person_daily_trips[person_id]

  person_age <- 18L + as.integer((seq_len(n_people) * 7L) %% 65L)
  person_gender <- rep("male", n_people)
  gender_order <- order((seq_len(n_people) * 37L) %% n_people)
  person_gender[gender_order[seq_len(round(0.54 * n_people))]] <- "female"
  person_vehicles <- rep(FALSE, n_people)
  vehicle_order <- order((seq_len(n_people) * 53L) %% n_people)
  person_vehicles[vehicle_order[seq_len(round(0.35 * n_people))]] <- TRUE
  person_employed <- person_age <= 67L & (seq_len(n_people) %% 5L != 0L)
  person_student <- person_age <= 25L
  person_income <- ifelse(
    person_employed,
    2500L,
    ifelse(seq_len(n_people) %% 4L == 0L, 0L, 900L)
  )
  first_employed <- which(person_employed)[1L]
  person_income[first_employed] <- 4000L
  income_adjustment <- 1800L * n_people - sum(person_income)
  adjustable <- setdiff(which(person_employed), first_employed)
  person_income[adjustable[seq_len(income_adjustment %/% 2L)]] <-
    person_income[adjustable[seq_len(income_adjustment %/% 2L)]] + 2L

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
  attributes <- setdiff(names(expanded), "individual_id")
  grouped <- expanded[, list(
    population_count = .N,
    individual_id = list(individual_id)
  ), by = attributes]
  data.table::set(grouped, j = "population_id", value = seq_len(nrow(grouped)))
  data.table::setcolorder(
    grouped,
    c("population_id", "population_count", "individual_id", attributes)
  )
  grouped
})
data.table::setkeyv(fsm_toy_population, "population_id")
data.table::setattr(fsm_toy_population, "class", c("fsm_population", class(fsm_toy_population)))
