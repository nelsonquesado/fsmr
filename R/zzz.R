#' Package Startup Message
#'
#' @param libname Package library path.
#' @param pkgname Package name.
#'
#' @return Nothing. Called for its side effect.
#' @noRd
.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname, lib.loc = libname)
  packageStartupMessage(
    pkgname, " ", version, ": trip-based four-step transport demand modeling in R"
  )
}

utils::globalVariables(c(
  "production",
  "attraction",
  "zone_id",
  "trip_id",
  "origin",
  "destination",
  "demand",
  ".fsm_production_exposure",
  ".fsm_attraction_exposure",
  "production_total",
  "attraction_total",
  "production_exposure",
  "attraction_exposure",
  "production_rate",
  "attraction_rate"
))
