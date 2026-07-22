# fsmr

##### Trip-based four-step transport demand modeling in R

[![Lifecycle: experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Development version: 0.1.0](https://img.shields.io/badge/dev%20version-0.1.0-0073B7.svg)](https://github.com/nelsonquesado/fsmr)
[![R](https://img.shields.io/badge/R-%3E%3D%204.1.0-276DC3.svg)](https://cran.r-project.org/)
![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)

`fsmr` provides objects and tools for trip-based four-step transport demand
modeling. Current coverage includes OD demand, zones, origin-destination
survey trips, trip generation, and matrix re-aggregation.

## Installation

```r
install.packages("remotes")
remotes::install_github("nelsonquesado/fsmr")
library(fsmr)
```

## Quick Start

```r
od <- fsm_toy_od
zones <- fsm_toy_zone
totals <- fsm_od_totals(od, zones)

fit <- fsm_generation(
  zones,
  totals,
  method = "regression",
  production_formula = production ~ 0 + population,
  attraction_formula = attraction ~ 0 + jobs
)

predict(fit)
confint(fit)
plot(od)
```

Toy objects: `fsm_toy_od`, `fsm_toy_zone`, and `fsm_toy_trip`.

## Guides

Use the **Data Objects**, **Trip Generation**, and **Matrix Re-Aggregation**
tabs on the package website.

## Citation

```r
citation("fsmr")
```

```bibtex
@Manual{fsmr2026,
  title = {fsmr: Trip-Based Four-Step Transport Demand Modeling in R},
  author = {Quesado Filho, Nelson de Oliveira and de Oliveira Neto, Francisco Moraes},
  year = {2026},
  note = {R package version 0.1.0},
  url = {https://nelsonquesado.github.io/fsmr/}
}
```

## Acknowledgement <img align="right" src="man/figures/logo_ufc.png" alt="Universidade Federal do Ceará" height="120"> <a href="https://det.ufc.br/petran"><img align="right" src="man/figures/opatp.png" alt="OPA-TP" width="120"></a>

**fsmr** is developed by [Nelson Quesado](https://github.com/nelsonquesado/)
and [Fco. Moraes](https://github.com/orgs/OPATP/people/OliveiraNetoFM) at the
OPA-TP research group, Universidade Federal do Ceará.
