
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN
status](https://www.r-pkg.org/badges/version/dymiumCore)](https://CRAN.R-project.org/package=dymiumCore)
[![Build
Status](https://travis-ci.org/dymium-org/dymiumCore.svg?branch=master)](https://travis-ci.org/dymium-org/dymiumCore)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dymium-org/dymiumCore?branch=master&svg=true)](https://ci.appveyor.com/project/dymium-org/dymiumCore)
[![Codecov test
coverage](https://codecov.io/gh/dymium-org/dymiumCore/branch/master/graph/badge.svg)](https://codecov.io/gh/dymium-org/dymiumCore?branch=master)
<!-- [![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/dymiumCore)](https://CRAN.R-project.org/package=dymiumCore) -->
<!-- badges: end -->

# dymiumCore

<img src="man/figures/dymium-banner.png" align="centre" />

**dymiumCore** is an R package which provides a toolbox for developing a
microsimulation model. While the core focus of the package is for
modelling urban systems, dymium can be easily extended to apply in other
contexts as well.

# Why `dymiumCore`?

  - written in R
  - easy to setup
  - ready-to-use events see
    [dymium-org/dymiumModules](https://github.com/dymium-org/dymiumModules)
  - microsimulation events are modular, sharable and scalable
  - equipped with the basic building blocks for building a
    microsimulation model that is flexible and extensible.
  - can use parameters from various model objects (e.g. `stats::lm`,
    `stats::glm`, `caret::train`).

## Installation

`dymiumCore` is not available on CRAN. You can install directly from
GitHub using `remotes`:

``` r
# install.packages("remotes")
remotes::install_github("dymium-org/dymiumCore")
```

## Documentation and Tutorials

For documentation and tutorials, please check our website at
<https://core.dymium.org>.

## Available modules

Please visit
[dymium-org/dymiumModules](https://github.com/dymium-org/dymiumModules)
to see the available modules.

## An example project

We have recently setup an example project so that you can see our
package in action\! Please visit
[dymium-org/dymiumExampleProject](https://github.com/dymium-org/dymiumExampleProject)
to learn more about it.

## Collaboration

We are open to collaboration on this project. If you are interested,
please email us at amarin at dymium.org.

## Development plan (as of 11th Jan 2020)

  - [x] **Version 0.1.0**: (21 Jan 2020) Releases all the basic building
    blocks for microsimulation modelling.
  - [ ] **Version 0.2.0**: Implements model-based calibration.
  - [ ] **Version 0.3.0**: Visualisation functions for life-courses,
    spatial data, etc.
  - [ ] **Version 0.4.0**: Integration with dymiumGUI.

# Similar frameworks

Urban Land-use

  - [UrbanSim](https://github.com/UDST/urbansim)
  - [SILO](https://github.com/msmobility/silo)
  - [ILUTE](https://github.com/TravelModellingGroup/ILUTE)

Demography

  - [MicSim](https://cran.r-project.org/web/packages/MicSim/index.html)
  - [neworder](https://github.com/virgesmith/neworder)
